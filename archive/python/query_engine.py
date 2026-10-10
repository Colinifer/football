# %%
import atexit
import os
import socket
import subprocess
import time
from typing import Any

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import seaborn as sns
from dotenv import load_dotenv
from sqlalchemy import (
    MetaData,
    Table,
    and_,
    create_engine,
    func,
    or_,
    select,
    text,
)
from sqlalchemy.engine import Engine

# %%

load_dotenv()


class QueryEngine:
    """PostgreSQL query engine for safe query execution.

    Safely builds and executes queries using SQLAlchemy Core.
    """

    VALID_AGGREGATIONS = {
        "sum": func.sum,
        "count": func.count,
        "avg": func.avg,
        "min": func.min,
        "max": func.max,
        "array_agg": func.array_agg,
        "string_agg": func.string_agg,
    }

    def __init__(
        self,
        conn_params: dict[str, Any],
        ssh_params: dict[str, Any] | None = None,
    ):
        """Initialize the query engine with a database connection.

        Args:
        ----
            conn_params: dict of postgres connection parameters
            ssh_params: dict containing ssh profile or other ssh configuration

        """
        self.tunnel_proc = None
        if ssh_params and ssh_params.get("profile"):
            atexit.register(self.close)
            self.tunnel_proc = subprocess.Popen(
                [
                    "ssh",
                    "-o",
                    "ExitOnForwardFailure=yes",
                    "-o",
                    "BatchMode=yes",
                    "-L",
                    f"{conn_params['port']}:{conn_params['host']}:{conn_params['port']}",
                    f"{ssh_params['profile']}",
                    "-N",
                ],
                stdout=subprocess.DEVNULL,
                stderr=subprocess.PIPE,
            )

            port = int(conn_params["port"])
            for _ in range(50):
                with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as sock:
                    sock.settimeout(0.1)
                    if sock.connect_ex(("127.0.0.1", port)) == 0:
                        break
                time.sleep(0.1)

        # Create SQLAlchemy engine
        user = conn_params.get("user")
        password = conn_params.get("password")
        host = "127.0.0.1" if self.tunnel_proc else conn_params.get("host")
        port = conn_params.get("port")
        dbname = conn_params.get("dbname")

        db_url = f"postgresql://{user}:{password}@{host}:{port}/{dbname}"
        self.engine: Engine = create_engine(db_url)
        self.metadata = MetaData()

    def close(self):
        """Close connection and terminate tunnel process."""
        if hasattr(self, "engine") and self.engine:
            self.engine.dispose()
            self.engine = None

        if (
            hasattr(self, "tunnel_proc")
            and self.tunnel_proc
            and self.tunnel_proc.poll() is None
        ):
            self.tunnel_proc.terminate()
            try:
                self.tunnel_proc.wait(timeout=2)
            except subprocess.TimeoutExpired:
                self.tunnel_proc.kill()
            self.tunnel_proc = None

    def __enter__(self):
        """Context manager entry."""
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        """Context manager exit - ensures connection is closed."""
        self.close()

    def execute_query(self, query_config: dict[str, Any]) -> list[dict[str, Any]]:
        """Execute a query based on the configuration dictionary.

        Args:
        ----
            query_config: Dictionary containing query parameters (see docstring below)

        Returns:
        -------
            List of dictionaries containing the query results

        """
        table_name = query_config.get("table")
        if not table_name:
            raise ValueError("Table name is required")

        # Reflect table schema
        table = Table(table_name, self.metadata, autoload_with=self.engine)

        # Build SELECT statement
        stmt = self._build_statement(table, query_config)

        # Execute and fetch results
        with self.engine.connect() as conn:
            result = conn.execute(stmt)
            return [dict(row) for row in result.mappings()]

    def _build_statement(self, table: Table, config: dict[str, Any]):
        """Build the SQLAlchemy select statement."""
        columns_config = config.get("columns", ["*"])
        select_cols = []

        if "*" in columns_config or not columns_config:
            select_cols = [table]
        else:
            for col_spec in columns_config:
                select_cols.append(self._get_column_expression(table, col_spec))

        stmt = select(*select_cols)

        if config.get("distinct"):
            stmt = stmt.distinct()

        # WHERE clause
        filters = config.get("filters")
        if filters:
            stmt = stmt.where(self._build_filter_expression(table, filters))

        # GROUP BY
        group_by = config.get("group_by")
        if group_by:
            group_cols = [table.c[col] for col in group_by]
            stmt = stmt.group_by(*group_cols)

        # HAVING
        having = config.get("having")
        if having:
            stmt = stmt.having(self._build_filter_expression(table, having))

        # ORDER BY
        order_by = config.get("order_by")
        if order_by:
            order_exprs = []
            for order in order_by:
                if isinstance(order, str):
                    order_exprs.append(table.c[order].asc())
                elif isinstance(order, dict):
                    col = table.c[order["column"]]
                    if order.get("direction", "asc").lower() == "desc":
                        order_exprs.append(col.desc())
                    else:
                        order_exprs.append(col.asc())
            stmt = stmt.order_by(*order_exprs)

        # LIMIT and OFFSET
        limit = config.get("limit")
        if limit is not None:
            stmt = stmt.limit(limit)

        offset = config.get("offset")
        if offset is not None:
            stmt = stmt.offset(offset)

        return stmt

    def _get_column_expression(self, table: Table, col_spec: str | dict[str, Any]):
        """Get SQLAlchemy column expression from spec."""
        if isinstance(col_spec, str):
            return table.c[col_spec]

        col_name = col_spec.get("column")
        agg = col_spec.get("agg")
        alias = col_spec.get("alias")

        if col_name == "*" and not agg:
            return table

        base_col = table.c[col_name] if col_name != "*" else text("*")

        if agg:
            agg_func = self.VALID_AGGREGATIONS.get(agg.lower())
            if not agg_func:
                raise ValueError(f"Invalid aggregation: {agg}")

            if agg.lower() == "string_agg":
                expr = agg_func(base_col, col_spec.get("delimiter", ", "))
            else:
                expr = agg_func(base_col)
        else:
            expr = base_col

        if alias:
            expr = expr.label(alias)

        return expr

    def _build_filter_expression(self, table: Table, filter_config: dict[str, Any]):
        """Build SQLAlchemy filter expression from config."""
        logic = filter_config.get("logic", "and").lower()
        conditions = filter_config.get("conditions", [])

        exprs = []
        for cond in conditions:
            if "logic" in cond:
                exprs.append(self._build_filter_expression(table, cond))
            else:
                col = table.c[cond["column"]]
                op = cond.get("operator", "=").lower()
                val = cond.get("value")

                if op == "=":
                    exprs.append(col == val)
                elif op in ("!=", "<>"):
                    exprs.append(col != val)
                elif op == ">":
                    exprs.append(col > val)
                elif op == "<":
                    exprs.append(col < val)
                elif op == ">=":
                    exprs.append(col >= val)
                elif op == "<=":
                    exprs.append(col <= val)
                elif op == "like":
                    exprs.append(col.like(val))
                elif op == "ilike":
                    exprs.append(col.ilike(val))
                elif op == "in":
                    exprs.append(col.in_(val))
                elif op == "not in":
                    exprs.append(col.not_in(val))
                elif op == "is":
                    exprs.append(col.is_(val))
                elif op == "is not":
                    exprs.append(col.is_not(val))
                else:
                    raise ValueError(f"Unsupported operator: {op}")

        if not exprs:
            return None

        return and_(*exprs) if logic == "and" else or_(*exprs)


def plot_density(
    data: pd.DataFrame,
    x_col: str,
    group_col: str | None = None,
    ax: plt.Axes | None = None,
) -> plt.Axes | np.ndarray:
    """Generate a 1D density plot for a specified variable.

    If a 'group_col' is provided, this function creates a vertical stack of
    density plots, with one subplot for each unique value in the group column.
    Each subplot has its own y-axis scale to normalize plot height.

    If no 'group_col' is provided, it generates a single density plot on
    the given Axes object or creates a new one.

    Args:
    ----
        data (pd.DataFrame): The pandas DataFrame containing the data to plot.
        x_col (str): The name of the column to be plotted on the x-axis.
        group_col (Optional[str, optional): The name of the column to create
            subplots for. A separate plot will be drawn for each group.
            Defaults to None.
        ax (Optional[plt.Axes], optional): An existing matplotlib Axes object
            to plot on. This parameter is ignored if 'group_col' is specified.
            Defaults to None.

    Returns:
    -------
        Union[plt.Axes, np.ndarray]: The matplotlib Axes object for a single
            plot, or a numpy array of Axes objects for grouped plots.

    """
    if group_col:
        # Create a vertical subplot for each group, preserving dataframe order
        groups = data[group_col].dropna().unique()
        n_groups = len(groups)

        if n_groups == 0:
            print("No groups to plot.")
            return None

        # Create a figure with n_groups vertical subplots that share an x-axis
        fig, axes = plt.subplots(
            n_groups,
            1,
            figsize=(8, 3 * n_groups),
            sharex=True,
            sharey=True,
        )

        # Ensure 'axes' is always an array, even for a single group
        if n_groups == 1:
            axes = np.array([axes])

        for i, group in enumerate(groups):
            ax_current = axes[i]
            group_data = data[data[group_col] == group]

            sns.kdeplot(data=group_data, x=x_col, ax=ax_current, fill=True)
            ax_current.set_title(f"{group}")
            ax_current.set_ylabel("Density")
            ax_current.set_xlabel("")  # Remove x-label from all but the last plot

        # Set a common title and the final x-label
        axes[-1].set_xlabel(x_col)
        fig.suptitle(f"Density of {x_col} by {group_col}", fontsize=16, y=1.0)
        fig.tight_layout(rect=[0, 0, 1, 0.97])

        return axes

    else:
        # Original behavior: plot a single density plot
        if ax is None:
            fig, ax = plt.subplots(figsize=(10, 8))

        sns.kdeplot(data=data, x=x_col, ax=ax, fill=True)

        ax.set_title(f"Density Plot of {x_col}")
        ax.set_xlabel(x_col)
        ax.set_ylabel("Density")

        return ax


# %%
SSH_PARAMS = {
    "profile": "moose",
}

DB_CONNECTION_PARAMS = {
    "dbname": os.getenv("DB_NAME"),
    "user": os.getenv("DB_USER"),
    "password": os.getenv("DB_PASS"),
    "host": os.getenv("DB_HOST_REMOTE"),
    "port": os.getenv("DB_PORT_REMOTE"),
}

# DB_CONNECTION_PARAMS = {
#     "dbname": os.getenv("DB_NAME"),
#     "user": os.getenv("DB_USER"),
#     "password": os.getenv("DB_PASS"),
#     "host": os.getenv("DB_HOST_LOCAL"),
#     "port": os.getenv("DB_PORT_LOCAL"),
# }

# %%
with QueryEngine(DB_CONNECTION_PARAMS, SSH_PARAMS) as engine:
    # Example 1: Basic query with aggregations
    query_config = {
        "table": "nflfastR_pbp",
        "columns": [
            "game_id",
            "week",
            "receiver_player_id",
            "receiver_player_name",
            {
                "column": "yards_gained",
                "agg": "sum",
                "alias": "receiving_yards",
            },
            {
                "column": "pass_attempt",
                "agg": "sum",
                "alias": "targets",
            },
        ],
        "filters": {
            "logic": "and",
            "conditions": [
                {
                    "column": "receiver_player_id",
                    "operator": "IS NOT",
                },
                {
                    "column": "season",
                    "operator": "=",
                    "value": 2025,
                },
                {
                    "column": "posteam",
                    "operator": "=",
                    "value": "TB",
                },
            ],
        },
        "group_by": [
            "game_id",
            "week",
            "receiver_player_id",
            "receiver_player_name",
        ],
        # "order_by": [{"column": "yards_gained", "direction": "desc"}],
        # "limit": 10
    }

    results = engine.execute_query(query_config)

# %%
df = pd.DataFrame(results)

receivers = [
    "D.Goedert",
    "S.Barkley",
    "D.Smith",
    "J.Dotson",
]

receivers = (
    df.groupby("receiver_player_name")["receiving_yards"]
    .sum()
    .nlargest(5)
    .reset_index()["receiver_player_name"]
)

print(receivers)

# Filter and sort by receivers list order
filtered_df = df[df["receiver_player_name"].isin(receivers)].copy()
filtered_df["receiver_player_name"] = pd.Categorical(
    filtered_df["receiver_player_name"],
    categories=receivers.tolist(),
    ordered=True,
)
filtered_df = filtered_df.sort_values("receiver_player_name")

plot_density(
    data=filtered_df,
    x_col="targets",
    # y_col="targets",
    group_col="receiver_player_name",
)

# %%
