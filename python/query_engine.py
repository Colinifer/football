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
import psycopg2
import seaborn as sns
from dotenv import load_dotenv
from psycopg2 import sql

# %%

load_dotenv()


class QueryEngine:
    """PostgreSQL query engine for safe query execution.

    Safely builds and executes queries using parameterized statements to prevent SQL injection.
    """

    VALID_AGGREGATIONS = {
        "sum",
        "count",
        "avg",
        "min",
        "max",
        "array_agg",
        "string_agg",
    }
    VALID_OPERATORS = {
        "=",
        "!=",
        "<>",
        ">",
        "<",
        ">=",
        "<=",
        "like",
        "ilike",
        "in",
        "not in",
        "is",
        "is not",
    }
    VALID_LOGICAL = {"and", "or"}

    def __init__(self, conn_params, ssh_params=None):
        """Initialize the query engine with a database connection.

        Args:
        ----
            conn_params: dict of postgres connection parameters
            ssh_config: dict containing ssh_host, ssh_username, ssh_pkey (path),
                        and remote_bind_address (db_host, db_port)

        """
        self.tunnel_proc = None
        # if ssh_params["host"] and ssh_params{"host"}:
        if ssh_params and ssh_params.get("profile"):
            # Register cleanup immediately so it runs
            # even if __init__ fails later
            atexit.register(self.close)
            self.tunnel_proc = subprocess.Popen(
                [
                    "ssh",
                    "-o",
                    "ExitOnForwardFailure=yes",
                    "-o",
                    "BatchMode=yes",
                    "-L",
                    f"{conn_params["port"]}:{conn_params["host"]}:{conn_params["port"]}",
                    f"{ssh_params["profile"]}",
                    "-N",
                ],
                stdout=subprocess.DEVNULL,
                stderr=subprocess.PIPE,
            )
            # time.sleep(2)  # Give the tunnel a moment to open
            # Wait for the port to become available (ready-check)
            port = int(conn_params["port"])
            for _ in range(50):  # Try for up to 5 seconds
                with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as sock:
                    sock.settimeout(0.1)
                    if sock.connect_ex(("127.0.0.1", port)) == 0:
                        break
                time.sleep(0.1)

        self.conn = psycopg2.connect(**conn_params)

    def close(self):
        """Close connection and terminate tunnel process."""
        if hasattr(self, "conn") and self.conn:
            try:
                self.conn.close()
            except Exception:
                pass
            self.conn = None

        if self.tunnel_proc and self.tunnel_proc.poll() is None:
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
            query_config: Dictionary containing query parameters:
                - table (str): Table name (required)
                - columns (list): List of column definitions (required)
                    Each can be a string (column name) or dict:
                    {"column": "col_name", "agg": "sum", "alias": "total"}
                - filters (list): List of filter conditions (optional)
                    Each filter is a dict: {"column": "name", "operator": "=", "value": 123}
                    or {"logic": "and/or", "conditions": [...nested filters...]}
                - group_by (list): List of column names to group by (optional)
                - order_by (list): List of ordering definitions (optional)
                    Each can be a string or dict: {"column": "name", "direction": "asc/desc"}
                - limit (int): Maximum number of rows to return (optional)
                - offset (int): Number of rows to skip (optional)
                - distinct (bool): Use SELECT DISTINCT (optional, default False)

        Returns:
        -------
            List of dictionaries containing the query results

        Example:
        -------
            query_config = {
                "table": "sales",
                "columns": [
                    "product_id",
                    {"column": "amount", "agg": "sum", "alias": "total_amount"},
                    {"column": "*", "agg": "count", "alias": "num_sales"}
                ],
                "filters": [
                    {"column": "status", "operator": "=", "value": "completed"},
                    {"column": "amount", "operator": ">", "value": 100}
                ],
                "group_by": ["product_id"],
                "order_by": [{"column": "total_amount", "direction": "desc"}],
                "limit": 10
            }

        """
        table = query_config.get("table")
        if not table:
            raise ValueError("Table name is required")

        columns = query_config.get("columns", ["*"])
        filters = query_config.get("filters", [])
        group_by = query_config.get("group_by", [])
        having = query_config.get("having")
        order_by = query_config.get("order_by", [])
        limit = query_config.get("limit")
        offset = query_config.get("offset")
        distinct = query_config.get("distinct", False)

        # Build the query
        query, params = self._build_query(
            table=table,
            columns=columns,
            filters=filters,
            group_by=group_by,
            having=having,
            order_by=order_by,
            limit=limit,
            offset=offset,
            distinct=distinct,
        )

        # Execute and fetch results
        with self.conn.cursor() as cursor:
            cursor.execute(query, params)
            column_names = [desc[0] for desc in cursor.description]
            results = cursor.fetchall()

            # Convert to list of dictionaries
            return [dict(zip(column_names, row, strict=False)) for row in results]

    def _build_query(
        self,
        table: str,
        columns: list[str | dict],
        filters: dict,
        group_by: list[str],
        order_by: list[str | dict],
        having: dict | None,
        limit: int | None,
        offset: int | None,
        distinct: bool,
    ) -> tuple:
        """Build the SQL query and parameters."""
        params = []

        # Build SELECT clause
        select_parts = []
        distinct_keyword = "DISTINCT " if distinct else ""

        for col in columns:
            select_parts.append(self._build_column_expression(col))

        select_clause = sql.SQL("SELECT {}{} FROM {}").format(
            sql.SQL(distinct_keyword),
            sql.SQL(", ").join(select_parts),
            sql.Identifier(table),
        )

        # Build WHERE clause
        where_clause = sql.SQL("")
        if filters:
            where_expr, where_params = self._build_where_clause(filters)
            if where_expr:
                where_clause = sql.SQL(" WHERE {}").format(where_expr)
                params.extend(where_params)

        # Build GROUP BY clause
        group_clause = sql.SQL("")
        if group_by:
            group_identifiers = [sql.Identifier(col) for col in group_by]
            group_clause = sql.SQL(" GROUP BY {}").format(
                sql.SQL(", ").join(group_identifiers),
            )

        # Build HAVING clause
        having_clause = sql.SQL("")
        if having:
            having_expr, having_params = self._build_conditions_clause(having)
            if having_expr:
                having_clause = sql.SQL(" HAVING {}").format(having_expr)
                params.extend(having_params)

        # Build ORDER BY clause
        order_clause = sql.SQL("")
        if order_by:
            order_parts = []
            for order in order_by:
                if isinstance(order, str):
                    order_parts.append(sql.Identifier(order))
                elif isinstance(order, dict):
                    col_name = order["column"]
                    direction = order.get("direction", "asc").upper()
                    if direction not in ("ASC", "DESC"):
                        raise ValueError(f"Invalid order direction: {direction}")
                    order_parts.append(
                        sql.SQL("{} {}").format(
                            sql.Identifier(col_name),
                            sql.SQL(direction),
                        ),
                    )
            order_clause = sql.SQL(" ORDER BY {}").format(
                sql.SQL(", ").join(order_parts),
            )

        # Build LIMIT and OFFSET clauses
        limit_clause = sql.SQL("")
        if limit is not None:
            limit_clause = sql.SQL(" LIMIT %s")
            params.append(limit)

        offset_clause = sql.SQL("")
        if offset is not None:
            offset_clause = sql.SQL(" OFFSET %s")
            params.append(offset)

        # Combine all parts
        query = sql.SQL("{}{}{}{}{}{}").format(
            select_clause,
            where_clause,
            group_clause,
            having_clause,
            order_clause,
            limit_clause,
            offset_clause,
        )

        return query, params

    def _build_column_expression(self, col: str | dict) -> sql.Composable:
        """Build a column expression, potentially with aggregation."""
        if isinstance(col, str):
            if col == "*":
                return sql.SQL("*")
            return sql.Identifier(col)

        elif isinstance(col, dict):
            col_name = col.get("column", "*")
            agg = col.get("agg", "").lower()
            alias = col.get("alias")

            if agg:
                if agg not in self.VALID_AGGREGATIONS:
                    raise ValueError(f"Invalid aggregation function: {agg}")

                # Handle special cases for aggregations
                if col_name == "*":
                    col_expr = sql.SQL("*")
                else:
                    col_expr = sql.Identifier(col_name)

                # Special handling for string_agg which requires a delimiter
                if agg == "string_agg":
                    delimiter = col.get("delimiter", ", ")
                    # Use sql.Literal to safely inject the delimiter string
                    agg_expr = sql.SQL("STRING_AGG({}, {})").format(
                        col_expr,
                        sql.Literal(delimiter),
                    )
                else:
                    agg_expr = sql.SQL("{}({})").format(sql.SQL(agg.upper()), col_expr)

                if alias:
                    return sql.SQL("{} AS {}").format(agg_expr, sql.Identifier(alias))
                return agg_expr

            else:
                col_expr = sql.Identifier(col_name)
                if alias:
                    return sql.SQL("{} AS {}").format(col_expr, sql.Identifier(alias))
                return col_expr

        raise ValueError(f"Invalid column specification: {col}")

    def _build_where_clause(self, filter_group: dict) -> tuple:
        """Build WHERE clause with parameterized values from a filter group."""
        if not filter_group or not filter_group.get("conditions"):
            return sql.SQL(""), []

        logic_op = filter_group.get("logic", "and").upper()
        if logic_op not in ("AND", "OR"):
            raise ValueError(f"Invalid logical operator: {logic_op}")

        conditions = []
        params = []

        for filter_item in filter_group.get("conditions", []):
            # Handle nested logical conditions (AND/OR groups)
            if "logic" in filter_item:
                nested_expr, nested_params = self._build_where_clause(filter_item)
                if nested_expr:
                    conditions.append(sql.SQL("({})").format(nested_expr))
                    params.extend(nested_params)

            # Handle simple conditions
            elif "column" in filter_item:
                col = filter_item["column"]
                op = filter_item.get("operator", "=").lower()
                value = filter_item.get("value")

                if op not in self.VALID_OPERATORS:
                    raise ValueError(f"Invalid operator: {op}")

                # Handle NULL checks
                if op in ("is", "is not"):
                    if value is None or (
                        isinstance(value, str) and value.lower() == "null"
                    ):
                        conditions.append(
                            sql.SQL("{} {} NULL").format(
                                sql.Identifier(col),
                                sql.SQL(op.upper()),
                            ),
                        )
                    else:
                        raise ValueError("IS/IS NOT operators require NULL value")

                # Handle IN/NOT IN with lists
                elif op in ("in", "not in"):
                    if not isinstance(value, (list, tuple)):
                        raise ValueError(f"{op.upper()} operator requires a list value")
                    placeholders = sql.SQL(", ").join([sql.Placeholder()] * len(value))
                    conditions.append(
                        sql.SQL("{} {} ({})").format(
                            sql.Identifier(col),
                            sql.SQL(op.upper()),
                            placeholders,
                        ),
                    )
                    params.extend(value)

                # Handle standard operators
                else:
                    conditions.append(
                        sql.SQL("{} {} {}").format(
                            sql.Identifier(col),
                            sql.SQL(op.upper()),
                            sql.Placeholder(),
                        ),
                    )
                    params.append(value)

        # Combine conditions with AND by default
        where_expression = sql.SQL(" AND ").join(conditions)

        return where_expression, params


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
        # Create a vertical subplot for each group
        groups = sorted(data[group_col].dropna().unique())
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

DB_CONNECTION_PARAMS = {
    "dbname": os.getenv("DB_NAME"),
    "user": os.getenv("DB_USER"),
    "password": os.getenv("DB_PASS"),
    "host": os.getenv("DB_HOST_LOCAL"),
    "port": os.getenv("DB_PORT_LOCAL"),
}

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
                    "value": "PHI",
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

# %%
df = pd.DataFrame(results)

receivers = [
    "D.Goedert",
    "S.Barkley",
    "D.Smith",
    "J.Dotson",
]

# %%
plot_density(
    data=df[df["receiver_player_name"].isin(receivers)],
    x_col="receiving_yards",
    # y_col="targets",
    group_col="receiver_player_name",
)
