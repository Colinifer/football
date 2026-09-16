import logging
import os
from datetime import date
from typing import Any
from urllib.parse import urlparse
from urllib.request import Request, urlopen

import matplotlib.pyplot as plt
import pandas as pd
from dotenv import find_dotenv, load_dotenv
from matplotlib.offsetbox import AnnotationBbox, OffsetImage
from PIL import Image
from sqlalchemy import create_engine, text

load_dotenv(find_dotenv())

DEFAULT_PLOT_DIR = "../../output/plots"

color_cw = {
    "dark_grey_1": "#1D2329",
    "dark_grey_2": "#2C343A",
    "dark_grey_3": "#38424B",
    "black": "#16191C",
    "white": "#e0e0e0",
    "green": "#1AB063",
    "red": "#E53935",
    "light_blue": "#00B8D9",
    "purple": "#651FFF",
    "pink": "#F50057",
    "yellow": "#FFD180",
}

logger = logging.getLogger(__name__)


def set_cw_dark_theme(
    palette: dict[str, str] = color_cw,
) -> None:
    """Sets the matplotlib style parameters to mimic the custom dark R theme.

    Args:
        palette: Dictionary mapping color names to hex codes.

    Returns:
        None
    """
    # plt.style.use("dark_background")  # A good starting point for a dark theme

    plt.rcParams.update(
        {
            # General
            "figure.facecolor": palette["black"],
            "axes.facecolor": palette["black"],
            "text.color": palette["white"],
            "font.family": "monospace",  # Assuming a monospaced font from the R script"s feel
            "font.size": 12,
            # Axes
            "axes.edgecolor": palette["dark_grey_2"],
            "axes.labelcolor": palette["white"],
            "xtick.color": palette["white"],
            "ytick.color": palette["white"],
            "axes.grid": True,
            "grid.color": palette["dark_grey_2"],
            "grid.linestyle": "--",
            "grid.alpha": 0.5,
            # Spines
            "axes.spines.top": False,
            "axes.spines.right": False,
            # Title
            "axes.titlecolor": palette["white"],
            "axes.titlelocation": "left",
            "axes.titlesize": "x-large",
            # Legend
            "legend.facecolor": palette["black"],
            "legend.edgecolor": palette["dark_grey_2"],
            "legend.labelcolor": palette["white"],
            # Line properties
            "lines.linewidth": 2,
        },
    )


# A conceptual Python function to replicate the R script"s functionality
def get_epa_team_tiers_data(
    db_connection_string: str,
    season: int,
) -> pd.DataFrame | None:
    """Connects to a database, retrieves NFL play-by-play data for a given season,
    and formats it for team tier analysis.

    This function fetches raw play-by-play data, filters it to include only
    passing and rushing plays, and then aggregates the data by offense and defense
    to calculate various EPA metrics per team per season.

    Args:
        db_connection_string: The connection string for the PostgreSQL database.
        season: The NFL season to analyze (e.g., 2023).

    Returns:
        A pandas DataFrame containing aggregated EPA stats for each team if the
        database connection and query are successful. Returns None otherwise.

    """
    try:
        query = text(
            """
            SELECT * 
            FROM "nflfastR_pbp" 
            WHERE season_type = 'REG' 
              AND season = :season 
              AND posteam IS NOT NULL;
            """,
        )
        engine = create_engine(db_connection_string)
        with engine.connect() as conn:
            pbp = pd.read_sql(query, conn, params={"season": season})
    except Exception as exc:
        raise RuntimeError("Error connecting to database or fetching data") from exc

    # 2. Data Preparation and Aggregation using pandas
    pbp_filtered = pbp[
        (pbp["posteam"].notna())
        & ((pbp["rush"].fillna(0) == 1) | (pbp["pass"].fillna(0) == 1))
    ]

    offense_stats = (
        pbp_filtered.groupby(["posteam", "season"])
        .agg(
            n_pass=("pass", "sum"),
            n_rush=("rush", "sum"),
            epa_per_pass=(
                "epa",
                lambda x: (
                    (x * pbp_filtered.loc[x.index, "pass"]).sum()
                    / pbp_filtered.loc[x.index, "pass"].sum()
                ),
            ),
            off_epa=("epa", "mean"),
        )
        .reset_index()
    )

    defense_stats = (
        pbp_filtered.groupby(["defteam", "season"])
        .agg(def_epa=("epa", "mean"))
        .reset_index()
    )

    # Join offense and defense data
    chart_data = offense_stats.merge(
        defense_stats,
        left_on=["posteam", "season"],
        right_on=["defteam", "season"],
    )

    return chart_data


def plot_epa_team_tiers(
    chart_data: pd.DataFrame,
    season: int,
    output_dir: str,
) -> None:
    """Generate and save a scatter plot of NFL team tiers based on offensive and
    defensive EPA metrics.

    The plot visualizes each team's performance by placing their logo on a grid
    defined by their offensive and defensive EPA per play. Teams are plotted with
    their logos at coordinates corresponding to their offensive EPA (x-axis) and
    defensive EPA (y-axis). League average lines are included as red dashed lines
    for reference.

    Args:
        chart_data: A pandas DataFrame containing the prepared data for plotting.
            Required columns:
            - 'off_epa' (float): Offensive EPA per play
            - 'def_epa' (float): Defensive EPA per play
            - 'posteam' (str): Team abbreviation (e.g., 'KC', 'SF')
        season: The NFL season year corresponding to the data (e.g., 2023).
        output_dir: The base directory where the plot image should be saved.
            The plot will be saved to {output_dir}/team_tiers/team_tiers_{season}.png

    Returns:
        None. Saves the plot as a PNG file to the specified output directory.

    Raises:
        KeyError: If required columns are missing from chart_data.
        OSError: If the output directory cannot be created or written to.

    Example:
        >>> data = pd.DataFrame({
        ...     'off_epa': [0.1, -0.05, 0.15],
        ...     'def_epa': [-0.08, 0.02, -0.12],
        ...     'posteam': ['KC', 'NYJ', 'SF']
        ... })
        >>> plot_epa_team_tiers(data, 2023, './output')

    """
    # plt.style.use("./cw_dark.mplstyle")
    set_cw_dark_theme()

    # Ensure the output directory exists
    os.makedirs(os.path.join(output_dir, "team_tiers"), exist_ok=True)

    fig, ax = plt.subplots(figsize=(10, 8))

    fig.set_facecolor(color_cw["black"])

    x = chart_data["off_epa"]
    y = chart_data["def_epa"]
    teams = chart_data["posteam"]

    def get_image(path: str) -> OffsetImage | None:
        """Retrieves and returns an OffsetImage from a given URL.

        Args:
            path: The URL string of the image.

        Returns:
            An OffsetImage instance if successful, or None otherwise.
        """
        try:
            parsed_url = urlparse(path)
            if parsed_url.scheme not in ("http", "https"):
                raise ValueError("URL must use http or https scheme")
            req = Request(path, headers={"User-Agent": "Mozilla/5.0"})  # noqa: S310
            with urlopen(req) as response:  # noqa: S310
                image = Image.open(response)
                return OffsetImage(image, zoom=0.4)
        except (ValueError, OSError) as e:
            logger.error("Error loading image: %s", e)
            return None

    for x0, y0, team in zip(x, y, teams, strict=True):
        logo_path = f"https://a.espncdn.com/combiner/i?img=/i/teamlogos/nfl/500/{team}.png&h=100&w=100"
        ab = AnnotationBbox(
            get_image(logo_path),
            (x0, y0),
            frameon=False,
        )
        ax.add_artist(ab)

    ax.set_xlabel("Offense EPA/play")
    ax.set_ylabel("Defense EPA/play")
    ax.set_title(f"{season} NFL Team Tiers", size=24)

    # Calculate league averages
    offense_epa_avg = chart_data["off_epa"].mean()
    defense_epa_avg = chart_data["def_epa"].mean()

    # Plot league average lines
    ax.axvline(
        offense_epa_avg,
        color="red",
        linestyle="--",
        alpha=0.7,
        label=f"League Avg Offense EPA: {offense_epa_avg:.2f}",
    )
    ax.axhline(
        defense_epa_avg,
        color="red",
        linestyle="--",
        alpha=0.7,
        label=f"League Avg Defense EPA: {defense_epa_avg:.2f}",
    )

    x_min, x_max = x.min() - 0.1, x.max() + 0.1  # Add/subtract a small margin
    y_min, y_max = y.min() - 0.1, y.max() + 0.1

    ax.set_xlim(x_min, x_max)
    ax.set_ylim(y_max, y_min)

    ax.grid(True, linestyle="--", alpha=0.5)

    ax.legend()
    ax.get_legend().get_frame().set_boxstyle("square")

    filename = f"team_tiers_{season}.png"
    full_save_path = os.path.join(output_dir, "team_tiers", filename)

    print(f"Saving Figure to: {full_save_path}")
    plt.savefig(
        full_save_path,
        facecolor=fig.get_facecolor(),
        dpi=300,
    )


def get_year(x: date | None = None) -> int:
    """Return the NFL season year based on the given date.

    Args:
        x: Optional reference date. Defaults to today.

    Returns:
        The corresponding NFL season year as an integer.
    """
    if x is None:
        x = date.today()
    month_threshold = 8
    if x.month > month_threshold:
        return x.year
    return x.year - 1


def build_epa_plots(
    db_connection_string: str,
    season: int | str | None = None,
    **kwargs: Any,
) -> None:
    """A wrapper function to get data and build all EPA-based plots.

    This function is designed to be called by a PythonOperator in an Airflow DAG
    or via CLI command arguments.

    Args:
        db_connection_string: The connection string for the PostgreSQL database.
        season: The NFL season year or string. Defaults to None.
        **kwargs: Additional keyword arguments including output_dir.

    Returns:
        None
    """
    import argparse

    if season is None:
        parser = argparse.ArgumentParser(description="Build EPA team tiers plot.")
        parser.add_argument(
            "--season",
            type=str,
            default=None,
            help="NFL season year",
        )
        parsed_args, _ = parser.parse_known_args()
        if parsed_args.season is not None:
            season = parsed_args.season

    if season is None or str(season).lower() == "none":
        season = kwargs.get("season")
        if str(season).lower() == "none":
            season = None

    season = get_year() if season is None else int(season)
    print(f"Using season: {season}")

    db_conn = db_connection_string

    default_output_dir = os.path.abspath(
        os.path.join(os.path.dirname(__file__), DEFAULT_PLOT_DIR),
    )
    output_dir = kwargs.get("output_dir", default_output_dir)

    if not os.path.exists(output_dir):
        os.makedirs(output_dir, exist_ok=True)

    ## Ensure all required arguments are present
    if not all([db_conn, output_dir]):
        raise ValueError(
            "Missing required arguments: db_connection_string or output_dir.",
        )

    chart_data = get_epa_team_tiers_data(
        db_connection_string=db_conn,
        season=season,
    )

    # Check if data retrieval was successful
    if chart_data is not None:
        # 2. Pass the retrieved data and other arguments to the plotting function
        plot_epa_team_tiers(
            chart_data=chart_data,
            season=season,
            output_dir=output_dir,
        )


if __name__ == "__main__":
    from helpers.db_utils import build_connection

    db_conn_str = build_connection()
    build_epa_plots(db_connection_string=db_conn_str)
