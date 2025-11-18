import pandas as pd
import numpy as np

import psycopg2

import matplotlib as mpl
import matplotlib.pyplot as plt
import matplotlib.style as style
import matplotlib.font_manager as fm
from matplotlib.offsetbox import OffsetImage, AnnotationBbox
import matplotlib.image as mpimg
import matplotlib.transforms as transforms
import seaborn as sns

import requests
from PIL import Image
from io import BytesIO


def get_team_color(
    team: str,
    conn: psycopg2.extensions.connection,
) -> None:
    """
    Retrieves a team logo from the database based on the team and source.

    Args:
        team (str): The team abbreviation.
        source (str, optional): The source of the logo. Defaults to "espn".
        conn (psycopg2.extensions.connection): The database connection.

    Returns:
        str: The team color value.
    """
    query = f"""
        select team_color
        from "nflfastR_logos"
        where team_abbr = '{team.upper()}'
    """

    # print(f"Team: {team}")
    # print(query)

    df = pd.read_sql_query(
        sql=query,
        con=conn,
    )
    color = df[f"team_color"][0]

    return color


# Example:
# get_team_color("phi")


def get_image_from_url(
    url: str,
) -> Image:
    """
    Retrieves an image from a given URL.

    Args:
        url (str): The URL of the image.

    Returns:
        PIL.Image: The retrieved image object.
    """
    response = requests.get(url)
    return Image.open(BytesIO(response.content))


def get_team_logo(
    team: str,
    conn: psycopg2.extensions.connection,
    source: str = "espn",
) -> None:
    """
    Retrieves a team logo from the database based on the team and source.

    Args:
        team (str): The team abbreviation.
        source (str, optional): The source of the logo. Defaults to "espn".
        conn (psycopg2.extensions.connection): The database connection.

    Returns:
        PIL.Image: The team logo image object.
    """
    query = f"""
        select team_logo_{source.lower()}
        from "nflfastR_logos"
        where team_abbr = '{team.upper()}'
    """

    # print(f"Team: {team}")
    # print(f"Source: {source}")
    # print(query)

    df = pd.read_sql_query(
        sql=query,
        con=conn,
    )
    url = df[f"team_logo_{source}"][0]

    return get_image_from_url(url=url)


# Query Functions
def gamerecap_selectbox_query(season: int):
    return f"""
            select distinct 
                game_id,
                old_game_id
            from "nflfastR_pbp"
            where season = {season}
            order by old_game_id desc
        """


def gamerecap_pbp_query(gameid: str):
    return f"""
            select *
            from "nflfastR_pbp"
            where game_id = '{gameid}'
            order by old_game_id, game_seconds_remaining desc
        """


def plot_wp_worm(
    df: pd.DataFrame,
    game_id: str,
) -> plt:
    df = df.query("qtr != 5")

    x = df["game_seconds_remaining"].astype(int)
    y = df["home_wp"].astype(float)

    # Create a density plot using Seaborn
    plt.figure(figsize=(8, 4))
    plt.plot(x, y)
    plt.gca().invert_xaxis()
    plt.ylim(0, 1)
    plt.xlabel("Game Seconds Remaining")
    plt.ylabel("Win Probability")
    plt.title("Win Probability", loc="left")
    return plt


def plot_wp_dist(
    df: pd.DataFrame,
    game_id: str,
) -> plt.figure:

    wp = df["home_wp"].astype(float)

    # Create a density plot using Seaborn
    fig, ax = plt.subplots(figsize=(2, 4))
    sns.kdeplot(
        y=wp,
        ax=ax,
        fill=True,
    )

    ax.set_yticks([])
    ax.set_ylim(0, 1)
    ax.set_xlabel("Density")
    ax.set_ylabel("")
    ax.set_title("Win Probability Distribution", loc="left")
    return fig


def plot_wp(
    df: pd.DataFrame,
    game_id: str,
    conn: psycopg2.extensions.connection,
) -> plt.figure:
    """
    Plots the win probability of a game.

    Args:
    game_id (str): The ID of the game.

    Returns:
    Tuple[plt.Figure, plt.Axes]: A tuple containing the figure and axes of the plot.
    """

    # df = pd.read_sql_query(gamerecap_pbp_query(game_id), conn)
    # print(df.head())

    df = df.query("qtr != 5")

    x = df["game_seconds_remaining"].astype(int)
    y = df["vegas_home_wp"].astype(float)

    away_team = game_id.split("_")[2]
    home_team = game_id.split("_")[3]

    away_team_color = get_team_color(
        away_team,
        conn=conn,
    )
    home_team_color = get_team_color(
        home_team,
        conn=conn,
    )

    # Create mid-point for plot styling
    df["mid_wp"] = 0.5
    mid_wp = df["mid_wp"]

    # Create a density plot using Seaborn
    fig, ax = plt.subplots(
        nrows=1,
        ncols=2,
        figsize=(8, 3),
        gridspec_kw={
            "height_ratios": [1],
            "width_ratios": [3, 1],
            "wspace": 0,
            "hspace": 0,
        },
    )

    # Add Logos
    away_imagebox = OffsetImage(
        np.array(
            get_team_logo(
                team=away_team,
                conn=conn,
            )
        ),
        alpha=1,
        zoom=0.1,
    )
    away_ab = AnnotationBbox(
        away_imagebox,
        (3300, 0.15),
        frameon=False,
    )
    ax[0].add_artist(
        away_ab,
    )

    home_imagebox = OffsetImage(
        np.array(
            get_team_logo(
                team=home_team,
                conn=conn,
            )
        ),
        alpha=1,
        zoom=0.1,
    )
    home_ab = AnnotationBbox(
        home_imagebox,
        (3300, 0.85),
        frameon=False,
    )
    ax[0].add_artist(
        home_ab,
    )

    # ax[0].plot(x, y, zorder=1)
    ax[0].axhline(0.5, color="black", linestyle="--")
    ax[0].fill_between(
        x,
        y,
        mid_wp,
        where=y >= mid_wp,
        interpolate=True,
        facecolor=home_team_color,
        alpha=0.8,
        zorder=3,
    )
    ax[0].fill_between(
        x,
        y,
        mid_wp,
        where=y <= mid_wp,
        interpolate=True,
        facecolor=away_team_color,
        alpha=0.8,
        zorder=3,
    )
    ax[0].invert_xaxis()
    ax[0].set_ylim(0, 1)
    ax[0].set_xlabel("Game Seconds Remaining")
    ax[0].set_ylabel("Win Probability")
    ax[0].set_title("Win Probability", loc="left")

    # Density Plot
    sns.kdeplot(
        y=y,
        ax=ax[1],
        fill=True,
        linewidth=0,
    )
    ax[1].axhline(0.5, color="black", linestyle="--")

    # ax[1].spines['top'].set_visible(False)
    # ax[1].spines['right'].set_visible(False)
    # ax[1].spines['bottom'].set_visible(False)
    ax[1].spines["left"].set_visible(False)

    ax[1].set_xticks([])
    ax[1].set_yticks([])
    ax[1].set_ylim(0, 1)
    ax[1].set_xlabel("Density")
    ax[1].set_ylabel("", labelpad=20)
    ax[1].set_title("Distribution", loc="left")

    fig.tight_layout()

    return fig
