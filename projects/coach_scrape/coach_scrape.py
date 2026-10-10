# %%
"""Scrape NFL coaching film data using the nflpro library and stores it.

It connects to a PostgreSQL database, retrieves play-by-play data and \
     schedule information, and then uses the NFLProAPI to fetch \
        coaching film metadata. Finally, it downloads the coaching \
            film using the nflpro.film module.

The script is organized into several sections (using cell delimiters "%%") \
    for clarity and modularity, making it suitable for interactive \
        execution in environments like Jupyter Notebook or VS Code's \
            Python Interactive window.

Key Functions:
- create_connection: Establishes a connection to the PostgreSQL database.
- execute_query: Executes a SQL query and returns the result.
- query_to_dataframe: Executes a SQL query and returns the result \
    as a Pandas DataFrame.
- write_dataframe_to_postgres: Writes a Pandas DataFrame to a PostgreSQL table.

Data Flow:
1. Connect to the PostgreSQL database.
2. Retrieve play-by-play data from the nflfastR_pbp table.
3. Retrieve schedule data from the nflfastR_schedule table.
4. Use the NFLProAPI to fetch coaching film metadata for each play.
5. Download the coaching film using the nflpro.film module.
"""

# %%
import argparse
import os
from datetime import UTC, datetime
from pathlib import Path

import nflpro
from dotenv import load_dotenv

load_dotenv(Path(__file__).resolve().parents[2] / ".env")


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Scrape coaching film metadata and download plays."
    )
    # 1. Accept list of teams (e.g. -t CIN PHI KC)
    parser.add_argument(
        "--teams",
        "-t",
        nargs="+",
        default=["CIN"],
        help="One or more team abbreviations (e.g. -t CIN PHI). Default: ['CIN']",
    )
    # 2. Optional player filters
    parser.add_argument(
        "--passer",
        "-p",
        type=str,
        default=None,
        help="Passer player name (e.g. J.Burrow). Optional.",
    )
    parser.add_argument(
        "--receiver",
        "-r",
        type=str,
        default=None,
        help="Receiver player name (e.g. J.Chase). Optional.",
    )
    parser.add_argument(
        "--rusher",
        type=str,
        default=None,
        help="Rusher player name (e.g. J.Mixon). Optional.",
    )
    parser.add_argument(
        "--week",
        "-w",
        type=int,
        default=2,
        help="NFL week number. Default: 2",
    )
    parser.add_argument(
        "--limit",
        "-l",
        type=int,
        default=5,
        help="Maximum number of plays to fetch. Default: 5",
    )
    parser.add_argument(
        "--game-date",
        "-d",
        type=str,
        default=datetime.now(UTC).strftime("%Y-%m-%d"),
        help="Upper bound game date filter (YYYY-MM-DD). Default: today",
    )
    # 3. Sort metric and direction
    parser.add_argument(
        "--sort-by",
        "-s",
        choices=["wpa", "epa", "vegas_wpa", "yards_gained", "air_yards"],
        default="wpa",
        help="Metric column to sort by. Default: wpa",
    )
    parser.add_argument(
        "--order",
        choices=["asc", "desc"],
        default="desc",
        help="Sort direction (asc or desc). Default: desc",
    )

    args, _ = parser.parse_known_args()
    return args


args = parse_args()

# %%
config = {
    "host": os.getenv("DB_HOST_LOCAL", "moose.local"),
    "port": os.getenv("DB_PORT_LOCAL", "4432"),
    "database": os.getenv("DB_NAME"),
    "username": os.getenv("DB_USER"),
    "password": os.getenv("DB_PASS"),
}

# %%
schema_df = nflpro.query_to_dataframe(
    conn=nflpro.create_connection(config=config),
    query="""
    select *
    from "nflfastR_pbp"
    where season = (select max(season) from "nflfastR_pbp")
    limit 0
    """,
)

nflfastr_columns = schema_df.columns

# %% [markdown]
# ### Get plays

# %%
nflpro_api = nflpro.NFLProAPI()

# %%
# Dynamically build WHERE conditions and parameters
where_clauses = [
    'season = (select max(season) from "nflfastR_pbp")',
    "play = 1",
]
params = []

if args.teams:
    where_clauses.append("posteam = ANY(%s)")
    params.append(list(args.teams))

if args.passer:
    where_clauses.append("passer_player_name = %s")
    params.append(args.passer)

if args.receiver:
    where_clauses.append("receiver_player_name = %s")
    params.append(args.receiver)

if args.rusher:
    where_clauses.append("rusher_player_name = %s")
    params.append(args.rusher)

if args.week is not None:
    where_clauses.append("week = %s")
    params.append(args.week)

if args.game_date:
    where_clauses.append("game_date < %s")
    params.append(args.game_date)

# Add LIMIT to params
params.append(args.limit)

where_sql = " AND\n            ".join(where_clauses)
order_sql = f"{args.sort_by} {args.order.upper()}"

query = f"""
    with ranked_plays as (
        select
            season,
            week,
            season_type,
            game_id,
            old_game_id,
            nfl_api_id,
            cast(play_id as varchar) as play_id,
            posteam,
            defteam,
            home_score,
            away_score,
            "desc",
            epa,
            wpa,
            row_number() over (
                partition by posteam
                order by {order_sql} nulls last
            ) as rn
        from "nflfastR_pbp"
        where
            {where_sql}
    )
    select
        season,
        week,
        season_type,
        game_id,
        old_game_id,
        nfl_api_id,
        play_id,
        posteam,
        defteam,
        home_score,
        away_score,
        "desc",
        epa,
        wpa
    from ranked_plays
    where rn <= %s
    order by posteam, rn
"""

plays_df = nflpro.query_to_dataframe(
    conn=nflpro.create_connection(config=config),
    query=query,
    params=tuple(params),
    print_query=True,
)

sep = "=" * 60
sub_sep = "-" * 60

print(
    f"\n{sep}",
    "\nQuery:",
    "\n{sep}\n\n{query}\n\n{sub_sep}\n",
)
print(
    f"\n{sep}",
    "\nPlays:",
    "\n{sep}\n",
)

print(plays_df)
print("=" * 60)

# %%
df = nflpro.download_film_from_dataframe(
    nfl_pro_api=nflpro_api,
    plays_df=plays_df,
    column_names={
        "game_id": "game_id",
        "old_game_id": "old_game_id",
        "nfl_api_id": "nfl_api_id",
        "posteam": "posteam",
        "play_id": "play_id",
    },
)

print(df)
