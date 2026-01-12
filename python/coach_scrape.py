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

from datetime import UTC, datetime

import nflpro

# %%
config = {
    "host": "moose.local",
    # "port":"4432",  # noqa: ERA001
    # "database":"football",  # noqa: ERA001
    # "username":"user",  # noqa: ERA001
    # "password":"pass",  # noqa: ERA001
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
nflfastr_columns  # noqa: B018

# %% [markdown]
# ### Get schedule

# %%
nflfastr_schedule_df = nflpro.query_to_dataframe(
    conn=nflpro.create_connection(config=config),
    query="""select
        game_id,
        old_game_id,
        season,
        week,
        game_type
    from "nflfastR_schedule"
    where season >= %s
    order by 1
    """,
    params=(2025,),
)

nflfastr_schedule_df = nflfastr_schedule_df.assign(
    week_slug=lambda x: x.apply(
        lambda x: nflpro.create_week_slug(
            week=x["week"],
            game_type=x["game_type"],
        ),
        axis=1,
    ),
)

nflfastr_schedule_df  # noqa: B018

# %% [markdown]
# ### Get plays

# %%
nflpro_api = nflpro.NFLProAPI()
# nflpro_api._refresh_token()  # noqa: ERA001


# %%
today = datetime.now(UTC).strftime("%Y-%m-%d")

plays_df = nflpro.query_to_dataframe(
    conn=nflpro.create_connection(config=config),
    query="""select
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
        epa
    from "nflfastR_pbp"
      where season = (select max(season) from "nflfastR_pbp")
        and play = 1
        and posteam = %s
        and receiver_player_name = %s
        and week = 19
        and game_date < %s
    order by epa desc
    --limit 10
    """,
    params=(
        "LA",
        "P.Nacua",
        today,
    ),
    print_query=True,
)

plays_df  # noqa: B018

# %%
nflpro.download_film_from_dataframe(
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

# %%

nflpro_api._get_account_info()


# %%

df = nflpro.query_to_dataframe(
    conn=nflpro.create_connection(config=config),
    query="""select *
    from "nflfastR_pbp"
    where season = (select max(season) from "nflfastR_pbp")
    limit 10
    """,
    params=(),
    print_query=True,
)

df.columns
