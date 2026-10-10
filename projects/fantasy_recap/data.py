"""Data loading, caching, and SQL queries for the Fantasy Football Streamlit Dashboard."""

from __future__ import annotations

import os
from typing import Any

import pandas as pd
import streamlit as st
from dotenv import find_dotenv, load_dotenv
from sqlalchemy import create_engine, text

load_dotenv(find_dotenv())


def get_db_engine():
    """Build PostgreSQL database engine using environment variables."""
    host = os.getenv("DB_HOST_LOCAL", os.getenv("DB_HOST", "localhost"))
    port = os.getenv("DB_PORT_LOCAL", os.getenv("DB_PORT", "5432"))
    database = os.getenv("DB_NAME", "football")
    user = os.getenv("DB_USER", "postgres")
    password = os.getenv("DB_PASS", "")

    connection_string = f"postgresql+psycopg2://{user}:{password}@{host}:{port}/{database}"
    return create_engine(connection_string)


@st.cache_data(ttl=600)
def get_franchises() -> list[str]:
    """Retrieve all distinct fantasy franchises from PostgreSQL."""
    engine = get_db_engine()
    query = 'SELECT DISTINCT franchise_name FROM "ffscrapR_rosters" WHERE franchise_name IS NOT NULL ORDER BY franchise_name'
    with engine.connect() as conn:
        df = pd.read_sql(query, conn)
    return df["franchise_name"].tolist()


@st.cache_data(ttl=600)
def get_seasons() -> list[int]:
    """Retrieve available seasons in player stats."""
    engine = get_db_engine()
    query = 'SELECT DISTINCT season FROM "nflfastR_player_stats_week" WHERE season IS NOT NULL ORDER BY season DESC'
    with engine.connect() as conn:
        df = pd.read_sql(query, conn)
    return df["season"].tolist()


@st.cache_data(ttl=600)
def get_roster_players(franchise_name: str, season: int | None = None) -> pd.DataFrame:
    """Retrieve rostered players for a given franchise with crosswalk IDs."""
    engine = get_db_engine()
    query = """
    SELECT 
        r.franchise_id,
        r.franchise_name,
        r.player_name,
        r.pos as roster_pos,
        r.team as roster_team,
        r.status as roster_status,
        c.gsis_id,
        c.fpid,
        c.espn_id,
        c.nfl_name,
        c.nfl_pos,
        c.nfl_team
    FROM "ffscrapR_rosters" r
    LEFT JOIN "player_crosswalk" c ON r.player_name = c.name OR r.player_id::text = c.espn_id
    WHERE r.franchise_name = :franchise_name
    ORDER BY 
        CASE r.pos 
            WHEN 'QB' THEN 1 
            WHEN 'RB' THEN 2 
            WHEN 'WR' THEN 3 
            WHEN 'TE' THEN 4 
            WHEN 'K' THEN 5 
            WHEN 'DEF' THEN 6 
            WHEN 'DST' THEN 6 
            ELSE 7 
        END,
        r.player_name;
    """
    with engine.connect() as conn:
        df = pd.read_sql(text(query), conn, params={"franchise_name": franchise_name})
    return df


@st.cache_data(ttl=600)
def get_free_agents_data(
    season: int = 2026,
    scoring_format: str = "half-ppr",
    min_games: int = 1,
) -> pd.DataFrame:
    """Retrieve all available free agents with aggregate season stats, volume metrics, and projections."""
    engine = get_db_engine()
    query = """
    WITH rostered_ids AS (
        SELECT DISTINCT c.gsis_id
        FROM "ffscrapR_rosters" r
        JOIN "player_crosswalk" c ON r.player_name = c.name OR r.player_id::text = c.espn_id
        WHERE c.gsis_id IS NOT NULL
    ),
    latest_proj AS (
        SELECT DISTINCT ON (fpid)
            fpid,
            (stats->>'points')::float as proj_pts_std,
            (stats->>'points_half')::float as proj_pts_half,
            (stats->>'points_ppr')::float as proj_pts_ppr
        FROM "ffpros_projections"
        ORDER BY fpid, _fetched_at DESC
    ),
    weekly_stats AS (
        SELECT 
            w.player_id,
            COALESCE(w.player_display_name, w.player_name) as player_name,
            w.position,
            w.team,
            w.season,
            w.week,
            w.opponent_team,
            COALESCE(w.completions, 0) as completions,
            COALESCE(w.attempts, 0) as pass_att,
            COALESCE(w.passing_yards, 0) as pass_yds,
            COALESCE(w.passing_tds, 0) as pass_tds,
            COALESCE(w.passing_interceptions, 0) as pass_ints,
            COALESCE(w.carries, 0) as carries,
            COALESCE(w.rushing_yards, 0) as rush_yds,
            COALESCE(w.rushing_tds, 0) as rush_tds,
            COALESCE(w.receptions, 0) as receptions,
            COALESCE(w.targets, 0) as targets,
            COALESCE(w.receiving_yards, 0) as rec_yds,
            COALESCE(w.receiving_tds, 0) as rec_tds,
            COALESCE(w.receiving_air_yards, 0) as air_yards,
            COALESCE(w.target_share, 0) as target_share,
            COALESCE(w.air_yards_share, 0) as air_yards_share,
            COALESCE(w.wopr, 0) as wopr,
            COALESCE(w.racr, 0) as racr,
            CASE WHEN COALESCE(w.targets, 0) > 0 
                 THEN (COALESCE(w.receiving_air_yards, 0)::float / w.targets::float)
                 ELSE 0.0 END as adot,
            COALESCE(w.fantasy_points, 0) as points_std,
            COALESCE(w.fantasy_points_ppr, 0) as points_ppr,
            ((COALESCE(w.fantasy_points, 0) + COALESCE(w.fantasy_points_ppr, 0)) / 2.0) as points_half,
            (COALESCE(w.carries, 0) + COALESCE(w.targets, 0)) as opportunities,
            (COALESCE(w.rushing_yards, 0) + COALESCE(w.receiving_yards, 0)) as scrimmage_yards,
            (COALESCE(w.rushing_tds, 0) + COALESCE(w.receiving_tds, 0) + COALESCE(w.special_teams_tds, 0)) as total_tds,
            COALESCE(w.passing_epa, 0) + COALESCE(w.rushing_epa, 0) + COALESCE(w.receiving_epa, 0) as total_epa
        FROM "nflfastR_player_stats_week" w
        WHERE w.season = :season
    )
    SELECT 
        s.player_id,
        s.player_name,
        s.position,
        s.team,
        COUNT(*) as games,
        ROUND(AVG(s.points_half)::numeric, 2) as ppg_half,
        ROUND(AVG(s.points_ppr)::numeric, 2) as ppg_ppr,
        ROUND(AVG(s.points_std)::numeric, 2) as ppg_std,
        ROUND(PERCENTILE_CONT(0.25) WITHIN GROUP (ORDER BY s.points_half)::numeric, 2) as floor_half,
        ROUND(PERCENTILE_CONT(0.75) WITHIN GROUP (ORDER BY s.points_half)::numeric, 2) as ceiling_half,
        ROUND(STDDEV(s.points_half)::numeric, 2) as std_half,
        ROUND(AVG(s.targets)::numeric, 2) as targets_pg,
        ROUND(AVG(s.receptions)::numeric, 2) as rec_pg,
        ROUND(AVG(s.rec_yds)::numeric, 2) as rec_yds_pg,
        ROUND(AVG(s.air_yards)::numeric, 2) as air_yds_pg,
        ROUND(AVG(s.adot)::numeric, 2) as avg_adot,
        ROUND(AVG(s.target_share)::numeric * 100.0, 1) as avg_target_share_pct,
        ROUND(AVG(s.air_yards_share)::numeric * 100.0, 1) as avg_air_yards_share_pct,
        ROUND(AVG(s.wopr)::numeric, 3) as avg_wopr,
        ROUND(AVG(s.carries)::numeric, 2) as carries_pg,
        ROUND(AVG(s.rush_yds)::numeric, 2) as rush_yds_pg,
        ROUND(AVG(s.scrimmage_yards)::numeric, 2) as scrimmage_yds_pg,
        ROUND(AVG(s.opportunities)::numeric, 2) as opps_pg,
        ROUND(AVG(s.total_epa)::numeric, 2) as avg_epa_pg,
        SUM(s.total_tds) as total_tds,
        COALESCE(lp.proj_pts_half, 0.0) as proj_pts_half,
        COALESCE(lp.proj_pts_ppr, 0.0) as proj_pts_ppr,
        COALESCE(lp.proj_pts_std, 0.0) as proj_pts_std,
        c.fpid
    FROM weekly_stats s
    LEFT JOIN "player_crosswalk" c ON s.player_id = c.gsis_id
    LEFT JOIN latest_proj lp ON c.fpid = lp.fpid
    WHERE s.position IN ('QB', 'RB', 'WR', 'TE')
      AND (s.player_id NOT IN (SELECT gsis_id FROM rostered_ids))
    GROUP BY s.player_id, s.player_name, s.position, s.team, lp.proj_pts_half, lp.proj_pts_ppr, lp.proj_pts_std, c.fpid
    HAVING COUNT(*) >= :min_games
    ORDER BY ppg_half DESC;
    """
    with engine.connect() as conn:
        df = pd.read_sql(text(query), conn, params={"season": season, "min_games": min_games})

    # Select primary PPG based on scoring format
    if scoring_format.lower() == "ppr":
        df["fantasy_ppg"] = df["ppg_ppr"]
        df["proj_points"] = df["proj_pts_ppr"]
    elif scoring_format.lower() == "standard":
        df["fantasy_ppg"] = df["ppg_std"]
        df["proj_points"] = df["proj_pts_std"]
    else:
        df["fantasy_ppg"] = df["ppg_half"]
        df["proj_points"] = df["proj_pts_half"]

    return df


@st.cache_data(ttl=600)
def get_player_weekly_stats(player_ids: list[str], season: int | None = None) -> pd.DataFrame:
    """Retrieve full weekly game-by-game log for selected player IDs."""
    if not player_ids:
        return pd.DataFrame()

    engine = get_db_engine()
    season_filter = "AND w.season = :season" if season is not None else ""
    params: dict[str, Any] = {"player_ids": tuple(player_ids)}
    if season is not None:
        params["season"] = season

    query = f"""
    SELECT 
        w.player_id,
        COALESCE(w.player_display_name, w.player_name) as player_name,
        w.position,
        w.team,
        w.season,
        w.week,
        w.opponent_team,
        w.game_id,
        COALESCE(w.completions, 0) as completions,
        COALESCE(w.attempts, 0) as pass_att,
        COALESCE(w.passing_yards, 0) as pass_yds,
        COALESCE(w.passing_tds, 0) as pass_tds,
        COALESCE(w.passing_interceptions, 0) as pass_ints,
        COALESCE(w.carries, 0) as carries,
        COALESCE(w.rushing_yards, 0) as rush_yds,
        COALESCE(w.rushing_tds, 0) as rush_tds,
        COALESCE(w.receptions, 0) as receptions,
        COALESCE(w.targets, 0) as targets,
        COALESCE(w.receiving_yards, 0) as rec_yds,
        COALESCE(w.receiving_tds, 0) as rec_tds,
        COALESCE(w.receiving_air_yards, 0) as air_yards,
        COALESCE(w.target_share, 0) as target_share,
        COALESCE(w.air_yards_share, 0) as air_yards_share,
        COALESCE(w.wopr, 0) as wopr,
        COALESCE(w.racr, 0) as racr,
        CASE WHEN COALESCE(w.targets, 0) > 0 
             THEN ROUND((COALESCE(w.receiving_air_yards, 0)::float / w.targets::float)::numeric, 2)
             ELSE 0.0 END as adot,
        COALESCE(w.fantasy_points, 0) as points_std,
        COALESCE(w.fantasy_points_ppr, 0) as points_ppr,
        ROUND(((COALESCE(w.fantasy_points, 0) + COALESCE(w.fantasy_points_ppr, 0)) / 2.0)::numeric, 2) as points_half,
        (COALESCE(w.carries, 0) + COALESCE(w.targets, 0)) as opportunities,
        (COALESCE(w.rushing_yards, 0) + COALESCE(w.receiving_yards, 0)) as scrimmage_yards,
        (COALESCE(w.rushing_tds, 0) + COALESCE(w.receiving_tds, 0) + COALESCE(w.special_teams_tds, 0)) as total_tds,
        COALESCE(w.passing_epa, 0) + COALESCE(w.rushing_epa, 0) + COALESCE(w.receiving_epa, 0) as total_epa
    FROM "nflfastR_player_stats_week" w
    WHERE w.player_id IN :player_ids
      {season_filter}
    ORDER BY w.player_id, w.season ASC, w.week ASC;
    """  # noqa: S608
    with engine.connect() as conn:
        df = pd.read_sql(text(query), conn, params=params)
    return df


@st.cache_data(ttl=600)
def get_all_active_players_summary(season: int = 2026) -> pd.DataFrame:
    """Retrieve summary of ALL active players (both rostered and free agents) for the search dropdown."""
    engine = get_db_engine()
    query = """
    WITH rostered_info AS (
        SELECT DISTINCT 
            c.gsis_id,
            r.franchise_name
        FROM "ffscrapR_rosters" r
        JOIN "player_crosswalk" c ON r.player_name = c.name OR r.player_id::text = c.espn_id
        WHERE c.gsis_id IS NOT NULL
    )
    SELECT 
        w.player_id,
        COALESCE(w.player_display_name, w.player_name) as player_name,
        w.position,
        w.team,
        COUNT(*) as games,
        ROUND(AVG((COALESCE(w.fantasy_points, 0) + COALESCE(w.fantasy_points_ppr, 0)) / 2.0)::numeric, 2) as ppg_half,
        ROUND(AVG(COALESCE(w.fantasy_points_ppr, 0))::numeric, 2) as ppg_ppr,
        ROUND(AVG(COALESCE(w.fantasy_points, 0))::numeric, 2) as ppg_std,
        ROUND(AVG(COALESCE(w.targets, 0))::numeric, 2) as targets_pg,
        ROUND(AVG(COALESCE(w.receptions, 0))::numeric, 2) as rec_pg,
        ROUND(AVG(COALESCE(w.receiving_yards, 0))::numeric, 2) as rec_yds_pg,
        ROUND(AVG(COALESCE(w.receiving_air_yards, 0))::numeric, 2) as air_yds_pg,
        ROUND(AVG(COALESCE(w.target_share, 0))::numeric * 100.0, 1) as avg_target_share_pct,
        ROUND(AVG(COALESCE(w.wopr, 0))::numeric, 3) as avg_wopr,
        ROUND(AVG(COALESCE(w.carries, 0))::numeric, 2) as carries_pg,
        ROUND(AVG(COALESCE(w.rushing_yards, 0))::numeric, 2) as rush_yds_pg,
        ROUND(AVG(COALESCE(w.rushing_yards, 0) + COALESCE(w.receiving_yards, 0))::numeric, 2) as scrimmage_yds_pg,
        SUM(COALESCE(w.rushing_tds, 0) + COALESCE(w.receiving_tds, 0) + COALESCE(w.special_teams_tds, 0)) as total_tds,
        r.franchise_name,
        (r.franchise_name IS NOT NULL) as is_rostered
    FROM "nflfastR_player_stats_week" w
    LEFT JOIN rostered_info r ON w.player_id = r.gsis_id
    WHERE w.season = :season
      AND w.position IN ('QB', 'RB', 'WR', 'TE')
    GROUP BY w.player_id, w.player_display_name, w.player_name, w.position, w.team, r.franchise_name
    ORDER BY ppg_half DESC;
    """
    with engine.connect() as conn:
        df = pd.read_sql(text(query), conn, params={"season": season})
    return df
