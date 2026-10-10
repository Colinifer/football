"""Pydantic models for FantasyPros tables (players, projections, rankings, ECR)."""

from __future__ import annotations

from datetime import date as dt_date
from datetime import datetime as dt_datetime
from typing import Any

from pydantic import Field

from shared.models.base import BaseFootballModel


class FFProsPlayer(BaseFootballModel):
    """PostgreSQL table: `ffpros_players`."""

    player_id: int
    player_name: str | None = None
    short_name: str | None = None
    first_name: str | None = None
    last_name: str | None = None
    reverse_name: str | None = None
    position_id: str | None = None
    positions: dict[str, Any] | list[Any] | None = None
    team_id: str | None = None
    filename: str | None = None
    sportsdata_player_id: str | None = None
    large_image_url: str | None = None
    square_image_url: str | None = None
    image_url: str | None = None
    rank_ecr: int | None = None
    rank_adp: int | None = None
    rank_ecr_ppr: int | None = None
    rank_adp_ppr: int | None = None
    rank_ecr_half: int | None = None
    birthdate: str | None = None
    birthdatetime: str | None = None
    season: int | None = None
    week: int | None = None
    age: int | None = None
    draft_class: int | None = None
    rookie: str | None = None
    fetched_at: dt_datetime = Field(..., alias="_fetched_at")
    endpoint: str | None = Field(default=None, alias="_endpoint")
    context: dict[str, Any] | list[Any] | None = Field(default=None, alias="_context")


class FFProsProjection(BaseFootballModel):
    """PostgreSQL table: `ffpros_projections`."""

    id: int = Field(..., alias="_id")
    fpid: int | None = None
    mflid: int | None = None
    name: str | None = None
    position_id: str | None = None
    team_id: str | None = None
    filename: str | None = None
    stats: dict[str, Any] | list[Any] | None = None
    season: int | None = None
    week: int | None = None
    scoring: str | None = None
    fetched_at: dt_datetime = Field(..., alias="_fetched_at")
    endpoint: str | None = Field(default=None, alias="_endpoint")
    context: dict[str, Any] | list[Any] | None = Field(default=None, alias="_context")

    # Projected Passing Stats
    pass_att: float | None = None
    pass_cmp: float | None = None
    pass_yds: float | None = None
    pass_tds: float | None = None
    pass_ints: float | None = None
    pass_yds_300: float | None = None
    pass_yds_400: float | None = None

    # Projected Rushing Stats
    rush_att: float | None = None
    rush_yds: float | None = None
    rush_tds: float | None = None
    rush_yds_100: float | None = None
    rush_yds_200: float | None = None

    # Projected Receiving Stats
    rec_rec: float | None = None
    rec_yds: float | None = None
    rec_tds: float | None = None
    rec_yds_100: float | None = None
    rec_yds_200: float | None = None

    # Projected Fantasy Points
    points: float | None = None
    points_half: float | None = None
    points_ppr: float | None = None

    # Special / Misc
    c_2pt_tds: float | None = None
    fumbles: float | None = None
    ret_tds: float | None = None
    scrimage_yards_100: float | None = None
    scrimage_yards_200: float | None = None
    fg: float | None = None
    fga: float | None = None
    xpt: float | None = None

    # Projected Defense Stats
    def_ff: float | None = None
    def_fr: float | None = None
    def_int: float | None = None
    def_pa: float | None = None
    def_pa_a: float | None = None
    def_pa_b: float | None = None
    def_pa_c: float | None = None
    def_pa_d: float | None = None
    def_pa_e: float | None = None
    def_pa_f: float | None = None
    def_pa_g: float | None = None
    def_retd: float | None = None
    def_sack: float | None = None
    def_safety: float | None = None
    def_td: float | None = None
    def_tyda: float | None = None


class FFProsRanking(BaseFootballModel):
    """PostgreSQL table: `ffpros_rankings`."""

    id_: int = Field(..., alias="_id")
    id: int | None = None
    player_name: str | None = None
    short_name: str | None = None
    first_name: str | None = None
    last_name: str | None = None
    reverse_name: str | None = None
    position_id: str | None = None
    positions: dict[str, Any] | list[Any] | None = None
    team_id: str | None = None
    filename: str | None = None
    large_image_url: str | None = None
    square_image_url: str | None = None
    image_url: str | None = None
    rank: dict[str, Any] | list[Any] | None = None
    season: int | None = None
    week: int | None = None
    fetched_at: dt_datetime = Field(..., alias="_fetched_at")
    endpoint: str | None = Field(default=None, alias="_endpoint")
    context: dict[str, Any] | list[Any] | None = Field(default=None, alias="_context")


class FFProsECRRanking(BaseFootballModel):
    """PostgreSQL table: `ffpros_ecr_rankings`."""

    date: dt_date | None = None
    season: str | None = None
    ranking_type: str | None = None
    week: float | None = None
    scoring: str | None = None
    total_experts: int | None = None
    fantasypros_id: str | None = None
    player_name: str | None = None
    pos: str | None = None
    team: str | None = None
    rank: float | None = None
    ecr: float | None = None
    sd: float | None = None
    best: float | None = None
    worst: float | None = None
    player_positions: str | None = None
    player_short_name: str | None = None
    player_eligibility: str | None = None
    player_page_url: str | None = None
    player_filename: str | None = None
    player_bye_week: str | None = None
    player_owned_avg: float | None = None
    player_opponent: str | None = None
    player_opponent_id: str | None = None
    player_ecr_delta: int | None = None
    note: str | None = None
    tag: str | None = None
    recommendation: bool | None = None
    pos_rank: str | None = None
    gsis_id: str | None = None
    espn_id: str | None = None
    position: str | None = None
    franchise_id: int | None = None
    self_: bool | None = Field(default=None, alias="self")


class FFProsConsensusRanking(BaseFootballModel):
    """PostgreSQL table: `ffpros_consensus_rankings`."""

    id_: int = Field(..., alias="_id")
    player_id: int | None = None
    player_name: str | None = None
    sportsdata_id: str | None = None
    player_team_id: str | None = None
    player_position_id: str | None = None
    player_positions: str | None = None
    player_short_name: str | None = None
    player_eligibility: str | None = None
    player_yahoo_positions: str | None = None
    player_page_url: str | None = None
    player_filename: str | None = None
    player_square_image_url: str | None = None
    player_image_url: str | None = None
    player_yahoo_id: str | None = None
    cbs_player_id: str | None = None
    player_bye_week: str | None = None
    player_owned_avg: float | None = None
    player_owned_espn: float | None = None
    player_owned_yahoo: int | None = None
    player_ecr_delta: str | None = None
    rank_ecr: int | None = None
    rank_min: str | None = None
    rank_max: str | None = None
    rank_ave: str | None = None
    rank_std: str | None = None
    pos_rank: str | None = None
    tier: int | None = None
    season: int | None = None
    ranking_type: str | None = None
    week: int | None = None
    scoring: str | None = None
    position_id: str | None = None
    fetched_at: dt_datetime = Field(..., alias="_fetched_at")
    endpoint: str | None = Field(default=None, alias="_endpoint")
    context: dict[str, Any] | list[Any] | None = Field(default=None, alias="_context")


class FFProsComparePlayer(BaseFootballModel):
    """PostgreSQL table: `ffpros_compare_players`."""

    id_: int = Field(..., alias="_id")
    record_key: str | None = None
    player_name: str | None = None
    player_team_id: str | None = None
    player_position_id: str | None = None
    player_page_url: str | None = None
    player_square_image_url: str | None = None
    player_image_url: str | None = None
    season: int | None = None
    ranking_type: str | None = None
    week: int | None = None
    position_id: str | None = None
    fetched_at: dt_datetime = Field(..., alias="_fetched_at")
    endpoint: str | None = Field(default=None, alias="_endpoint")
    context: dict[str, Any] | list[Any] | None = Field(default=None, alias="_context")
