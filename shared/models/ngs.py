"""Pydantic models for Next Gen Stats (NGS) tables."""

from __future__ import annotations

from pydantic import Field

from shared.models.base import BaseFootballModel


class NGSPassingStats(BaseFootballModel):
    """PostgreSQL table: `ngs_passing_stats`."""

    display_name: str | None = None
    position: str | None = None
    team_id: str | None = None
    jersey_number: int | None = None
    cmp: int | None = None
    att: int | None = None
    yds: int | None = None
    td: int | None = None
    int_: int | None = Field(default=None, alias="int")
    rating: float | None = None
    ypa: float | None = None
    cmp_pct: float | None = None
    sack: int | None = None
    x_cmp: float | None = None
    cpoe: float | None = None
    db: int | None = None
    epa: float | None = None
    epa_db: float | None = None
    avg_ttt: float | None = None
    avg_ttp: float | None = None
    avg_tts: float | None = None
    qbp: int | None = None
    qbp_r: float | None = None
    blitz_r: float | None = None
    drop: int | None = None
    drop_r: float | None = None
    ay: float | None = None
    yac: float | None = None
    x_yac: float | None = None
    yac_pct: float | None = None
    ay_att: float | None = None
    avg_sep: float | None = None
    deep_att_pct: float | None = None
    pa_db_pct: float | None = None
    tw_att_pct: float | None = None
    nfl_id: str | None = None
    week_slug: str | None = None
    opponent_team_id: str | None = None
    game_id: int | None = None
    final_score: str | None = None
    is_home: bool | None = None
    game_result: str | None = None
    position_group: str | None = None
    ngs_position_group: str | None = None
    fapi_game_id: str | None = None
    headshot: str | None = None
    short_name: str | None = None
    ngs_position: str | None = None
    gp: int | None = None
    gs: int | None = None
    tg: int | None = None
    total_tg: int | None = None
    qp: bool | None = None
    cmp_pg: int | None = None
    att_pg: int | None = None
    yds_pg: int | None = None
    td_pg: int | None = None
    int_pg: int | None = None
    sack_pg: int | None = None
    db_pg: int | None = None
    epa_pg: float | None = None
    qbp_pg: int | None = None
    drop_pg: int | None = None
    tw_att_pg: int | None = None
    season: int | None = None


class NGSRushingStats(BaseFootballModel):
    """PostgreSQL table: `ngs_rushing_stats`."""

    display_name: str | None = None
    position: str | None = None
    team_id: str | None = None
    jersey_number: int | None = None
    att: int | None = None
    yds: int | None = None
    td: int | None = None
    ypc: float | None = None
    epa: float | None = None
    epa_att: float | None = None
    x_ry: float | None = None
    x_ypc: float | None = None
    ryoe: float | None = None
    ryoe_att: float | None = None
    yaco: float | None = None
    yaco_att: float | None = None
    ybco: float | None = None
    success: float | None = None
    fum: int | None = None
    lost: int | None = None
    rush10_p_yds: int | None = None
    rush15_p_mph: int | None = None
    rush20_p_mph: int | None = None
    eff: float | None = None
    in_t_pct: float | None = None
    st_box_pct: float | None = None
    under_pct: float | None = None
    nfl_id: str | None = None
    week_slug: str | None = None
    opponent_team_id: str | None = None
    game_id: int | None = None
    final_score: str | None = None
    is_home: bool | None = None
    game_result: str | None = None
    position_group: str | None = None
    ngs_position_group: str | None = None
    fapi_game_id: str | None = None
    headshot: str | None = None
    short_name: str | None = None
    ngs_position: str | None = None
    gp: int | None = None
    gs: int | None = None
    tg: int | None = None
    total_tg: int | None = None
    qr: bool | None = None
    att_pg: int | None = None
    yds_pg: int | None = None
    td_pg: int | None = None
    epa_pg: float | None = None
    x_ry_pg: float | None = None
    ryoe_pg: float | None = None
    yaco_pg: float | None = None
    ybco_pg: float | None = None
    fum_pg: int | None = None
    lost_pg: int | None = None
    rush10_p_yds_pg: int | None = None
    rush15_p_mph_pg: int | None = None
    rush20_p_mph_pg: int | None = None
    season: int | None = None


class NGSReceivingStats(BaseFootballModel):
    """PostgreSQL table: `ngs_receiving_stats`."""

    display_name: str | None = None
    position: str | None = None
    team_id: str | None = None
    jersey_number: int | None = None
    rec: int | None = None
    yds: int | None = None
    td: int | None = None
    tgt: int | None = None
    rt: int | None = None
    int_: int | None = Field(default=None, alias="int")
    rating: float | None = None
    catch: float | None = None
    x_catch: float | None = None
    croe: float | None = None
    yds_rec: float | None = None
    yds_rt: float | None = None
    epa: float | None = None
    epa_tgt: float | None = None
    epa_rt: float | None = None
    drop: int | None = None
    drop_tgt: float | None = None
    yac: int | None = None
    x_yac: int | None = None
    yacoe: int | None = None
    yac_rec: float | None = None
    avg_sep: float | None = None
    ay: float | None = None
    ay_tgt: float | None = None
    tgt_rt: float | None = None
    avg_rt_dep: float | None = None
    ez_tgt: int | None = None
    ez_rec: int | None = None
    deep_tgt_pct: float | None = None
    tw_pct: float | None = None
    nfl_id: str | None = None
    week_slug: str | None = None
    opponent_team_id: str | None = None
    game_id: int | None = None
    final_score: str | None = None
    is_home: bool | None = None
    game_result: str | None = None
    position_group: str | None = None
    ngs_position_group: str | None = None
    fapi_game_id: str | None = None
    headshot: str | None = None
    short_name: str | None = None
    ngs_position: str | None = None
    gp: int | None = None
    gs: int | None = None
    tg: int | None = None
    total_tg: int | None = None
    qr: bool | None = None
    rt_pg: int | None = None
    tgt_pg: int | None = None
    rec_pg: int | None = None
    yds_pg: int | None = None
    td_pg: int | None = None
    int_pg: int | None = None
    epa_pg: float | None = None
    drop_pg: int | None = None
    yac_pg: int | None = None
    x_yac_pg: int | None = None
    yacoe_pg: int | None = None
    ay_pg: float | None = None
    ez_tgt_pg: int | None = None
    ez_rec_pg: int | None = None
    season: int | None = None


class NGSDefenseStats(BaseFootballModel):
    """PostgreSQL table: `ngs_defense_stats`."""

    display_name: str | None = None
    position: str | None = None
    team_id: str | None = None
    jersey_number: int | None = None
    snap: int | None = None
    rd: int | None = None
    pr: int | None = None
    tck: int | None = None
    t_stop: int | None = None
    h_stop: int | None = None
    qbp: int | None = None
    sack: float | None = None
    tgt_nd: int | None = None
    rec_nd: int | None = None
    rec_yds_nd: int | None = None
    rec_td_nd: int | None = None
    int_: int | None = Field(default=None, alias="int")
    pass_rating_nd: float | None = None
    game_snap: int | None = None
    team_snap: int | None = None
    qbp_r: float | None = None
    nfl_id: str | None = None
    week_slug: str | None = None
    opponent_team_id: str | None = None
    game_id: int | None = None
    final_score: str | None = None
    is_home: bool | None = None
    game_result: str | None = None
    position_group: str | None = None
    ngs_position_group: str | None = None
    fapi_game_id: str | None = None
    headshot: str | None = None
    short_name: str | None = None
    ngs_position: str | None = None
    gp: int | None = None
    gs: int | None = None
    tg: int | None = None
    total_tg: int | None = None
    snap_pct: float | None = None
    qd: bool | None = None
    season: int | None = None


class NGSHighlight(BaseFootballModel):
    """PostgreSQL table: `ngs_highlights`."""

    season: int | None = None
    game_id: int | None = None
    play_id: int | None = None
