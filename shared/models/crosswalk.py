"""Pydantic model for player ID crosswalk mapping."""

from __future__ import annotations

from shared.models.base import BaseFootballModel


class PlayerCrosswalk(BaseFootballModel):
    """PostgreSQL table: `player_crosswalk`.

    Maps players across various data providers:
    FantasyPros (fpid), NFL / nflfastR (gsis_id), ESPN (espn_id),
    Pro-Football-Reference (pfr_id), PFF (pff_id), OverTheCap (otc_id),
    SportsData (sportsdata_player_id), Elias/ESB, etc.
    """

    match_status: str | None = None
    fpid: int | None = None
    name: str | None = None
    position: str | None = None
    team: str | None = None
    birthdate: str | None = None
    sportsdata_player_id: str | None = None
    match_type: str | None = None
    similarity: float | None = None
    gsis_id: str | None = None
    espn_id: str | None = None
    pfr_id: str | None = None
    pff_id: str | None = None
    esb_id: str | None = None
    nfl_id: str | None = None
    smart_id: str | None = None
    otc_id: str | None = None
    nfl_name: str | None = None
    nfl_pos: str | None = None
    nfl_team: str | None = None
    nfl_status: str | None = None
    nfl_birth_date: str | None = None
    is_matched: bool | None = None
