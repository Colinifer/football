"""Pydantic models for Average Draft Position (ADP) tables."""

from __future__ import annotations

from datetime import datetime as dt_datetime

from shared.models.base import BaseFootballModel


class ADPPlayer(BaseFootballModel):
    """PostgreSQL table: `adp_players`."""

    id: int
    player_id: int
    name: str
    position: str
    team: str | None = None
    adp: float
    adp_formatted: str | None = None
    times_drafted: int | None = None
    high: float | None = None
    low: float | None = None
    stdev: float | None = None
    bye: int | None = None
    year: int
    scoring_format: str
    teams: int


class ADPHistory(BaseFootballModel):
    """PostgreSQL table: `adp_history`."""

    id: int
    player_id: int
    scoring_format: str
    teams: int
    timestamp_ms: int
    adp: float
    date: dt_datetime
