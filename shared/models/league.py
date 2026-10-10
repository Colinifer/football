"""Pydantic models for fantasy leagues, teams, drafts, keepers, and rosters."""

from __future__ import annotations

from datetime import datetime as dt_datetime

from shared.models.base import BaseFootballModel


class League(BaseFootballModel):
    """PostgreSQL table: `leagues`."""

    league_id: str
    league_name: str
    total_rounds: int | None = None


class LeagueTeam(BaseFootballModel):
    """PostgreSQL table: `teams`."""

    league_id: str
    team_name: str
    draft_position: int | None = None


class DraftSelection(BaseFootballModel):
    """PostgreSQL table: `draft_selections`."""

    league_id: str
    pick_index: int
    team_name: str | None = None
    player_name: str | None = None
    position: str | None = None
    is_keeper: bool | None = False


class Keeper(BaseFootballModel):
    """PostgreSQL table: `keepers`."""

    league_id: str
    team_name: str
    round_number: int
    player_name: str | None = None


class FFDraftResult(BaseFootballModel):
    """PostgreSQL table: `ff_draft_results`."""

    league: str
    pick_num: int
    round: int | None = None
    team: str | None = None
    player: str | None = None


class FFScrapRRoster(BaseFootballModel):
    """PostgreSQL table: `ffscrapR_rosters`."""

    franchise_id: int | None = None
    franchise_name: str | None = None
    player_id: int | None = None
    player_name: str | None = None
    team: str | None = None
    pos: str | None = None
    status: str | None = None
    acquisition_type: str | None = None
    acquisition_date: dt_datetime | None = None


class FFScrapRTransaction(BaseFootballModel):
    """PostgreSQL table: `ffscrapR_transactions`."""

    timestamp: dt_datetime | None = None
    type: str | None = None
    type_desc: str | None = None
    franchise_id: int | None = None
    franchise_name: str | None = None
    player_id: int | None = None
    player_name: str | None = None
    pos: str | None = None
    team: str | None = None
    bbid_spent: int | None = None
    trade_partner: int | None = None
