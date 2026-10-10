"""Pydantic models for NFLverse / nflfastR tables."""

from __future__ import annotations

from datetime import date as dt_date
from datetime import datetime as dt_datetime

from pydantic import computed_field

from shared.models.base import BaseFootballModel


class NFLPlayer(BaseFootballModel):
    """PostgreSQL table: `nflfastR_players`."""

    gsis_id: str | None = None
    display_name: str | None = None
    common_first_name: str | None = None
    first_name: str | None = None
    last_name: str | None = None
    short_name: str | None = None
    football_name: str | None = None
    suffix: str | None = None
    esb_id: str | None = None
    nfl_id: str | None = None
    pfr_id: str | None = None
    pff_id: str | None = None
    otc_id: str | None = None
    espn_id: str | None = None
    smart_id: str | None = None
    birth_date: str | None = None
    position_group: str | None = None
    position: str | None = None
    ngs_position_group: str | None = None
    ngs_position: str | None = None
    height: int | None = None
    weight: int | None = None
    headshot: str | None = None
    college_name: str | None = None
    college_conference: str | None = None
    jersey_number: str | None = None
    rookie_season: int | None = None
    last_season: int | None = None
    latest_team: str | None = None
    status: str | None = None
    ngs_status: str | None = None
    ngs_status_short_description: str | None = None
    years_of_experience: int | None = None
    pff_position: str | None = None
    pff_status: str | None = None
    draft_year: int | None = None
    draft_round: int | None = None
    draft_pick: int | None = None
    draft_team: str | None = None


class NFLWeeklyPlayerStats(BaseFootballModel):
    """PostgreSQL table: `nflfastR_player_stats_week` / `nflfastR_player_stats_weekly`."""

    player_id: str | None = None
    player_name: str | None = None
    player_display_name: str | None = None
    position: str | None = None
    position_group: str | None = None
    headshot_url: str | None = None
    season: int | None = None
    week: int | None = None
    season_type: str | None = None
    team: str | None = None
    recent_team: str | None = None
    opponent_team: str | None = None
    game_id: str | None = None

    # Passing
    completions: int | None = None
    attempts: int | None = None
    passing_yards: int | None = None
    passing_tds: int | None = None
    passing_interceptions: int | None = None
    sacks_suffered: int | None = None
    sack_yards_lost: int | None = None
    sack_fumbles: int | None = None
    sack_fumbles_lost: int | None = None
    passing_air_yards: int | None = None
    passing_yards_after_catch: int | None = None
    passing_first_downs: int | None = None
    passing_epa: float | None = None
    passing_cpoe: float | None = None
    passing_2pt_conversions: int | None = None
    pacr: float | None = None

    # Rushing
    carries: int | None = None
    rushing_yards: int | None = None
    rushing_tds: int | None = None
    rushing_fumbles: int | None = None
    rushing_fumbles_lost: int | None = None
    rushing_first_downs: int | None = None
    rushing_epa: float | None = None
    rushing_2pt_conversions: int | None = None

    # Receiving
    receptions: int | None = None
    targets: int | None = None
    receiving_yards: int | None = None
    receiving_tds: int | None = None
    receiving_fumbles: int | None = None
    receiving_fumbles_lost: int | None = None
    receiving_air_yards: int | None = None
    receiving_yards_after_catch: int | None = None
    receiving_first_downs: int | None = None
    receiving_epa: float | None = None
    receiving_2pt_conversions: int | None = None
    racr: float | None = None
    target_share: float | None = None
    air_yards_share: float | None = None
    wopr: float | None = None

    # Special Teams & Misc
    special_teams_tds: int | None = None
    misc_yards: int | None = None
    penalties: int | None = None
    penalty_yards: int | None = None
    punt_returns: int | None = None
    punt_return_yards: int | None = None
    kickoff_returns: int | None = None
    kickoff_return_yards: int | None = None

    # Kicking
    fg_made: int | None = None
    fg_att: int | None = None
    fg_missed: int | None = None
    fg_blocked: int | None = None
    fg_long: int | None = None
    fg_pct: float | None = None
    pat_made: int | None = None
    pat_att: int | None = None
    pat_missed: int | None = None
    pat_blocked: int | None = None
    pat_pct: float | None = None

    # Fantasy Points
    fantasy_points: float | None = None
    fantasy_points_ppr: float | None = None

    @computed_field  # type: ignore[misc]
    @property
    def fantasy_points_half_ppr(self) -> float | None:
        """Calculate half-PPR fantasy points."""
        if self.fantasy_points is not None and self.fantasy_points_ppr is not None:
            return round((self.fantasy_points + self.fantasy_points_ppr) / 2.0, 2)
        return self.fantasy_points


class NFLSeasonPlayerStats(BaseFootballModel):
    """PostgreSQL table: `nflfastR_player_stats_season` / `nflfastR_player_stats`."""

    player_id: str | None = None
    player_name: str | None = None
    player_display_name: str | None = None
    position: str | None = None
    position_group: str | None = None
    headshot_url: str | None = None
    season: int | None = None
    season_type: str | None = None
    recent_team: str | None = None
    games: int | None = None

    # Passing Totals
    completions: int | None = None
    attempts: int | None = None
    passing_yards: int | None = None
    passing_tds: int | None = None
    passing_interceptions: int | None = None
    sacks_suffered: int | None = None
    sack_yards_lost: int | None = None
    passing_air_yards: int | None = None
    passing_yards_after_catch: int | None = None
    passing_first_downs: int | None = None
    passing_epa: float | None = None
    passing_cpoe: float | None = None
    pacr: float | None = None

    # Rushing Totals
    carries: int | None = None
    rushing_yards: int | None = None
    rushing_tds: int | None = None
    rushing_fumbles: int | None = None
    rushing_fumbles_lost: int | None = None
    rushing_first_downs: int | None = None
    rushing_epa: float | None = None

    # Receiving Totals
    receptions: int | None = None
    targets: int | None = None
    receiving_yards: int | None = None
    receiving_tds: int | None = None
    receiving_fumbles: int | None = None
    receiving_fumbles_lost: int | None = None
    receiving_air_yards: int | None = None
    receiving_yards_after_catch: int | None = None
    receiving_first_downs: int | None = None
    receiving_epa: float | None = None
    racr: float | None = None
    target_share: float | None = None
    air_yards_share: float | None = None
    wopr: float | None = None

    # Fantasy Points Totals
    fantasy_points: float | None = None
    fantasy_points_ppr: float | None = None

    @computed_field  # type: ignore[misc]
    @property
    def fantasy_points_half_ppr(self) -> float | None:
        """Calculate half-PPR fantasy points."""
        if self.fantasy_points is not None and self.fantasy_points_ppr is not None:
            return round((self.fantasy_points + self.fantasy_points_ppr) / 2.0, 2)
        return self.fantasy_points


class NFLSchedule(BaseFootballModel):
    """PostgreSQL table: `nflfastR_schedule`."""

    game_id: str | None = None
    season: int | None = None
    game_type: str | None = None
    week: int | None = None
    gameday: str | None = None
    weekday: str | None = None
    gametime: str | None = None
    away_team: str | None = None
    away_score: int | None = None
    home_team: str | None = None
    home_score: int | None = None
    location: str | None = None
    result: int | None = None
    total: int | None = None
    overtime: int | None = None
    old_game_id: str | None = None
    gsis: int | None = None
    nfl_detail_id: str | None = None
    pfr: str | None = None
    pff: int | None = None
    espn: str | None = None
    ftn: int | None = None
    away_rest: int | None = None
    home_rest: int | None = None
    away_moneyline: int | None = None
    home_moneyline: int | None = None
    spread_line: float | None = None
    away_spread_odds: int | None = None
    home_spread_odds: int | None = None
    total_line: float | None = None
    under_odds: int | None = None
    over_odds: int | None = None
    div_game: int | None = None
    roof: str | None = None
    surface: str | None = None
    temp: int | None = None
    wind: int | None = None
    away_qb_id: str | None = None
    home_qb_id: str | None = None
    away_qb_name: str | None = None
    home_qb_name: str | None = None
    away_coach: str | None = None
    home_coach: str | None = None
    referee: str | None = None
    stadium_id: str | None = None
    stadium: str | None = None


class NFLRoster(BaseFootballModel):
    """PostgreSQL table: `nflfastR_rosters`."""

    season: int | None = None
    team: str | None = None
    position: str | None = None
    depth_chart_position: str | None = None
    jersey_number: str | None = None
    status: str | None = None
    full_name: str | None = None
    first_name: str | None = None
    last_name: str | None = None
    birth_date: dt_date | None = None
    height: float | None = None
    weight: int | None = None
    college: str | None = None
    gsis_id: str | None = None
    espn_id: str | None = None
    sportradar_id: str | None = None
    yahoo_id: str | None = None
    rotowire_id: str | None = None
    pff_id: str | None = None
    pfr_id: str | None = None
    fantasy_data_id: str | None = None
    sleeper_id: str | None = None
    years_exp: int | None = None
    headshot_url: str | None = None
    esb_id: str | None = None
    gsis_it_id: str | None = None
    smart_id: str | None = None
    entry_year: int | None = None
    rookie_year: int | None = None
    draft_club: str | None = None
    ngs_position: str | None = None
    week: int | None = None
    game_type: str | None = None
    status_description_abbr: str | None = None
    football_name: str | None = None
    draft_number: str | None = None


class NFLDepthChart(BaseFootballModel):
    """PostgreSQL table: `nflfastR_depthcharts`."""

    season: int | None = None
    club_code: str | None = None
    week: int | None = None
    game_type: str | None = None
    depth_team: str | None = None
    last_name: str | None = None
    first_name: str | None = None
    football_name: str | None = None
    formation: str | None = None
    gsis_id: str | None = None
    jersey_number: str | None = None
    position: str | None = None
    elias_id: str | None = None
    depth_position: str | None = None
    full_name: str | None = None
    dt: str | None = None
    team: str | None = None
    player_name: str | None = None
    espn_id: str | None = None
    pos_grp_id: str | None = None
    pos_grp: str | None = None
    pos_id: str | None = None
    pos_name: str | None = None
    pos_abb: str | None = None
    pos_slot: int | None = None
    pos_rank: int | None = None


class NFLInjury(BaseFootballModel):
    """PostgreSQL table: `nflfastR_injuries`."""

    season: int | None = None
    season_type: str | None = None
    game_type: str | None = None
    team: str | None = None
    week: int | None = None
    gsis_id: str | None = None
    position: str | None = None
    full_name: str | None = None
    first_name: str | None = None
    last_name: str | None = None
    report_primary_injury: str | None = None
    report_secondary_injury: str | None = None
    report_status: str | None = None
    practice_primary_injury: str | None = None
    practice_secondary_injury: str | None = None
    practice_status: str | None = None


class NFLContract(BaseFootballModel):
    """PostgreSQL table: `nflfastR_contracts`."""

    player: str | None = None
    position: str | None = None
    team: str | None = None
    is_active: bool | None = None
    year_signed: int | None = None
    years: int | None = None
    value: float | None = None
    apy: float | None = None
    guaranteed: float | None = None
    apy_cap_pct: float | None = None
    inflated_value: float | None = None
    inflated_apy: float | None = None
    inflated_guaranteed: float | None = None
    player_page: str | None = None
    otc_id: int | None = None
    gsis_id: str | None = None
    date_of_birth: str | None = None
    height: str | None = None
    weight: str | None = None
    college: str | None = None
    draft_year: int | None = None
    draft_round: int | None = None
    draft_overall: int | None = None
    draft_team: str | None = None
    detail_year: str | None = None
    detail_team: str | None = None
    detail_base_salary: float | None = None
    detail_prorated_bonus: float | None = None
    detail_roster_bonus: float | None = None
    detail_guaranteed_salary: float | None = None
    detail_cap_number: float | None = None
    detail_cap_percent: float | None = None
    detail_cash_paid: float | None = None
    detail_workout_bonus: float | None = None
    detail_other_bonus: float | None = None
    detail_per_game_roster_bonus: float | None = None
    detail_option_bonus: float | None = None


class NFLDraftPick(BaseFootballModel):
    """PostgreSQL table: `nflfastR_draft`."""

    season: int | None = None
    round: int | None = None
    pick: int | None = None
    team: str | None = None
    gsis_id: str | None = None
    pfr_player_id: str | None = None
    cfb_player_id: str | None = None
    pfr_player_name: str | None = None
    hof: bool | None = None
    position: str | None = None
    category: str | None = None
    side: str | None = None
    college: str | None = None
    age: int | None = None
    to: int | None = None
    allpro: int | None = None
    probowls: int | None = None
    seasons_started: int | None = None
    w_av: int | None = None
    car_av: bool | None = None
    dr_av: int | None = None
    games: int | None = None


class NFLCombine(BaseFootballModel):
    """PostgreSQL table: `nflfastR_combine`."""

    season: int | None = None
    draft_year: float | None = None
    draft_team: str | None = None
    draft_round: float | None = None
    draft_ovr: float | None = None
    pfr_id: str | None = None
    cfb_id: str | None = None
    player_name: str | None = None
    pos: str | None = None
    school: str | None = None
    ht: str | None = None
    wt: float | None = None
    forty: float | None = None
    bench: float | None = None
    vertical: float | None = None
    broad_jump: float | None = None
    cone: float | None = None
    shuttle: float | None = None


class NFLTrade(BaseFootballModel):
    """PostgreSQL table: `nflfastR_trades`."""

    trade_id: float | None = None
    season: float | None = None
    trade_date: dt_date | None = None
    gave: str | None = None
    received: str | None = None
    pick_season: float | None = None
    pick_round: float | None = None
    pick_number: float | None = None
    conditional: float | None = None
    pfr_id: str | None = None
    pfr_name: str | None = None


class NFLOfficial(BaseFootballModel):
    """PostgreSQL table: `nflfastR_officials`."""

    game_id: str | None = None
    game_key: str | None = None
    official_name: str | None = None
    position: str | None = None
    jersey_number: int | None = None
    official_id: str | None = None
    season: int | None = None
    season_type: str | None = None
    week: int | None = None


class NFLTeamLogo(BaseFootballModel):
    """PostgreSQL table: `nflfastR_logos`."""

    team_abbr: str | None = None
    team_name: str | None = None
    team_id: str | None = None
    team_nick: str | None = None
    team_conf: str | None = None
    team_division: str | None = None
    team_color: str | None = None
    team_color2: str | None = None
    team_color3: str | None = None
    team_color4: str | None = None
    team_logo_wikipedia: str | None = None
    team_logo_espn: str | None = None
    team_wordmark: str | None = None
    team_conference_logo: str | None = None
    team_league_logo: str | None = None


class NFLWeeklyTeamStats(BaseFootballModel):
    """PostgreSQL table: `nflfastR_team_stats_week` / `nflfastR_team_stats_weekly`."""

    season: int | None = None
    week: int | None = None
    team: str | None = None
    season_type: str | None = None
    opponent_team: str | None = None
    game_id: str | None = None
    completions: int | None = None
    attempts: int | None = None
    passing_yards: float | None = None
    passing_tds: int | None = None
    interceptions: float | None = None
    sacks: float | None = None
    sack_yards: float | None = None
    carries: int | None = None
    rushing_yards: float | None = None
    rushing_tds: int | None = None
    receptions: int | None = None
    targets: int | None = None
    receiving_yards: float | None = None
    receiving_tds: int | None = None


class NFLSeasonTeamStats(BaseFootballModel):
    """PostgreSQL table: `nflfastR_team_stats_season` / `nflfastR_team_stats`."""

    season: int | None = None
    team: str | None = None
    season_type: str | None = None
    games: int | None = None
    completions: int | None = None
    attempts: int | None = None
    passing_yards: float | None = None
    passing_tds: int | None = None
    interceptions: float | None = None
    sacks: float | None = None
    sack_yards: float | None = None
    carries: int | None = None
    rushing_yards: float | None = None
    rushing_tds: int | None = None


class NFLParticipation(BaseFootballModel):
    """PostgreSQL table: `nflfastR_participation`."""

    nflverse_game_id: str | None = None
    old_game_id: str | None = None
    play_id: float | None = None
    possession_team: str | None = None
    offense_formation: str | None = None
    offense_personnel: str | None = None
    defenders_in_box: int | None = None
    defense_personnel: str | None = None
    number_of_pass_rushers: int | None = None
    players_on_play: str | None = None
    offense_players: str | None = None
    defense_players: str | None = None
    n_offense: int | None = None
    n_defense: int | None = None
    ngs_air_yards: float | None = None
    time_to_throw: float | None = None
    was_pressure: bool | None = None
    route: str | None = None
    defense_man_zone_type: str | None = None
    defense_coverage_type: str | None = None


class NFLFTN(BaseFootballModel):
    """PostgreSQL table: `nflfastR_ftn`."""

    ftn_game_id: int | None = None
    nflverse_game_id: str | None = None
    season: int | None = None
    week: int | None = None
    ftn_play_id: int | None = None
    nflverse_play_id: int | None = None
    starting_hash: str | None = None
    qb_location: str | None = None
    n_offense_backfield: int | None = None
    n_defense_box: int | None = None
    is_no_huddle: bool | None = None
    is_motion: bool | None = None
    is_play_action: bool | None = None
    is_screen_pass: bool | None = None
    is_rpo: bool | None = None
    is_trick_play: bool | None = None
    is_qb_out_of_pocket: bool | None = None
    is_interception_worthy: bool | None = None
    is_throw_away: bool | None = None
    read_thrown: str | None = None
    is_catchable_ball: bool | None = None
    is_contested_ball: bool | None = None
    is_created_reception: bool | None = None
    is_drop: bool | None = None
    is_qb_sneak: bool | None = None
    n_blitzers: int | None = None
    n_pass_rushers: int | None = None
    is_qb_fault_sack: bool | None = None
    date_pulled: dt_datetime | None = None
