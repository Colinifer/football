from __future__ import annotations

from datetime import date
from typing import Optional, Union
from pydantic import BaseModel, ConfigDict, Field


class PlayByPlay(BaseModel):
    """
    Pydantic model representing an NFL play-by-play event record
    (based on nflfastR / nflverse schema specifications).
    """

    model_config = ConfigDict(
        populate_by_name=True,
        extra="ignore",
    )

    # 1-13: Identifiers and Game Context
    play_id: Optional[float] = Field(
        default=None,
        description="Numeric play id that when used with game_id and drive provides the unique identifier for a single play.",
    )
    game_id: Optional[str] = Field(
        default=None,
        description="Ten digit identifier for NFL game.",
    )
    old_game_id: Optional[Union[str, int]] = Field(
        default=None,
        description="Legacy NFL game ID.",
    )
    home_team: Optional[str] = Field(
        default=None,
        description="String abbreviation for the home team.",
    )
    away_team: Optional[str] = Field(
        default=None,
        description="String abbreviation for the away team.",
    )
    season_type: Optional[str] = Field(
        default=None,
        description="'REG' or 'POST' indicating if the game belongs to regular or post season.",
    )
    week: Optional[int] = Field(
        default=None,
        description="Season week.",
    )
    posteam: Optional[str] = Field(
        default=None,
        description="String abbreviation for the team with possession.",
    )
    posteam_type: Optional[str] = Field(
        default=None,
        description="String indicating whether the posteam team is home or away.",
    )
    defteam: Optional[str] = Field(
        default=None,
        description="String abbreviation for the team on defense.",
    )
    side_of_field: Optional[str] = Field(
        default=None,
        description="String abbreviation for which team's side of the field the team with possession is currently on.",
    )
    yardline_100: Optional[float] = Field(
        default=None,
        description="Numeric distance in the number of yards from the opponent's endzone for the posteam.",
    )
    game_date: Optional[Union[date, str]] = Field(
        default=None,
        description="Date of the game.",
    )

    # 14-27: Clock and Down/Distance Situation
    quarter_seconds_remaining: Optional[float] = Field(
        default=None,
        description="Numeric seconds remaining in the quarter.",
    )
    half_seconds_remaining: Optional[float] = Field(
        default=None,
        description="Numeric seconds remaining in the half.",
    )
    game_seconds_remaining: Optional[float] = Field(
        default=None,
        description="Numeric seconds remaining in the game.",
    )
    game_half: Optional[str] = Field(
        default=None,
        description="String indicating which half the play is in, either Half1, Half2, or Overtime.",
    )
    quarter_end: Optional[float] = Field(
        default=None,
        description="Binary indicator for whether or not the row of the data is marking the end of a quarter.",
    )
    drive: Optional[float] = Field(
        default=None,
        description="Numeric drive number in the game.",
    )
    sp: Optional[float] = Field(
        default=None,
        description="Binary indicator for whether or not a score occurred on the play.",
    )
    qtr: Optional[float] = Field(
        default=None,
        description="Quarter of the game (5 is overtime).",
    )
    down: Optional[float] = Field(
        default=None,
        description="The down for the given play.",
    )
    goal_to_go: Optional[float] = Field(
        default=None,
        description="Binary indicator for whether or not the posteam is in a goal down situation.",
    )
    time: Optional[str] = Field(
        default=None,
        description="Time at start of play provided in string format as minutes:seconds remaining in the quarter.",
    )
    yrdln: Optional[str] = Field(
        default=None,
        description="String indicating the current field position for a given play.",
    )
    ydstogo: Optional[float] = Field(
        default=None,
        description="Numeric yards in distance from either the first down marker or the endzone in goal down situations.",
    )
    ydsnet: Optional[float] = Field(
        default=None,
        description="Numeric value for total yards gained on the given drive.",
    )

    # 28-46: Play Description and Detailed Play Attributes
    desc: Optional[str] = Field(
        default=None,
        description="Detailed string description for the given play.",
    )
    play_type: Optional[str] = Field(
        default=None,
        description="String indicating the type of play: pass, run, punt, field_goal, kickoff, extra_point, qb_kneel, qb_spike, no_play, etc.",
    )
    yards_gained: Optional[float] = Field(
        default=None,
        description="Numeric yards gained (or lost) by the possessing team, excluding yards gained via fumble recoveries and laterals.",
    )
    shotgun: Optional[float] = Field(
        default=None,
        description="Binary indicator for whether or not the play was in shotgun formation.",
    )
    no_huddle: Optional[float] = Field(
        default=None,
        description="Binary indicator for whether or not the play was in no_huddle formation.",
    )
    qb_dropback: Optional[float] = Field(
        default=None,
        description="Binary indicator for whether or not the QB dropped back on the play (pass attempt, sack, or scrambled).",
    )
    qb_kneel: Optional[float] = Field(
        default=None,
        description="Binary indicator for whether or not the QB took a knee.",
    )
    qb_spike: Optional[float] = Field(
        default=None,
        description="Binary indicator for whether or not the QB spiked the ball.",
    )
    qb_scramble: Optional[float] = Field(
        default=None,
        description="Binary indicator for whether or not the QB scrambled.",
    )
    pass_length: Optional[str] = Field(
        default=None,
        description="String indicator for pass length: short or deep.",
    )
    pass_location: Optional[str] = Field(
        default=None,
        description="String indicator for pass location: left, middle, or right.",
    )
    air_yards: Optional[float] = Field(
        default=None,
        description="Numeric value for distance in yards perpendicular to the line of scrimmage at where the targeted receiver either caught or didn't catch the ball.",
    )
    yards_after_catch: Optional[float] = Field(
        default=None,
        description="Numeric value for distance in yards perpendicular to the yard line where the receiver made the reception to where the play ended.",
    )
    run_location: Optional[str] = Field(
        default=None,
        description="String indicator for location of run: left, middle, or right.",
    )
    run_gap: Optional[str] = Field(
        default=None,
        description="String indicator for line gap of run: end, guard, or tackle.",
    )
    field_goal_result: Optional[str] = Field(
        default=None,
        description="String indicator for result of field goal attempt: made, missed, or blocked.",
    )
    kick_distance: Optional[float] = Field(
        default=None,
        description="Numeric distance in yards for kickoffs, field goals, and punts.",
    )
    extra_point_result: Optional[str] = Field(
        default=None,
        description="String indicator for the result of the extra point attempt: good, failed, blocked, safety, or aborted.",
    )
    two_point_conv_result: Optional[str] = Field(
        default=None,
        description="String indicator for result of two point conversion attempt: success, failure, safety, or return.",
    )

    # 47-63: Timeouts and Score State
    home_timeouts_remaining: Optional[float] = Field(
        default=None,
        description="Numeric timeouts remaining in the half for the home team.",
    )
    away_timeouts_remaining: Optional[float] = Field(
        default=None,
        description="Numeric timeouts remaining in the half for the away team.",
    )
    timeout: Optional[float] = Field(
        default=None,
        description="Binary indicator for whether or not a timeout was called by either team.",
    )
    timeout_team: Optional[str] = Field(
        default=None,
        description="String abbreviation for which team called the timeout.",
    )
    td_team: Optional[str] = Field(
        default=None,
        description="String abbreviation for which team scored the touchdown.",
    )
    td_player_name: Optional[str] = Field(
        default=None,
        description="String name of the player who scored a touchdown.",
    )
    td_player_id: Optional[str] = Field(
        default=None,
        description="Unique identifier of the player who scored a touchdown.",
    )
    posteam_timeouts_remaining: Optional[float] = Field(
        default=None,
        description="Number of timeouts remaining for the possession team.",
    )
    defteam_timeouts_remaining: Optional[float] = Field(
        default=None,
        description="Number of timeouts remaining for the team on defense.",
    )
    total_home_score: Optional[float] = Field(
        default=None,
        description="Score for the home team at the end of the play.",
    )
    total_away_score: Optional[float] = Field(
        default=None,
        description="Score for the away team at the end of the play.",
    )
    posteam_score: Optional[float] = Field(
        default=None,
        description="Score of the posteam at the start of the play.",
    )
    defteam_score: Optional[float] = Field(
        default=None,
        description="Score of the defteam at the start of the play.",
    )
    score_differential: Optional[float] = Field(
        default=None,
        description="Score differential between the posteam and defteam at the start of the play.",
    )
    posteam_score_post: Optional[float] = Field(
        default=None,
        description="Score for the posteam at the end of the play.",
    )
    defteam_score_post: Optional[float] = Field(
        default=None,
        description="Score for the defteam at the end of the play.",
    )
    score_differential_post: Optional[float] = Field(
        default=None,
        description="Score differential between the posteam and defteam at the end of the play.",
    )

    # 64-92: Expected Points Probabilities and EPA Metrics
    no_score_prob: Optional[float] = Field(
        default=None,
        description="Predicted probability of no score occurring for the rest of the half based on the expected points model.",
    )
    opp_fg_prob: Optional[float] = Field(
        default=None,
        description="Predicted probability of the defteam scoring a FG next.",
    )
    opp_safety_prob: Optional[float] = Field(
        default=None,
        description="Predicted probability of the defteam scoring a safety next.",
    )
    opp_td_prob: Optional[float] = Field(
        default=None,
        description="Predicted probability of the defteam scoring a TD next.",
    )
    fg_prob: Optional[float] = Field(
        default=None,
        description="Predicted probability of the posteam scoring a FG next.",
    )
    safety_prob: Optional[float] = Field(
        default=None,
        description="Predicted probability of the posteam scoring a safety next.",
    )
    td_prob: Optional[float] = Field(
        default=None,
        description="Predicted probability of the posteam scoring a TD next.",
    )
    extra_point_prob: Optional[float] = Field(
        default=None,
        description="Predicted probability of the posteam scoring an extra point.",
    )
    two_point_conversion_prob: Optional[float] = Field(
        default=None,
        description="Predicted probability of the posteam scoring the two point conversion.",
    )
    ep: Optional[float] = Field(
        default=None,
        description="Using the scoring event probabilities, the estimated expected points with respect to the possession team for the given play.",
    )
    epa: Optional[float] = Field(
        default=None,
        description="Expected points added (EPA) by the posteam for the given play.",
    )
    total_home_epa: Optional[float] = Field(
        default=None,
        description="Cumulative total EPA for the home team in the game so far.",
    )
    total_away_epa: Optional[float] = Field(
        default=None,
        description="Cumulative total EPA for the away team in the game so far.",
    )
    total_home_rush_epa: Optional[float] = Field(
        default=None,
        description="Cumulative total rushing EPA for the home team in the game so far.",
    )
    total_away_rush_epa: Optional[float] = Field(
        default=None,
        description="Cumulative total rushing EPA for the away team in the game so far.",
    )
    total_home_pass_epa: Optional[float] = Field(
        default=None,
        description="Cumulative total passing EPA for the home team in the game so far.",
    )
    total_away_pass_epa: Optional[float] = Field(
        default=None,
        description="Cumulative total passing EPA for the away team in the game so far.",
    )
    air_epa: Optional[float] = Field(
        default=None,
        description="EPA from the air yards alone.",
    )
    yac_epa: Optional[float] = Field(
        default=None,
        description="EPA from the yards after catch alone.",
    )
    comp_air_epa: Optional[float] = Field(
        default=None,
        description="EPA from the air yards alone only for completions.",
    )
    comp_yac_epa: Optional[float] = Field(
        default=None,
        description="EPA from the yards after catch alone only for completions.",
    )
    total_home_comp_air_epa: Optional[float] = Field(
        default=None,
        description="Cumulative total completions air EPA for the home team in the game so far.",
    )
    total_away_comp_air_epa: Optional[float] = Field(
        default=None,
        description="Cumulative total completions air EPA for the away team in the game so far.",
    )
    total_home_comp_yac_epa: Optional[float] = Field(
        default=None,
        description="Cumulative total completions yac EPA for the home team in the game so far.",
    )
    total_away_comp_yac_epa: Optional[float] = Field(
        default=None,
        description="Cumulative total completions yac EPA for the away team in the game so far.",
    )
    total_home_raw_air_epa: Optional[float] = Field(
        default=None,
        description="Cumulative total raw air EPA for the home team in the game so far.",
    )
    total_away_raw_air_epa: Optional[float] = Field(
        default=None,
        description="Cumulative total raw air EPA for the away team in the game so far.",
    )
    total_home_raw_yac_epa: Optional[float] = Field(
        default=None,
        description="Cumulative total raw yac EPA for the home team in the game so far.",
    )
    total_away_raw_yac_epa: Optional[float] = Field(
        default=None,
        description="Cumulative total raw yac EPA for the away team in the game so far.",
    )

    # 93-119: Win Probability and WPA Metrics
    wp: Optional[float] = Field(
        default=None,
        description="Estimated win probability for the posteam given the current situation at the start of the given play.",
    )
    def_wp: Optional[float] = Field(
        default=None,
        description="Estimated win probability for the defteam.",
    )
    home_wp: Optional[float] = Field(
        default=None,
        description="Estimated win probability for the home team.",
    )
    away_wp: Optional[float] = Field(
        default=None,
        description="Estimated win probability for the away team.",
    )
    wpa: Optional[float] = Field(
        default=None,
        description="Win probability added (WPA) for the posteam.",
    )
    vegas_wpa: Optional[float] = Field(
        default=None,
        description="Win probability added (WPA) for the posteam: spread_adjusted model.",
    )
    vegas_home_wpa: Optional[float] = Field(
        default=None,
        description="Win probability added (WPA) for the home team: spread_adjusted model.",
    )
    home_wp_post: Optional[float] = Field(
        default=None,
        description="Estimated win probability for the home team at the end of the play.",
    )
    away_wp_post: Optional[float] = Field(
        default=None,
        description="Estimated win probability for the away team at the end of the play.",
    )
    vegas_wp: Optional[float] = Field(
        default=None,
        description="Estimated win probability for the posteam given the current situation at the start of the given play, incorporating pre-game Vegas line.",
    )
    vegas_home_wp: Optional[float] = Field(
        default=None,
        description="Estimated win probability for the home team incorporating pre-game Vegas line.",
    )
    total_home_rush_wpa: Optional[float] = Field(
        default=None,
        description="Cumulative total rushing WPA for the home team in the game so far.",
    )
    total_away_rush_wpa: Optional[float] = Field(
        default=None,
        description="Cumulative total rushing WPA for the away team in the game so far.",
    )
    total_home_pass_wpa: Optional[float] = Field(
        default=None,
        description="Cumulative total passing WPA for the home team in the game so far.",
    )
    total_away_pass_wpa: Optional[float] = Field(
        default=None,
        description="Cumulative total passing WPA for the away team in the game so far.",
    )
    air_wpa: Optional[float] = Field(
        default=None,
        description="WPA through the air (same logic as air_epa).",
    )
    yac_wpa: Optional[float] = Field(
        default=None,
        description="WPA from yards after the catch (same logic as yac_epa).",
    )
    comp_air_wpa: Optional[float] = Field(
        default=None,
        description="The air_wpa for completions only.",
    )
    comp_yac_wpa: Optional[float] = Field(
        default=None,
        description="The yac_wpa for completions only.",
    )
    total_home_comp_air_wpa: Optional[float] = Field(
        default=None,
        description="Cumulative total completions air WPA for the home team in the game so far.",
    )
    total_away_comp_air_wpa: Optional[float] = Field(
        default=None,
        description="Cumulative total completions air WPA for the away team in the game so far.",
    )
    total_home_comp_yac_wpa: Optional[float] = Field(
        default=None,
        description="Cumulative total completions yac WPA for the home team in the game so far.",
    )
    total_away_comp_yac_wpa: Optional[float] = Field(
        default=None,
        description="Cumulative total completions yac WPA for the away team in the game so far.",
    )
    total_home_raw_air_wpa: Optional[float] = Field(
        default=None,
        description="Cumulative total raw air WPA for the home team in the game so far.",
    )
    total_away_raw_air_wpa: Optional[float] = Field(
        default=None,
        description="Cumulative total raw air WPA for the away team in the game so far.",
    )
    total_home_raw_yac_wpa: Optional[float] = Field(
        default=None,
        description="Cumulative total raw yac WPA for the home team in the game so far.",
    )
    total_away_raw_yac_wpa: Optional[float] = Field(
        default=None,
        description="Cumulative total raw yac WPA for the away team in the game so far.",
    )

    # 120-170: Binary Indicator Flags for Play Events
    punt_blocked: Optional[float] = Field(
        default=None,
        description="Binary indicator for if the punt was blocked.",
    )
    first_down_rush: Optional[float] = Field(
        default=None,
        description="Binary indicator for if a running play converted the first down.",
    )
    first_down_pass: Optional[float] = Field(
        default=None,
        description="Binary indicator for if a passing play converted the first down.",
    )
    first_down_penalty: Optional[float] = Field(
        default=None,
        description="Binary indicator for if a penalty converted the first down.",
    )
    third_down_converted: Optional[float] = Field(
        default=None,
        description="Binary indicator for if the first down was converted on third down.",
    )
    third_down_failed: Optional[float] = Field(
        default=None,
        description="Binary indicator for if the posteam failed to convert first down on third down.",
    )
    fourth_down_converted: Optional[float] = Field(
        default=None,
        description="Binary indicator for if the first down was converted on fourth down.",
    )
    fourth_down_failed: Optional[float] = Field(
        default=None,
        description="Binary indicator for if the posteam failed to convert first down on fourth down.",
    )
    incomplete_pass: Optional[float] = Field(
        default=None,
        description="Binary indicator for if the pass was incomplete.",
    )
    touchback: Optional[float] = Field(
        default=None,
        description="Binary indicator for if a touchback occurred on the play.",
    )
    interception: Optional[float] = Field(
        default=None,
        description="Binary indicator for if the pass was intercepted.",
    )
    punt_inside_twenty: Optional[float] = Field(
        default=None,
        description="Binary indicator for if the punt ended inside the twenty yard line.",
    )
    punt_in_endzone: Optional[float] = Field(
        default=None,
        description="Binary indicator for if the punt was in the endzone.",
    )
    punt_out_of_bounds: Optional[float] = Field(
        default=None,
        description="Binary indicator for if the punt went out of bounds.",
    )
    punt_downed: Optional[float] = Field(
        default=None,
        description="Binary indicator for if the punt was downed.",
    )
    punt_fair_catch: Optional[float] = Field(
        default=None,
        description="Binary indicator for if the punt was caught with a fair catch.",
    )
    kickoff_inside_twenty: Optional[float] = Field(
        default=None,
        description="Binary indicator for if the kickoff ended inside the twenty yard line.",
    )
    kickoff_in_endzone: Optional[float] = Field(
        default=None,
        description="Binary indicator for if the kickoff was in the endzone.",
    )
    kickoff_out_of_bounds: Optional[float] = Field(
        default=None,
        description="Binary indicator for if the kickoff went out of bounds.",
    )
    kickoff_downed: Optional[float] = Field(
        default=None,
        description="Binary indicator for if the kickoff was downed.",
    )
    kickoff_fair_catch: Optional[float] = Field(
        default=None,
        description="Binary indicator for if the kickoff was caught with a fair catch.",
    )
    fumble_forced: Optional[float] = Field(
        default=None,
        description="Binary indicator for if the fumble was forced.",
    )
    fumble_not_forced: Optional[float] = Field(
        default=None,
        description="Binary indicator for if the fumble was not forced.",
    )
    fumble_out_of_bounds: Optional[float] = Field(
        default=None,
        description="Binary indicator for if the fumble went out of bounds.",
    )
    solo_tackle: Optional[float] = Field(
        default=None,
        description="Binary indicator if the play had a solo tackle.",
    )
    safety: Optional[float] = Field(
        default=None,
        description="Binary indicator for whether or not a safety occurred.",
    )
    penalty: Optional[float] = Field(
        default=None,
        description="Binary indicator for whether or not a penalty occurred.",
    )
    tackled_for_loss: Optional[float] = Field(
        default=None,
        description="Binary indicator for whether or not a tackle for loss on a run play occurred.",
    )
    fumble_lost: Optional[float] = Field(
        default=None,
        description="Binary indicator for if the fumble was lost.",
    )
    own_kickoff_recovery: Optional[float] = Field(
        default=None,
        description="Binary indicator for if the kicking team recovered the kickoff.",
    )
    own_kickoff_recovery_td: Optional[float] = Field(
        default=None,
        description="Binary indicator for if the kicking team recovered the kickoff and scored a TD.",
    )
    qb_hit: Optional[float] = Field(
        default=None,
        description="Binary indicator if the QB was hit on the play.",
    )
    rush_attempt: Optional[float] = Field(
        default=None,
        description="Binary indicator for if the play was a run.",
    )
    pass_attempt: Optional[float] = Field(
        default=None,
        description="Binary indicator for if the play was a pass attempt (includes sacks).",
    )
    sack: Optional[float] = Field(
        default=None,
        description="Binary indicator for if the play ended in a sack.",
    )
    touchdown: Optional[float] = Field(
        default=None,
        description="Binary indicator for if the play resulted in a TD.",
    )
    pass_touchdown: Optional[float] = Field(
        default=None,
        description="Binary indicator for if the play resulted in a passing TD.",
    )
    rush_touchdown: Optional[float] = Field(
        default=None,
        description="Binary indicator for if the play resulted in a rushing TD.",
    )
    return_touchdown: Optional[float] = Field(
        default=None,
        description="Binary indicator for if the play resulted in a return TD.",
    )
    extra_point_attempt: Optional[float] = Field(
        default=None,
        description="Binary indicator for extra point attempt.",
    )
    two_point_attempt: Optional[float] = Field(
        default=None,
        description="Binary indicator for two point conversion attempt.",
    )
    field_goal_attempt: Optional[float] = Field(
        default=None,
        description="Binary indicator for field goal attempt.",
    )
    kickoff_attempt: Optional[float] = Field(
        default=None,
        description="Binary indicator for kickoff.",
    )
    punt_attempt: Optional[float] = Field(
        default=None,
        description="Binary indicator for punts.",
    )
    fumble: Optional[float] = Field(
        default=None,
        description="Binary indicator for if a fumble occurred.",
    )
    complete_pass: Optional[float] = Field(
        default=None,
        description="Binary indicator for if the pass was completed.",
    )
    assist_tackle: Optional[float] = Field(
        default=None,
        description="Binary indicator for if an assist tackle occurred.",
    )
    lateral_reception: Optional[float] = Field(
        default=None,
        description="Binary indicator for if a lateral occurred on the reception.",
    )
    lateral_rush: Optional[float] = Field(
        default=None,
        description="Binary indicator for if a lateral occurred on a run.",
    )
    lateral_return: Optional[float] = Field(
        default=None,
        description="Binary indicator for if a lateral occurred on a return.",
    )
    lateral_recovery: Optional[float] = Field(
        default=None,
        description="Binary indicator for if a lateral occurred on a fumble recovery.",
    )

    # 171-207: Primary Player Roles (Passing, Rushing, Receiving, Returns, Kicking)
    passer_player_id: Optional[str] = Field(
        default=None,
        description="Unique identifier for the player that attempted the pass.",
    )
    passer_player_name: Optional[str] = Field(
        default=None,
        description="String name for the player that attempted the pass.",
    )
    passing_yards: Optional[float] = Field(
        default=None,
        description="Numeric yards by the passer_player_name, including yards gained in pass plays with laterals.",
    )
    receiver_player_id: Optional[str] = Field(
        default=None,
        description="Unique identifier for the receiver that was targeted on the pass.",
    )
    receiver_player_name: Optional[str] = Field(
        default=None,
        description="String name for the targeted receiver.",
    )
    receiving_yards: Optional[float] = Field(
        default=None,
        description="Numeric yards by the receiver_player_name, excluding yards gained in pass plays with laterals.",
    )
    rusher_player_id: Optional[str] = Field(
        default=None,
        description="Unique identifier for the player that attempted the run.",
    )
    rusher_player_name: Optional[str] = Field(
        default=None,
        description="String name for the player that attempted the run.",
    )
    rushing_yards: Optional[float] = Field(
        default=None,
        description="Numeric yards by the rusher_player_name, excluding yards gained in rush plays with laterals.",
    )
    lateral_receiver_player_id: Optional[str] = Field(
        default=None,
        description="Unique identifier for the player that received the last lateral on a pass play.",
    )
    lateral_receiver_player_name: Optional[str] = Field(
        default=None,
        description="String name for the player that received the last lateral on a pass play.",
    )
    lateral_receiving_yards: Optional[float] = Field(
        default=None,
        description="Numeric yards by the lateral_receiver_player_name in pass plays with laterals.",
    )
    lateral_rusher_player_id: Optional[str] = Field(
        default=None,
        description="Unique identifier for the player that received the last lateral on a run play.",
    )
    lateral_rusher_player_name: Optional[str] = Field(
        default=None,
        description="String name for the player that received the last lateral on a run play.",
    )
    lateral_rushing_yards: Optional[float] = Field(
        default=None,
        description="Numeric yards by the lateral_rusher_player_name in run plays with laterals.",
    )
    lateral_sack_player_id: Optional[str] = Field(
        default=None,
        description="Unique identifier for the player that received the lateral on a sack.",
    )
    lateral_sack_player_name: Optional[str] = Field(
        default=None,
        description="String name for the player that received the lateral on a sack.",
    )
    interception_player_id: Optional[str] = Field(
        default=None,
        description="Unique identifier for the player that intercepted the pass.",
    )
    interception_player_name: Optional[str] = Field(
        default=None,
        description="String name for the player that intercepted the pass.",
    )
    lateral_interception_player_id: Optional[str] = Field(
        default=None,
        description="Unique identifier for the player that received the lateral on an interception.",
    )
    lateral_interception_player_name: Optional[str] = Field(
        default=None,
        description="String name for the player that received the lateral on an interception.",
    )
    punt_returner_player_id: Optional[str] = Field(
        default=None,
        description="Unique identifier for the punt returner.",
    )
    punt_returner_player_name: Optional[str] = Field(
        default=None,
        description="String name for the punt returner.",
    )
    lateral_punt_returner_player_id: Optional[str] = Field(
        default=None,
        description="Unique identifier for the player that received the lateral on a punt return.",
    )
    lateral_punt_returner_player_name: Optional[str] = Field(
        default=None,
        description="String name for the player that received the lateral on a punt return.",
    )
    kickoff_returner_player_name: Optional[str] = Field(
        default=None,
        description="String name for the kickoff returner.",
    )
    kickoff_returner_player_id: Optional[str] = Field(
        default=None,
        description="Unique identifier for the kickoff returner.",
    )
    lateral_kickoff_returner_player_id: Optional[str] = Field(
        default=None,
        description="Unique identifier for the player that received the lateral on a kickoff return.",
    )
    lateral_kickoff_returner_player_name: Optional[str] = Field(
        default=None,
        description="String name for the player that received the lateral on a kickoff return.",
    )
    punter_player_id: Optional[str] = Field(
        default=None,
        description="Unique identifier for the punter.",
    )
    punter_player_name: Optional[str] = Field(
        default=None,
        description="String name for the punter.",
    )
    kicker_player_name: Optional[str] = Field(
        default=None,
        description="String name for the kicker on FG or kickoff.",
    )
    kicker_player_id: Optional[str] = Field(
        default=None,
        description="Unique identifier for the kicker on FG or kickoff.",
    )
    own_kickoff_recovery_player_id: Optional[str] = Field(
        default=None,
        description="Unique identifier for the player that recovered their own kickoff.",
    )
    own_kickoff_recovery_player_name: Optional[str] = Field(
        default=None,
        description="String name for the player that recovered their own kickoff.",
    )
    blocked_player_id: Optional[str] = Field(
        default=None,
        description="Unique identifier for the player that blocked the punt or FG.",
    )
    blocked_player_name: Optional[str] = Field(
        default=None,
        description="String name for the player that blocked the punt or FG.",
    )

    # 208-270: Defensive Stats (Tackles, Hits, Fumbles, Pass Defenses, Sacks)
    tackle_for_loss_1_player_id: Optional[str] = Field(
        default=None,
        description="Unique identifier for one of the potential players with the tackle for loss.",
    )
    tackle_for_loss_1_player_name: Optional[str] = Field(
        default=None,
        description="String name for one of the potential players with the tackle for loss.",
    )
    tackle_for_loss_2_player_id: Optional[str] = Field(
        default=None,
        description="Unique identifier for one of the potential players with the tackle for loss.",
    )
    tackle_for_loss_2_player_name: Optional[str] = Field(
        default=None,
        description="String name for one of the potential players with the tackle for loss.",
    )
    qb_hit_1_player_id: Optional[str] = Field(
        default=None,
        description="Unique identifier for one of the potential players that hit the QB.",
    )
    qb_hit_1_player_name: Optional[str] = Field(
        default=None,
        description="String name for one of the potential players that hit the QB.",
    )
    qb_hit_2_player_id: Optional[str] = Field(
        default=None,
        description="Unique identifier for one of the potential players that hit the QB.",
    )
    qb_hit_2_player_name: Optional[str] = Field(
        default=None,
        description="String name for one of the potential players that hit the QB.",
    )
    forced_fumble_player_1_team: Optional[str] = Field(
        default=None,
        description="Team of one of the players with a forced fumble.",
    )
    forced_fumble_player_1_player_id: Optional[str] = Field(
        default=None,
        description="Unique identifier of one of the players with a forced fumble.",
    )
    forced_fumble_player_1_player_name: Optional[str] = Field(
        default=None,
        description="String name of one of the players with a forced fumble.",
    )
    forced_fumble_player_2_team: Optional[str] = Field(
        default=None,
        description="Team of one of the players with a forced fumble.",
    )
    forced_fumble_player_2_player_id: Optional[str] = Field(
        default=None,
        description="Unique identifier of one of the players with a forced fumble.",
    )
    forced_fumble_player_2_player_name: Optional[str] = Field(
        default=None,
        description="String name of one of the players with a forced fumble.",
    )
    solo_tackle_1_team: Optional[str] = Field(
        default=None,
        description="Team of one of the players with a solo tackle.",
    )
    solo_tackle_2_team: Optional[str] = Field(
        default=None,
        description="Team of one of the players with a solo tackle.",
    )
    solo_tackle_1_player_id: Optional[str] = Field(
        default=None,
        description="Unique identifier of one of the players with a solo tackle.",
    )
    solo_tackle_2_player_id: Optional[str] = Field(
        default=None,
        description="Unique identifier of one of the players with a solo tackle.",
    )
    solo_tackle_1_player_name: Optional[str] = Field(
        default=None,
        description="String name of one of the players with a solo tackle.",
    )
    solo_tackle_2_player_name: Optional[str] = Field(
        default=None,
        description="String name of one of the players with a solo tackle.",
    )
    assist_tackle_1_player_id: Optional[str] = Field(
        default=None,
        description="Unique identifier of one of the players with a tackle assist.",
    )
    assist_tackle_1_player_name: Optional[str] = Field(
        default=None,
        description="String name of one of the players with a tackle assist.",
    )
    assist_tackle_1_team: Optional[str] = Field(
        default=None,
        description="Team of one of the players with a tackle assist.",
    )
    assist_tackle_2_player_id: Optional[str] = Field(
        default=None,
        description="Unique identifier of one of the players with a tackle assist.",
    )
    assist_tackle_2_player_name: Optional[str] = Field(
        default=None,
        description="String name of one of the players with a tackle assist.",
    )
    assist_tackle_2_team: Optional[str] = Field(
        default=None,
        description="Team of one of the players with a tackle assist.",
    )
    assist_tackle_3_player_id: Optional[str] = Field(
        default=None,
        description="Unique identifier of one of the players with a tackle assist.",
    )
    assist_tackle_3_player_name: Optional[str] = Field(
        default=None,
        description="String name of one of the players with a tackle assist.",
    )
    assist_tackle_3_team: Optional[str] = Field(
        default=None,
        description="Team of one of the players with a tackle assist.",
    )
    assist_tackle_4_player_id: Optional[str] = Field(
        default=None,
        description="Unique identifier of one of the players with a tackle assist.",
    )
    assist_tackle_4_player_name: Optional[str] = Field(
        default=None,
        description="String name of one of the players with a tackle assist.",
    )
    assist_tackle_4_team: Optional[str] = Field(
        default=None,
        description="Team of one of the players with a tackle assist.",
    )
    tackle_with_assist: Optional[float] = Field(
        default=None,
        description="Binary indicator for if there has been a tackle with assist.",
    )
    tackle_with_assist_1_player_id: Optional[str] = Field(
        default=None,
        description="Unique identifier of one of the players with a tackle with assist.",
    )
    tackle_with_assist_1_player_name: Optional[str] = Field(
        default=None,
        description="String name of one of the players with a tackle with assist.",
    )
    tackle_with_assist_1_team: Optional[str] = Field(
        default=None,
        description="Team of one of the players with a tackle with assist.",
    )
    tackle_with_assist_2_player_id: Optional[str] = Field(
        default=None,
        description="Unique identifier of one of the players with a tackle with assist.",
    )
    tackle_with_assist_2_player_name: Optional[str] = Field(
        default=None,
        description="String name of one of the players with a tackle with assist.",
    )
    tackle_with_assist_2_team: Optional[str] = Field(
        default=None,
        description="Team of one of the players with a tackle with assist.",
    )
    pass_defense_1_player_id: Optional[str] = Field(
        default=None,
        description="Unique identifier of one of the players with a pass defense.",
    )
    pass_defense_1_player_name: Optional[str] = Field(
        default=None,
        description="String name of one of the players with a pass defense.",
    )
    pass_defense_2_player_id: Optional[str] = Field(
        default=None,
        description="Unique identifier of one of the players with a pass defense.",
    )
    pass_defense_2_player_name: Optional[str] = Field(
        default=None,
        description="String name of one of the players with a pass defense.",
    )
    fumbled_1_team: Optional[str] = Field(
        default=None,
        description="Team of one of the first player with a fumble.",
    )
    fumbled_1_player_id: Optional[str] = Field(
        default=None,
        description="Unique identifier of the first player who fumbled on the play.",
    )
    fumbled_1_player_name: Optional[str] = Field(
        default=None,
        description="String name of one of the first player who fumbled on the play.",
    )
    fumbled_2_player_id: Optional[str] = Field(
        default=None,
        description="Unique identifier of the second player who fumbled on the play.",
    )
    fumbled_2_player_name: Optional[str] = Field(
        default=None,
        description="String name of one of the second player who fumbled on the play.",
    )
    fumbled_2_team: Optional[str] = Field(
        default=None,
        description="Team of one of the second player with a fumble.",
    )
    fumble_recovery_1_team: Optional[str] = Field(
        default=None,
        description="Team of one of the players with a fumble recovery.",
    )
    fumble_recovery_1_yards: Optional[float] = Field(
        default=None,
        description="Yards gained by one of the players with a fumble recovery.",
    )
    fumble_recovery_1_player_id: Optional[str] = Field(
        default=None,
        description="Unique identifier of one of the players with a fumble recovery.",
    )
    fumble_recovery_1_player_name: Optional[str] = Field(
        default=None,
        description="String name of one of the players with a fumble recovery.",
    )
    fumble_recovery_2_team: Optional[str] = Field(
        default=None,
        description="Team of one of the players with a fumble recovery.",
    )
    fumble_recovery_2_yards: Optional[float] = Field(
        default=None,
        description="Yards gained by one of the players with a fumble recovery.",
    )
    fumble_recovery_2_player_id: Optional[str] = Field(
        default=None,
        description="Unique identifier of one of the players with a fumble recovery.",
    )
    fumble_recovery_2_player_name: Optional[str] = Field(
        default=None,
        description="String name of one of the players with a fumble recovery.",
    )
    sack_player_id: Optional[str] = Field(
        default=None,
        description="Unique identifier of the player who recorded a solo sack.",
    )
    sack_player_name: Optional[str] = Field(
        default=None,
        description="String name of the player who recorded a solo sack.",
    )
    half_sack_1_player_id: Optional[str] = Field(
        default=None,
        description="Unique identifier of the first player who recorded half a sack.",
    )
    half_sack_1_player_name: Optional[str] = Field(
        default=None,
        description="String name of the first player who recorded half a sack.",
    )
    half_sack_2_player_id: Optional[str] = Field(
        default=None,
        description="Unique identifier of the second player who recorded half a sack.",
    )
    half_sack_2_player_name: Optional[str] = Field(
        default=None,
        description="String name of the second player who recorded half a sack.",
    )

    # 271-285: Returns, Penalties, Replays, and Safeties
    return_team: Optional[str] = Field(
        default=None,
        description="String abbreviation of the return team.",
    )
    return_yards: Optional[float] = Field(
        default=None,
        description="Yards gained by the return team.",
    )
    penalty_team: Optional[str] = Field(
        default=None,
        description="String abbreviation of the team with the penalty.",
    )
    penalty_player_id: Optional[str] = Field(
        default=None,
        description="Unique identifier for the player with the penalty.",
    )
    penalty_player_name: Optional[str] = Field(
        default=None,
        description="String name for the player with the penalty.",
    )
    penalty_yards: Optional[float] = Field(
        default=None,
        description="Yards gained (or lost) by the posteam from the penalty.",
    )
    replay_or_challenge: Optional[float] = Field(
        default=None,
        description="Binary indicator for whether or not a replay or challenge.",
    )
    replay_or_challenge_result: Optional[str] = Field(
        default=None,
        description="String indicating the result of the replay or challenge.",
    )
    penalty_type: Optional[str] = Field(
        default=None,
        description="String indicating the penalty type of the first penalty in the given play.",
    )
    defensive_two_point_attempt: Optional[float] = Field(
        default=None,
        description="Binary indicator whether or not the defense was able to have an attempt on a two point conversion.",
    )
    defensive_two_point_conv: Optional[float] = Field(
        default=None,
        description="Binary indicator whether or not the defense successfully scored on the two point conversion.",
    )
    defensive_extra_point_attempt: Optional[float] = Field(
        default=None,
        description="Binary indicator whether or not the defense was able to have an attempt on an extra point attempt.",
    )
    defensive_extra_point_conv: Optional[float] = Field(
        default=None,
        description="Binary indicator whether or not the defense successfully scored on an extra point attempt.",
    )
    safety_player_name: Optional[str] = Field(
        default=None,
        description="String name for the player who scored a safety.",
    )
    safety_player_id: Optional[str] = Field(
        default=None,
        description="Unique identifier for the player who scored a safety.",
    )

    # 286-304: Season, Series, and NFL API Operational Metadata
    season: Optional[int] = Field(
        default=None,
        description="4 digit number indicating to which season the game belongs to.",
    )
    cp: Optional[float] = Field(
        default=None,
        description="Numeric value indicating the probability for a complete pass based on comparable game situations.",
    )
    cpoe: Optional[float] = Field(
        default=None,
        description="Completion percentage over expectation for the pass play.",
    )
    series: Optional[float] = Field(
        default=None,
        description="Starts at 1, each new first down increments.",
    )
    series_success: Optional[float] = Field(
        default=None,
        description="1: scored touchdown, gained enough yards for first down.",
    )
    series_result: Optional[str] = Field(
        default=None,
        description="Possible values: First down, Touchdown, Opp touchdown, Field goal, Missed field goal, Safety, Turnover, Punt, Turnover on downs, QB kneel, End of half.",
    )
    order_sequence: Optional[float] = Field(
        default=None,
        description="Column provided by NFL to fix out-of-order plays.",
    )
    start_time: Optional[str] = Field(
        default=None,
        description="Kickoff time in eastern time zone.",
    )
    time_of_day: Optional[str] = Field(
        default=None,
        description="Time of day of play in UTC 'HH:MM:SS' format.",
    )
    stadium: Optional[str] = Field(
        default=None,
        description="Game site name.",
    )
    weather: Optional[str] = Field(
        default=None,
        description="String describing the weather including temperature, humidity and wind.",
    )
    nfl_api_id: Optional[str] = Field(
        default=None,
        description="UUID of the game in the new NFL API.",
    )
    play_clock: Optional[str] = Field(
        default=None,
        description="Time on the playclock when the ball was snapped.",
    )
    play_deleted: Optional[float] = Field(
        default=None,
        description="Binary indicator for deleted plays.",
    )
    play_type_nfl: Optional[str] = Field(
        default=None,
        description="Play type as listed in the NFL source.",
    )
    special_teams_play: Optional[float] = Field(
        default=None,
        description="Binary indicator for whether play is special teams play from NFL source.",
    )
    st_play_type: Optional[str] = Field(
        default=None,
        description="Type of special teams play from NFL source.",
    )
    end_clock_time: Optional[str] = Field(
        default=None,
        description="Game time at the end of a given play.",
    )
    end_yard_line: Optional[str] = Field(
        default=None,
        description="String indicating the yardline at the end of the given play.",
    )

    # 305-323: Drive Level Aggregates
    fixed_drive: Optional[float] = Field(
        default=None,
        description="Manually created drive number in a game.",
    )
    fixed_drive_result: Optional[str] = Field(
        default=None,
        description="Manually created drive result.",
    )
    drive_real_start_time: Optional[str] = Field(
        default=None,
        description="Local day time when the drive started.",
    )
    drive_play_count: Optional[float] = Field(
        default=None,
        description="Numeric value of how many regular plays happened in a given drive.",
    )
    drive_time_of_possession: Optional[str] = Field(
        default=None,
        description="Time of possession in a given drive.",
    )
    drive_first_downs: Optional[float] = Field(
        default=None,
        description="Number of first downs in a given drive.",
    )
    drive_inside20: Optional[float] = Field(
        default=None,
        description="Binary indicator if the offense was able to get inside the opponents 20 yard line.",
    )
    drive_ended_with_score: Optional[float] = Field(
        default=None,
        description="Binary indicator the drive ended with a score.",
    )
    drive_quarter_start: Optional[float] = Field(
        default=None,
        description="Numeric value indicating in which quarter the given drive has started.",
    )
    drive_quarter_end: Optional[float] = Field(
        default=None,
        description="Numeric value indicating in which quarter the given drive has ended.",
    )
    drive_yards_penalized: Optional[float] = Field(
        default=None,
        description="Numeric value of how many yards the offense gained or lost through penalties in the given drive.",
    )
    drive_start_transition: Optional[str] = Field(
        default=None,
        description="String indicating how the offense got the ball.",
    )
    drive_end_transition: Optional[str] = Field(
        default=None,
        description="String indicating how the offense lost the ball.",
    )
    drive_game_clock_start: Optional[str] = Field(
        default=None,
        description="Game time at the beginning of a given drive.",
    )
    drive_game_clock_end: Optional[str] = Field(
        default=None,
        description="Game time at the end of a given drive.",
    )
    drive_start_yard_line: Optional[str] = Field(
        default=None,
        description="String indicating where a given drive started.",
    )
    drive_end_yard_line: Optional[str] = Field(
        default=None,
        description="String indicating where a given drive ended.",
    )
    drive_play_id_started: Optional[float] = Field(
        default=None,
        description="Play_id of the first play in the given drive.",
    )
    drive_play_id_ended: Optional[float] = Field(
        default=None,
        description="Play_id of the last play in the given drive.",
    )

    # 324-341: Game Totals, Betting Lines, Stadium, and Coaches
    away_score: Optional[float] = Field(
        default=None,
        description="Total points scored by the away team.",
    )
    home_score: Optional[float] = Field(
        default=None,
        description="Total points scored by the home team.",
    )
    location: Optional[str] = Field(
        default=None,
        description="Either 'Home' or 'Neutral' indicating if the home team played at home or at a neutral site.",
    )
    result: Optional[float] = Field(
        default=None,
        description="Equals home_score - away_score.",
    )
    total: Optional[float] = Field(
        default=None,
        description="Equals home_score + away_score.",
    )
    spread_line: Optional[float] = Field(
        default=None,
        description="The closing spread line for the game.",
    )
    total_line: Optional[float] = Field(
        default=None,
        description="The closing total line for the game.",
    )
    div_game: Optional[float] = Field(
        default=None,
        description="Binary indicator for if the given game was a division game.",
    )
    roof: Optional[str] = Field(
        default=None,
        description="One of 'dome', 'outdoors', 'closed', 'open'.",
    )
    surface: Optional[str] = Field(
        default=None,
        description="What type of ground the game was played on.",
    )
    temp: Optional[float] = Field(
        default=None,
        description="The temperature at the stadium.",
    )
    wind: Optional[float] = Field(
        default=None,
        description="The speed of the wind in miles/hour.",
    )
    home_coach: Optional[str] = Field(
        default=None,
        description="First and last name of the home team coach.",
    )
    away_coach: Optional[str] = Field(
        default=None,
        description="First and last name of the away team coach.",
    )
    stadium_id: Optional[str] = Field(
        default=None,
        description="ID of the stadium the game was played in.",
    )
    game_stadium: Optional[str] = Field(
        default=None,
        description="Name of the stadium the game was played in.",
    )
    aborted_play: Optional[float] = Field(
        default=None,
        description="Binary indicator if the play description indicates 'Aborted'.",
    )
    success: Optional[float] = Field(
        default=None,
        description="Binary indicator whether epa > 0 in the given play.",
    )

    # 342-364: Simplified Player Names, Jerseys, Primary Identifiers & Fantasy
    passer: Optional[str] = Field(
        default=None,
        description="Name of the dropback player (scrambles included) including plays with penalties.",
    )
    passer_jersey_number: Optional[Union[float, int, str]] = Field(
        default=None,
        description="Jersey number of the passer.",
    )
    rusher: Optional[str] = Field(
        default=None,
        description="Name of the rusher (no scrambles) including plays with penalties.",
    )
    rusher_jersey_number: Optional[Union[float, int, str]] = Field(
        default=None,
        description="Jersey number of the rusher.",
    )
    receiver: Optional[str] = Field(
        default=None,
        description="Name of the receiver including plays with penalties.",
    )
    receiver_jersey_number: Optional[Union[float, int, str]] = Field(
        default=None,
        description="Jersey number of the receiver.",
    )
    pass_: Optional[float] = Field(
        default=None,
        alias="pass",
        description="Binary indicator if the play was a pass play (sacks and scrambles included).",
    )
    rush: Optional[float] = Field(
        default=None,
        description="Binary indicator if the play was a rushing play.",
    )
    first_down: Optional[float] = Field(
        default=None,
        description="Binary indicator if the play ended in a first down.",
    )
    special: Optional[float] = Field(
        default=None,
        description="Binary indicator if the play was a special teams play.",
    )
    play: Optional[float] = Field(
        default=None,
        description="Binary indicator: 1 if the play was a 'normal' play (including penalties), 0 otherwise.",
    )
    passer_id: Optional[str] = Field(
        default=None,
        description="ID of the player in the 'passer' column.",
    )
    rusher_id: Optional[str] = Field(
        default=None,
        description="ID of the player in the 'rusher' column.",
    )
    receiver_id: Optional[str] = Field(
        default=None,
        description="ID of the player in the 'receiver' column.",
    )
    name: Optional[str] = Field(
        default=None,
        description="Name of the 'passer' if it is not 'NA', or name of the 'rusher' otherwise.",
    )
    jersey_number: Optional[Union[float, int, str]] = Field(
        default=None,
        description="Jersey number of the player listed in the 'name' column.",
    )
    id: Optional[str] = Field(
        default=None,
        description="ID of the player in the 'name' column.",
    )
    fantasy_player_name: Optional[str] = Field(
        default=None,
        description="Name of the rusher on rush plays or receiver on pass plays (from official stats).",
    )
    fantasy_player_id: Optional[str] = Field(
        default=None,
        description="ID of the rusher on rush plays or receiver on pass plays (from official stats).",
    )
    fantasy: Optional[str] = Field(
        default=None,
        description="Name of the rusher on rush plays or receiver on pass plays.",
    )
    fantasy_id: Optional[str] = Field(
        default=None,
        description="ID of the rusher on rush plays or receiver on pass plays.",
    )
    out_of_bounds: Optional[float] = Field(
        default=None,
        description="1 if play description contains ran ob, pushed ob, or sacked ob; 0 otherwise.",
    )
    home_opening_kickoff: Optional[float] = Field(
        default=None,
        description="1 if the home team received the opening kickoff, 0 otherwise.",
    )

    # 365-372: Advanced Expected & Completion Metrics
    qb_epa: Optional[float] = Field(
        default=None,
        description="Gives QB credit for EPA for up to the point where a receiver lost a fumble after a completed catch.",
    )
    xyac_epa: Optional[float] = Field(
        default=None,
        description="Expected value of EPA gained after the catch, starting from where the catch was made.",
    )
    xyac_mean_yardage: Optional[float] = Field(
        default=None,
        description="Average expected yards after the catch based on where the ball was caught.",
    )
    xyac_median_yardage: Optional[float] = Field(
        default=None,
        description="Median expected yards after the catch based on where the ball was caught.",
    )
    xyac_success: Optional[float] = Field(
        default=None,
        description="Probability play earns positive EPA (relative to where play started) based on where ball was caught.",
    )
    xyac_fd: Optional[float] = Field(
        default=None,
        description="Probability play earns a first down based on where the ball was caught.",
    )
    xpass: Optional[float] = Field(
        default=None,
        description="Probability of dropback scaled from 0 to 1.",
    )
    pass_oe: Optional[float] = Field(
        default=None,
        description="Dropback percent over expected on a given play scaled from 0 to 100.",
    )
