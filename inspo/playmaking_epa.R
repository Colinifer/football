# https://github.com/danmorse314/nfl-stuff/blob/main/playmaking_epa_function.R

get_playmaking_epa <- function(pbp){
  
  # Calculate playmaking EPA for defensive players
  # originated as EPA+ by Brian Burke
  # original code from Daniel Houston
  
  # pbp: play by play data pulled from the nflverse https://github.com/nflverse
  #       pbp <- nflreadr::load_pbp()
  
  pbp <- pbp_df |>
    dplyr::filter(!is.na(epa) &
                    (pass == 1 | rush == 1)) |>
    suppressMessagesssages({
      nflfastR::decode_player_ids()
    })
  
  # columns to keep
  keep_cols <- c("season","game_id","play_id","posteam","defteam","ep","epa","wpa","play_type",
                 "interception","touchdown","fumble_lost","fumble_forced",
                 "return_yards","qb_hit","sack")
  
  # CREATE SUBDATAFRAMES FOR LEAGUE
  interceptions_pbp <- pbp |>
    dplyr::filter(!is.na(interception_player_id)) |>
    dplyr::mutate(player_int = 1) |>
    dplyr::select(
      dplyr::all_of(keep_cols), player_int,
      player_id = interception_player_id,
      player_name = interception_player_name
    )
  
  blocked_pbp <- pbp |>
    dplyr::filter(!is.na(blocked_player_id)) |>
    dplyr::mutate(player_block = 1) |>
    dplyr::select(
      dplyr::all_of(keep_cols), player_block,
      player_id = blocked_player_id,
      player_name = blocked_player_name
    )
  
  sack_pbp <- pbp |>
    dplyr::filter(!is.na(sack_player_id)) |>
    dplyr::mutate(player_sack = 1) |>
    dplyr::select(
      dplyr::all_of(keep_cols), player_sack,
      player_id = sack_player_id,
      player_name = sack_player_name
    )
  
  half_sack_1_pbp <- pbp |>
    dplyr::filter(!is.na(half_sack_1_player_id)) |>
    dplyr::mutate(player_sack = .5) |>
    dplyr::select(
      dplyr::all_of(keep_cols), player_sack,
      player_id = half_sack_1_player_id,
      player_name = half_sack_1_player_name
    )
  
  half_sack_2_pbp <- pbp |>
    dplyr::filter(!is.na(half_sack_2_player_id)) |>
    dplyr::mutate(player_sack = .5) |>
    dplyr::select(
      dplyr::all_of(keep_cols), player_sack,
      player_id = half_sack_2_player_id,
      player_name = half_sack_2_player_name
    )
  
  qb_hit_1_pbp <- pbp |>
    dplyr::filter(!is.na(qb_hit_1_player_id)) |>
    dplyr::mutate(player_qb_hit = 1) |>
    dplyr::select(
      dplyr::all_of(keep_cols), player_qb_hit,
      player_id = qb_hit_1_player_id,
      player_name = qb_hit_1_player_name
    )
  
  qb_hit_2_pbp <- pbp |>
    dplyr::filter(!is.na(qb_hit_2_player_id)) |>
    dplyr::mutate(player_qb_hit = 1) |>
    dplyr::select(
      dplyr::all_of(keep_cols), player_qb_hit,
      player_id = qb_hit_2_player_id,
      player_name = qb_hit_2_player_name
    )
  
  tfl_1_pbp <- pbp |>
    dplyr::filter(!is.na(tackle_for_loss_1_player_id)) |>
    dplyr::mutate(player_tfl = 1) |>
    dplyr::select(
      dplyr::all_of(keep_cols), player_tfl,
      player_id = tackle_for_loss_1_player_id,
      player_name = tackle_for_loss_1_player_name
    )
  
  tfl_2_pbp <- pbp |>
    dplyr::filter(!is.na(tackle_for_loss_2_player_id)) |>
    dplyr::mutate(player_tfl = 1) |>
    dplyr::select(
      dplyr::all_of(keep_cols), player_tfl,
      player_id = tackle_for_loss_2_player_id,
      player_name = tackle_for_loss_2_player_name
    )
  
  forced_fumble_1_pbp <- pbp |>
    dplyr::filter(!is.na(forced_fumble_player_1_player_id)) |>
    dplyr::mutate(player_ff = 1) |>
    dplyr::select(
      dplyr::all_of(keep_cols), player_ff,
      player_id = forced_fumble_player_1_player_id,
      player_name = forced_fumble_player_1_player_name
    )
  
  forced_fumble_2_pbp <- pbp |>
    dplyr::filter(!is.na(forced_fumble_player_2_player_id)) |>
    dplyr::mutate(player_ff = 1) |>
    dplyr::select(
      dplyr::all_of(keep_cols), player_ff,
      player_id = forced_fumble_player_2_player_id,
      player_name = forced_fumble_player_2_player_name
    )
  
  solo_tackle_1_pbp <- pbp |>
    dplyr::filter(!is.na(solo_tackle_1_player_id)) |>
    dplyr::mutate(player_solo = 1) |>
    dplyr::select(dplyr::all_of(keep_cols), player_solo,
                  player_id = solo_tackle_1_player_id, player_name = solo_tackle_1_player_name)
  
  solo_tackle_2_pbp <- pbp |>
    dplyr::filter(!is.na(solo_tackle_2_player_id)) |>
    dplyr::mutate(player_solo = 1) |>
    dplyr::select(dplyr::all_of(keep_cols), player_solo,
                  player_id = solo_tackle_2_player_id, player_name = solo_tackle_2_player_name)
  
  twa_1_pbp <- pbp |>
    dplyr::filter(!is.na(tackle_with_assist_1_player_id)) |>
    dplyr::mutate(player_solo = 1) |>
    dplyr::select(
      dplyr::all_of(keep_cols), player_solo,
      player_id = tackle_with_assist_1_player_id,
      player_name = tackle_with_assist_1_player_name
    )
  
  twa_2_pbp <- pbp |>
    dplyr::filter(!is.na(tackle_with_assist_2_player_id)) |>
    dplyr::mutate(player_solo = 1) |>
    dplyr::select(
      dplyr::all_of(keep_cols), player_solo,
      player_id = tackle_with_assist_2_player_id,
      player_name = tackle_with_assist_2_player_name
    )
  
  assist_tackle_1_pbp <- pbp |>
    dplyr::filter(!is.na(assist_tackle_1_player_id)) |>
    dplyr::mutate(player_assist = 1) |>
    dplyr::select(dplyr::all_of(keep_cols), player_assist,
                  player_id = assist_tackle_1_player_id, player_name = assist_tackle_1_player_name)
  
  assist_tackle_2_pbp <- pbp |>
    dplyr::filter(!is.na(assist_tackle_2_player_id)) |>
    dplyr::mutate(player_assist = 1) |>
    dplyr::select(dplyr::all_of(keep_cols), player_assist,
                  player_id = assist_tackle_2_player_id, player_name = assist_tackle_2_player_name)
  
  assist_tackle_3_pbp <- pbp |>
    dplyr::filter(!is.na(assist_tackle_3_player_id)) |>
    dplyr::mutate(player_assist = 1) |>
    dplyr::select(dplyr::all_of(keep_cols), player_assist,
                  player_id = assist_tackle_3_player_id, player_name = assist_tackle_3_player_name)
  
  assist_tackle_4_pbp <- pbp |>
    dplyr::filter(!is.na(assist_tackle_4_player_id)) |>
    dplyr::mutate(player_assist = 1) |>
    dplyr::select(dplyr::all_of(keep_cols), player_assist,
                  player_id = assist_tackle_4_player_id, player_name = assist_tackle_4_player_name)
  
  pass_defense_1_pbp <- pbp |>
    dplyr::filter(!is.na(pass_defense_1_player_id)) |>
    dplyr::mutate(player_pass_defense = 1) |>
    dplyr::select(dplyr::all_of(keep_cols), player_pass_defense,
                  player_id = pass_defense_1_player_id, player_name = pass_defense_1_player_name)
  
  pass_defense_2_pbp <- pbp |>
    dplyr::filter(!is.na(pass_defense_2_player_id)) |>
    dplyr::mutate(player_pass_defense = 1) |>
    dplyr::select(dplyr::all_of(keep_cols), player_pass_defense,
                  player_id = pass_defense_2_player_id, player_name = pass_defense_2_player_name)
  
  fumble_recovery_1_pbp <- pbp |>
    dplyr::filter(!is.na(fumble_recovery_1_player_id)) |>
    dplyr::mutate(player_fr = 1) |>
    dplyr::mutate(return_yards = fumble_recovery_1_yards) |>
    dplyr::select(dplyr::all_of(keep_cols), player_fr,
                  player_id = fumble_recovery_1_player_id, player_name = fumble_recovery_1_player_name)
  
  fumble_recovery_2_pbp <- pbp |>
    dplyr::filter(!is.na(fumble_recovery_2_player_id)) |>
    dplyr::mutate(player_fr = 1) |>
    dplyr::mutate(return_yards = fumble_recovery_2_yards) |>
    dplyr::select(dplyr::all_of(keep_cols), player_fr,
                  player_id = fumble_recovery_2_player_id, player_name = fumble_recovery_2_player_name)
  
  #MERGE SUBDATAFRAMES including duplicates
  defense_df_merged <- dplyr::bind_rows(
    interceptions_pbp, 
    blocked_pbp, 
    sack_pbp, half_sack_1_pbp, half_sack_2_pbp,
    tfl_1_pbp, tfl_2_pbp,
    qb_hit_1_pbp, qb_hit_2_pbp, 
    forced_fumble_1_pbp, forced_fumble_2_pbp,
    solo_tackle_1_pbp, solo_tackle_2_pbp, twa_1_pbp, twa_2_pbp,
    assist_tackle_1_pbp, assist_tackle_2_pbp, assist_tackle_3_pbp, assist_tackle_4_pbp,
    pass_defense_1_pbp, pass_defense_2_pbp,
    fumble_recovery_1_pbp, fumble_recovery_2_pbp
  )
  
  # basic defensive stats
  # these should match pro-football-reference stats
  # note that epa does not have to be negative here
  basic_d <- defense_df_merged |>
    dplyr::group_by(player_id, season) |>
    dplyr::summarize(
      games_played = length(unique(game_id)),
      interceptions = sum(player_int, na.rm = TRUE),
      interception_yards = sum(return_yards[player_int == 1], na.rm = TRUE),
      interception_tds = sum(player_int == 1 & touchdown == 1, na.rm = TRUE),
      interception_long = ifelse(
        interceptions > 0,
        max(return_yards[player_int == 1], na.rm = TRUE),
        0
      ),
      passes_defensed = sum(player_pass_defense, na.rm = TRUE),
      fumbles_forced = sum(player_ff, na.rm = TRUE),
      fumbles_recovered = sum(player_fr, na.rm = TRUE),
      fumble_yards = sum(return_yards[player_fr == 1], na.rm = TRUE),
      fumble_recovery_tds = sum(player_fr == 1 & touchdown == 1, na.rm = TRUE),
      sacks = sum(player_sack, na.rm = TRUE),
      tackles_solo = sum(player_solo, na.rm = TRUE),
      tackles_assist = sum(player_assist, na.rm = TRUE),
      tackles_combined = tackles_solo + tackles_assist,
      tfl = sum(player_tfl, na.rm = TRUE),
      qb_hits = sum(player_qb_hit, na.rm = TRUE),
      .groups = "drop"
    )
  
  #remove duplicate plays for individual players BUT WITHOUT REMOVING SAME
  #use code below, but qualify that you're removing duplicates of same play_id AND player_name 
  defense_df_merged <- defense_df_merged |>
    dplyr::distinct(play_id, player_id, .keep_all = TRUE)
  
  # get rosters for position data
  rosters <- nflreadr::load_rosters(unique(pbp$season)) |>
    dplyr::select(season, full_name, player_id = gsis_id, position,
                  jersey_number, pff_id, headshot_url)
  
  #CREATE FINAL CALCULATION
  pmepa <- defense_df_merged |>
    dplyr::filter(epa < 0) |>
    dplyr::group_by(player_id, season) |>
    dplyr::summarize(
      player_name = paste(unique(player_name), collapse = ', '),
      team = paste(unique(defteam), collapse = ', '),
      plays_made = length(epa), 
      playmaking_epa = abs(sum(epa)),
      pass_playmaking_epa = abs(sum(epa[play_type == "pass"])),
      rush_playmaking_epa = abs(sum(epa[play_type == "run"])),
      playmaking_wpa = abs(sum(wpa[!is.na(wpa)])),
      .groups = "drop"
    ) |>
    dplyr::arrange(-playmaking_epa) |>
    dplyr::left_join(basic_d, by = c("player_id","season")) |>
    dplyr::left_join(rosters, by = c("player_id","season")) |>
    # select only defensive players
    dplyr::filter(position %in% c("CB","DB","DE","DL","DT","FS","ILB","LB","NT","OLB","S","SS")) |>
    # arrange the columns a bit nicer & matches PFR
    dplyr::select(
      season, full_name, team:sacks, tackles_combined, tackles_solo, tackles_assist,
      tfl, qb_hits, player_name, player_id, pff_id, jersey_number, headshot_url
    )
  
  return(pmepa)
  
}
