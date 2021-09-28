
# Update Schedule ---------------------------------------------------------

update_schedule_db <- function(season = year, db_connection = NULL){
  con <- db_connection
  schedule_df <- nflfastR::fast_scraper_schedules(season)
  DBI::dbExecute(db_connection, glue('DELETE FROM "nflfastR_schedule" WHERE "season" = {season}'))
  
  DBI::dbWriteTable(conn = db_connection, name = 'nflfastR_schedule', value = schedule_df, append = TRUE)
  DBI::dbDisconnect(con)
}





# Update Roster -----------------------------------------------------------

update_roster_db <- function(season = year, db_connection = NULL){
  con <- db_connection
  roster_df <- nflfastR::fast_scraper_roster(season)
  DBI::dbExecute(db_connection, glue('DELETE FROM "nflfastR_rosters" WHERE "season" = {season}'))
  
  DBI::dbWriteTable(conn = db_connection, name = 'nflfastR_rosters', value = roster_df, append = TRUE)
  DBI::dbDisconnect(con)
}





# Update Trades -----------------------------------------------------------

update_trades_db <- function(season = year, db_connection = NULL){
  con <- db_connection
  trades_df <- nflreadr::load_trades(seasons = season)
  DBI::dbExecute(db_connection, glue('DELETE FROM "nflfastR_trades" WHERE "season" = {season}'))
  
  DBI::dbWriteTable(conn = db_connection, name = 'nflfastR_trades', value = trades_df, append = TRUE)
  DBI::dbDisconnect(con)
}





# Update Draft ------------------------------------------------------------

update_draft_db <- function(season = year, db_connection = NULL){
  con <- db_connection
  draft_df <- nflreadr::load_draft_picks(seasons = season)
  DBI::dbExecute(db_connection, glue('DELETE FROM "nflfastR_draft" WHERE "season" = {season}'))
  
  DBI::dbWriteTable(conn = db_connection, name = 'nflfastR_draft', value = draft_df, append = TRUE)
  DBI::dbDisconnect(con)
}





# Update QBR --------------------------------------------------------------

update_ngs_db <- function(season = year, db_connection = NULL){
  con <- db_connection
  draft_df <- nflreadr::load_espn_qbr(seasons = season)
  DBI::dbExecute(db_connection, glue('DELETE FROM "nflfastR_draft" WHERE "season" = {season}'))
  
  DBI::dbWriteTable(conn = db_connection, name = 'nflfastR_draft', value = draft_df, append = TRUE)
  DBI::dbDisconnect(con)
}





# Player stats ------------------------------------------------------------

calculate_player_stats_mod <- function(pbp, weekly = FALSE) {
  
  
  # Prepare data ------------------------------------------------------------
  
  # load plays with multiple laterals
  con <- url("https://github.com/mrcaseb/nfl-data/blob/master/data/lateral_yards/multiple_lateral_yards.rds?raw=true")
  mult_lats <- readRDS(con) %>%
    dplyr::mutate(
      season = substr(.data$game_id, 1, 4) %>% as.integer(),
      week = substr(.data$game_id, 6, 7) %>% as.integer()
    ) %>%
    dplyr::filter(.data$yards != 0) %>%
    # the list includes all plays with multiple laterals
    # and all receivers. Since the last one already is in the
    # pbp data, we have to drop him here so the entry isn't duplicated
    dplyr::group_by(.data$game_id, .data$play_id) %>%
    dplyr::slice(seq_len(dplyr::n() - 1)) %>%
    dplyr::ungroup()
  close(con)
  
  # filter down to the 2 dfs we need
  suppressMessages({
    # 1. for "normal" plays: get plays that count in official stats
    data <- pbp %>%
      dplyr::filter(
        !is.na(.data$down),
        .data$play_type %in% c("pass", "qb_kneel", "qb_spike", "run")
      ) %>%
      decode_player_ids()
    
    if (!"qb_epa" %in% names(data)) data <- add_qb_epa(data)
    
    # 2. for 2pt conversions only, get those plays
    two_points <- pbp %>%
      dplyr::filter(.data$two_point_conv_result == "success") %>%
      dplyr::select(
        "week", "season", "posteam",
        "pass_attempt", "rush_attempt",
        "passer_player_name", "passer_player_id",
        "rusher_player_name", "rusher_player_id",
        "lateral_rusher_player_name", "lateral_rusher_player_id",
        "receiver_player_name", "receiver_player_id",
        "lateral_receiver_player_name", "lateral_receiver_player_id"
      ) %>%
      decode_player_ids()
  })
  
  if (!"special" %in% names(pbp)) {# we need this column for the special teams tds
    pbp <- pbp %>%
      dplyr::mutate(
        special = dplyr::if_else(
          .data$play_type %in% c("extra_point","field_goal","kickoff","punt"),
          1, 0
        )
      )
  }
  
  s_type <- pbp %>%
    dplyr::select(.data$season, .data$season_type, .data$week) %>%
    dplyr::distinct()
  
  # Get data seasons
  data_seasons <- pbp %>% pull(season) %>% unique()

  # load gsis_ids of FBs and RBs for RACR
  racr_ids <- nflreadr::qs_from_url("https://github.com/nflverse/nflfastR-roster/raw/master/data/nflfastR-RB_ids.qs")
  
  # Passing stats -----------------------------------------------------------
  
  # get passing stats
  pass_df <- data %>%
    dplyr::filter(.data$play_type %in% c("pass", "qb_spike")) %>%
    dplyr::group_by(.data$passer_player_id, .data$week, .data$season) %>% 
    dplyr::summarize(
      passing_yards_after_catch = sum((.data$passing_yards - .data$air_yards) * .data$complete_pass, na.rm = TRUE),
      name_pass = dplyr::first(.data$passer_player_name),
      team_pass = dplyr::first(.data$posteam),
      passing_yards = sum(.data$passing_yards, na.rm = TRUE),
      passing_tds = sum(.data$touchdown == 1 & .data$td_team == .data$posteam & .data$complete_pass == 1),
      interceptions = sum(.data$interception),
      attempts = sum(.data$complete_pass == 1 | .data$incomplete_pass == 1 | .data$interception == 1),
      completions = sum(.data$complete_pass == 1),
      sack_fumbles = sum(.data$fumble == 1 & .data$fumbled_1_player_id == .data$passer_player_id),
      sack_fumbles_lost = sum(.data$fumble_lost == 1 & .data$fumbled_1_player_id == .data$passer_player_id),
      passing_air_yards = sum(.data$air_yards, na.rm = TRUE),
      sacks = sum(.data$sack),
      sack_yards = -1*sum(.data$yards_gained * .data$sack),
      passing_first_downs = sum(.data$first_down_pass),
      passing_epa = sum(.data$qb_epa, na.rm = TRUE),
      pacr = .data$passing_yards / .data$passing_air_yards,
      pacr = dplyr::case_when(
        is.nan(.data$pacr) ~ NA_real_,
        .data$passing_air_yards <= 0 ~ 0,
        TRUE ~ .data$pacr
      ),
      anya = (.data$passing_yards - .data$sack_yards + (20 * passing_tds) - (45 * interceptions)) / (attempts + sacks)
    ) %>%
    dplyr::rename(player_id = .data$passer_player_id) %>%
    dplyr::ungroup()
  
  if (isTRUE(weekly)) pass_df <- add_dakota(pass_df, pbp = pbp, weekly = weekly)
  
  pass_two_points <- two_points %>%
    dplyr::filter(.data$pass_attempt == 1) %>%
    dplyr::group_by(.data$passer_player_id, .data$week, .data$season) %>%
    dplyr::summarise(
      # need name_pass and team_pass here for the full join in the next pipe
      name_pass = custom_mode(.data$passer_player_name),
      team_pass = custom_mode(.data$posteam),
      passing_2pt_conversions = dplyr::n()
    ) %>%
    dplyr::rename(player_id = .data$passer_player_id) %>%
    dplyr::ungroup()
  
  pass_df <- pass_df %>%
    # need a full join because players without passing stats that recorded
    # a passing two point (e.g. WRs) are dropped in any other join
    dplyr::full_join(pass_two_points, by = c("player_id", "week", "season", "name_pass", "team_pass")) %>%
    dplyr::mutate(passing_2pt_conversions = dplyr::if_else(is.na(.data$passing_2pt_conversions), 0L, .data$passing_2pt_conversions)) %>%
    dplyr::filter(!is.na(.data$player_id))
  
  pass_df_nas <- is.na(pass_df)
  epa_index <- which(dimnames(pass_df_nas)[[2]] %in% c("passing_epa", "dakota", "pacr"))
  pass_df_nas[,epa_index] <- c(FALSE)
  
  pass_df[pass_df_nas] <- 0
  
  # Rushing stats -----------------------------------------------------------
  
  # rush df 1: primary rusher
  rushes <- data %>%
    dplyr::filter(.data$play_type %in% c("run", "qb_kneel")) %>%
    dplyr::group_by(.data$rusher_player_id, .data$week, .data$season) %>% 
    dplyr::mutate(
      hvt = ifelse(yardline_100 <= 10, 1, 0)
    ) %>% 
    dplyr::summarize(
      name_rush = dplyr::first(.data$rusher_player_name),
      team_rush = dplyr::first(.data$posteam),
      yards = sum(.data$rushing_yards, na.rm = TRUE),
      tds = sum(.data$td_player_id == .data$rusher_player_id, na.rm = TRUE),
      carries = dplyr::n(),
      rushing_fumbles = sum(.data$fumble == 1 & .data$fumbled_1_player_id == .data$rusher_player_id & is.na(.data$lateral_rusher_player_id)),
      rushing_fumbles_lost = sum(.data$fumble_lost == 1 & .data$fumbled_1_player_id == .data$rusher_player_id & is.na(.data$lateral_rusher_player_id)),
      rushing_first_downs = sum(.data$first_down_rush & is.na(.data$lateral_rusher_player_id)),
      rushing_epa = sum(.data$epa, na.rm = TRUE),
      hvt = sum(.data$hvt)
    ) %>%
    dplyr::ungroup()
  
  # rush df 2: lateral
  laterals <- data %>%
    dplyr::filter(!is.na(.data$lateral_rusher_player_id)) %>%
    dplyr::group_by(.data$lateral_rusher_player_id, .data$week, .data$season) %>%
    dplyr::summarize(
      lateral_yards = sum(.data$lateral_rushing_yards, na.rm = TRUE),
      lateral_fds = sum(.data$first_down_rush, na.rm = TRUE),
      lateral_tds = sum(.data$td_player_id == .data$lateral_rusher_player_id, na.rm = TRUE),
      lateral_att = dplyr::n(),
      lateral_fumbles =  sum(.data$fumble, na.rm = TRUE),
      lateral_fumbles_lost = sum(.data$fumble_lost, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::rename(rusher_player_id = .data$lateral_rusher_player_id) %>%
    dplyr::bind_rows(
      mult_lats %>%
        dplyr::filter(
          .data$type == "lateral_rushing" & .data$season %in% data$season & .data$week %in% data$week
        ) %>%
        dplyr::select("season", "week", "rusher_player_id" = .data$gsis_player_id, "lateral_yards" = .data$yards) %>%
        dplyr::mutate(lateral_tds = 0L, lateral_att = 1L)
    )
  
  # rush df: join
  rush_df <- rushes %>%
    dplyr::left_join(laterals, by = c("rusher_player_id", "week", "season")) %>%
    dplyr::mutate(
      lateral_yards = dplyr::if_else(is.na(.data$lateral_yards), 0, .data$lateral_yards),
      lateral_tds = dplyr::if_else(is.na(.data$lateral_tds), 0L, .data$lateral_tds),
      lateral_fumbles = dplyr::if_else(is.na(.data$lateral_fumbles), 0, .data$lateral_fumbles),
      lateral_fumbles_lost = dplyr::if_else(is.na(.data$lateral_fumbles_lost), 0, .data$lateral_fumbles_lost),
      lateral_fds = dplyr::if_else(is.na(.data$lateral_fds), 0, .data$lateral_fds)
    ) %>%
    dplyr::mutate(
      rushing_yards = .data$yards + .data$lateral_yards,
      rushing_tds = .data$tds + .data$lateral_tds,
      rushing_first_downs = .data$rushing_first_downs + .data$lateral_fds,
      rushing_fumbles = .data$rushing_fumbles + .data$lateral_fumbles,
      rushing_fumbles_lost = .data$rushing_fumbles_lost + .data$lateral_fumbles_lost,
      hvt_percentage = hvt / carries
    ) %>%
    dplyr::rename(player_id = .data$rusher_player_id) %>%
    dplyr::select("player_id", "week", "season", "name_rush", "team_rush",
                  "rushing_yards", "carries", "rushing_tds", "rushing_fumbles",
                  "rushing_fumbles_lost", "rushing_first_downs", "rushing_epa", "hvt") %>%
    dplyr::ungroup()
  
  rush_two_points <- two_points %>%
    dplyr::filter(.data$rush_attempt == 1) %>%
    dplyr::group_by(.data$rusher_player_id, .data$week, .data$season) %>%
    dplyr::summarise(
      # need name_rush and team_rush here for the full join in the next pipe
      name_rush = custom_mode(.data$rusher_player_name),
      team_rush = custom_mode(.data$posteam),
      rushing_2pt_conversions = dplyr::n()
    ) %>%
    dplyr::rename(player_id = .data$rusher_player_id) %>%
    dplyr::ungroup()
  
  rush_df <- rush_df %>%
    # need a full join because players without rushing stats that recorded
    # a rushing two point (mostly QBs) are dropped in any other join
    dplyr::full_join(rush_two_points, by = c("player_id", "week", "season", "name_rush", "team_rush")) %>%
    dplyr::mutate(rushing_2pt_conversions = dplyr::if_else(is.na(.data$rushing_2pt_conversions), 0L, .data$rushing_2pt_conversions)) %>%
    dplyr::filter(!is.na(.data$player_id))
  
  rush_df_nas <- is.na(rush_df)
  epa_index <- which(dimnames(rush_df_nas)[[2]] == "rushing_epa")
  rush_df_nas[,epa_index] <- c(FALSE)
  
  rush_df[rush_df_nas] <- 0
  
  # Receiving stats ---------------------------------------------------------
  
  # receiver df 1: primary receiver
  rec <- data %>%
    dplyr::filter(!is.na(.data$receiver_player_id)) %>% 
    dplyr::mutate(
      hvt = ifelse(yardline_100 <= 10, 1, 0)
    ) %>% 
    dplyr::group_by(.data$receiver_player_id, .data$week, .data$season) %>%
    dplyr::summarize(
      name_receiver = dplyr::first(.data$receiver_player_name),
      team_receiver = dplyr::first(.data$posteam),
      yards = sum(.data$receiving_yards, na.rm = TRUE),
      receptions = sum(.data$complete_pass == 1),
      targets = dplyr::n(),
      tds = sum(.data$td_player_id == .data$receiver_player_id, na.rm = TRUE),
      receiving_fumbles = sum(.data$fumble == 1 & .data$fumbled_1_player_id == .data$receiver_player_id & is.na(.data$lateral_receiver_player_id)),
      receiving_fumbles_lost = sum(.data$fumble_lost == 1 & .data$fumbled_1_player_id == .data$receiver_player_id & is.na(.data$lateral_receiver_player_id)),
      receiving_air_yards = sum(.data$air_yards, na.rm = TRUE),
      receiving_yards_after_catch = sum(.data$yards_after_catch, na.rm = TRUE),
      receiving_first_downs = sum(.data$first_down_pass & is.na(.data$lateral_receiver_player_id)),
      receiving_epa = sum(.data$epa, na.rm = TRUE)
    ) %>%
    dplyr::ungroup()
  
  # receiver df 2: lateral
  laterals <- data %>%
    dplyr::filter(!is.na(.data$lateral_receiver_player_id)) %>%
    dplyr::mutate(
      hvt = ifelse(yardline_100 <= 10, 1, 0)
    ) %>% 
    dplyr::group_by(.data$lateral_receiver_player_id, .data$week, .data$season) %>%
    dplyr::summarize(
      lateral_yards = sum(.data$lateral_receiving_yards, na.rm = TRUE),
      lateral_tds = sum(.data$td_player_id == .data$lateral_receiver_player_id, na.rm = TRUE),
      lateral_att = dplyr::n(),
      lateral_fds = sum(.data$first_down_pass, na.rm = T),
      lateral_fumbles = sum(.data$fumble, na.rm = T),
      lateral_fumbles_lost = sum(.data$fumble_lost, na.rm = T)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::rename(receiver_player_id = .data$lateral_receiver_player_id) %>%
    dplyr::bind_rows(
      mult_lats %>%
        dplyr::filter(
          .data$type == "lateral_receiving" & .data$season %in% data$season & .data$week %in% data$week
        ) %>%
        dplyr::select("season", "week", "receiver_player_id" = .data$gsis_player_id, "lateral_yards" = .data$yards) %>%
        dplyr::mutate(lateral_tds = 0L, lateral_att = 1L)
    )
  
  # receiver df 3: team receiving for WOPR
  rec_team <- data %>%
    dplyr::filter(!is.na(.data$receiver_player_id)) %>%
    dplyr::group_by(.data$posteam, .data$week, .data$season) %>%
    dplyr::summarize(
      team_targets = dplyr::n(),
      team_air_yards = sum(.data$air_yards, na.rm = TRUE),
    ) %>%
    dplyr::ungroup()
  
  # rec df: join
  rec_df <- rec %>%
    dplyr::left_join(laterals, by = c("receiver_player_id", "week", "season")) %>%
    dplyr::left_join(rec_team, by = c("team_receiver" = "posteam", "week", "season")) %>%
    dplyr::mutate(
      lateral_yards = dplyr::if_else(is.na(.data$lateral_yards), 0, .data$lateral_yards),
      lateral_tds = dplyr::if_else(is.na(.data$lateral_tds), 0L, .data$lateral_tds),
      lateral_fumbles = dplyr::if_else(is.na(.data$lateral_fumbles), 0, .data$lateral_fumbles),
      lateral_fumbles_lost = dplyr::if_else(is.na(.data$lateral_fumbles_lost), 0, .data$lateral_fumbles_lost),
      lateral_fds = dplyr::if_else(is.na(.data$lateral_fds), 0, .data$lateral_fds)
    ) %>%
    dplyr::mutate(
      receiving_yards = .data$yards + .data$lateral_yards,
      receiving_tds = .data$tds + .data$lateral_tds,
      receiving_yards_after_catch = .data$receiving_yards_after_catch + .data$lateral_yards,
      receiving_first_downs = .data$receiving_first_downs + .data$lateral_fds,
      receiving_fumbles = .data$receiving_fumbles + .data$lateral_fumbles,
      receiving_fumbles_lost = .data$receiving_fumbles_lost + .data$lateral_fumbles_lost,
      racr = .data$receiving_yards / .data$receiving_air_yards,
      racr = dplyr::case_when(
        is.nan(.data$racr) ~ NA_real_,
        .data$receiving_air_yards == 0 ~ 0,
        # following Josh Hermsmeyer's definition, RACR stays < 0 for RBs (and FBs) and is set to
        # 0 for Receivers. The list "racr_ids" includes all known RB and FB gsis_ids
        .data$receiving_air_yards < 0 & !.data$receiver_player_id %in% racr_ids$gsis_id ~ 0,
        TRUE ~ .data$racr
      ),
      target_share = .data$targets / .data$team_targets,
      air_yards_share = .data$receiving_air_yards / .data$team_air_yards,
      wopr = 1.5 * .data$target_share + 0.7 * .data$air_yards_share
    ) %>%
    dplyr::rename(player_id = .data$receiver_player_id) %>%
    dplyr::select("player_id", "week", "season", "name_receiver", "team_receiver",
                  "receiving_yards", "receiving_air_yards", "receiving_yards_after_catch",
                  "receptions", "targets", "receiving_tds", "receiving_fumbles",
                  "receiving_fumbles_lost", "receiving_first_downs", "receiving_epa",
                  "racr", "target_share", "air_yards_share", "wopr")
  
  rec_two_points <- two_points %>%
    dplyr::filter(.data$pass_attempt == 1) %>%
    dplyr::group_by(.data$receiver_player_id, .data$week, .data$season) %>%
    dplyr::summarise(
      # need name_receiver and team_receiver here for the full join in the next pipe
      name_receiver = custom_mode(.data$receiver_player_name),
      team_receiver = custom_mode(.data$posteam),
      receiving_2pt_conversions = dplyr::n()
    ) %>%
    dplyr::rename(player_id = .data$receiver_player_id) %>%
    dplyr::ungroup()
  
  rec_df <- rec_df %>%
    # need a full join because players without receiving stats that recorded
    # a receiving two point are dropped in any other join
    dplyr::full_join(rec_two_points, by = c("player_id", "week", "season", "name_receiver", "team_receiver")) %>%
    dplyr::mutate(receiving_2pt_conversions = dplyr::if_else(is.na(.data$receiving_2pt_conversions), 0L, .data$receiving_2pt_conversions)) %>%
    dplyr::filter(!is.na(.data$player_id))
  
  rec_df_nas <- is.na(rec_df)
  epa_index <- which(dimnames(rec_df_nas)[[2]] == c("receiving_epa", "racr", "target_share", "air_yards_share", "wopr"))
  rec_df_nas[,epa_index] <- c(FALSE)
  
  rec_df[rec_df_nas] <- 0
  
  
  # Special Teams -----------------------------------------------------------
  
  st_tds <- pbp %>%
    dplyr::filter(.data$special == 1 & !is.na(.data$td_player_id)) %>%
    dplyr::group_by(.data$td_player_id, .data$week, .data$season) %>%
    dplyr::summarise(
      name_st = custom_mode(.data$td_player_name),
      team_st = custom_mode(.data$td_team),
      special_teams_tds = sum(.data$touchdown, na.rm = TRUE)
    ) %>%
    dplyr::rename(player_id = .data$td_player_id)
  

  # Combine all stats -------------------------------------------------------

  if (length(which(data_seasons >= 2013)) > 0) {
    snaps_df <-
      nflreadr::load_snap_counts(seasons = data_seasons[data_seasons >= 2013]) %>%
      left_join(roster_df %>%
                  select(gsis_id, pfr_id, first_name, last_name),
                by = c('pfr_id')) %>% 
      left_join(schedule_df %>% 
                  select(game_id, week),
                by = c('game_id')) %>% 
      mutate(
        name_snaps = paste0(substr(first_name, 1, 1), '.', last_name),
        offense_snaps = offense_snaps %>% as.integer(),
        offense_pct = offense_pct / 100,
        defense_snaps = defense_snaps %>% as.integer(),
        defense_pct = defense_pct / 100,
        st_snaps = st_snaps %>% as.integer(),
        st_pct = st_pct / 100
      ) %>% 
      select(
        player_id = gsis_id,
        week,
        season,
        name_snaps,
        team_snaps = team,
        offense_snaps:st_pct
      )
  } else {
    snaps_df <- tibble() %>%
      mutate(player_id = as.character(NA),
             week = as.integer(NA),
             season = as.integer(NA),
             name_snaps = as.character(NA))
  }

  
  # Combine all stats -------------------------------------------------------
  
  # combine all the stats together
  player_df <- pass_df %>%
    dplyr::full_join(rush_df, by = c("player_id", "week", "season")) %>%
    dplyr::full_join(rec_df, by = c("player_id", "week", "season")) %>%
    dplyr::full_join(st_tds, by = c("player_id", "week", "season")) %>% 
    dplyr::full_join(snaps_df, by = c("player_id", "week", "season")) %>% 
    dplyr::left_join(s_type, by = c("season", "week")) %>%
    dplyr::mutate(
      player_name = dplyr::case_when(
        !is.na(.data$name_pass) ~ .data$name_pass,
        !is.na(.data$name_rush) ~ .data$name_rush,
        !is.na(.data$name_receiver) ~ .data$name_receiver,
        !is.na(.data$name_snaps) ~ .data$name_snaps,
        TRUE ~ .data$name_st
      ),
      recent_team = dplyr::case_when(
        !is.na(.data$team_pass) ~ .data$team_pass,
        !is.na(.data$team_rush) ~ .data$team_rush,
        !is.na(.data$team_receiver) ~ .data$team_receiver,
        !is.na(.data$team_snaps) ~ .data$team_snaps,
        TRUE ~ .data$team_st
      )
    ) %>% 
    dplyr::left_join(data %>% select(season, game_id, week, recent_team = posteam) %>% unique(), by = c('season', 'week', 'recent_team')) %>% 
    dplyr::select(tidyselect::any_of(c(
      
      # id information
      "player_id", "player_name", "recent_team", "season", "week", "game_id", "season_type",
      
      # offense stats
      "offense_snaps", "offense_pct",
      
      # passing stats
      "completions", "attempts", "passing_yards", "passing_tds", "interceptions",
      "sacks", "sack_yards", "sack_fumbles", "sack_fumbles_lost", "passing_air_yards", "passing_yards_after_catch",
      "passing_first_downs", "passing_epa", "passing_2pt_conversions", "pacr", "anya", "dakota",
      
      # rushing stats
      "carries", "rushing_yards", "rushing_tds", "rushing_fumbles", "rushing_fumbles_lost",
      "rushing_first_downs", "rushing_epa", "rushing_2pt_conversions", "hvt",
      
      # receiving stats
      "receptions", "targets", "receiving_yards", "receiving_tds", "receiving_fumbles",
      "receiving_fumbles_lost", "receiving_air_yards", "receiving_yards_after_catch",
      "receiving_first_downs", "receiving_epa", "receiving_2pt_conversions", "racr",
      "target_share", "air_yards_share", "wopr", "hvt",
      
      # special teams
      "special_teams_tds", "st_snaps", "st_pct"
      
    ))) %>%
    dplyr::filter(!is.na(.data$player_id))
  
  player_df_nas <- is.na(player_df)
  epa_index <- which(dimnames(player_df_nas)[[2]] %in% c("season_type", "game_id", "week", "passing_epa", "rushing_epa", "receiving_epa", "dakota", "racr", "target_share", "air_yards_share", "wopr", "pacr"))
  player_df_nas[,epa_index] <- c(FALSE)
  
  player_df[player_df_nas] <- 0
  
  player_df <- player_df %>%
    dplyr::mutate(
      fantasy_points =
        1 / 25 * .data$passing_yards +
        4 * .data$passing_tds +
        -2 * .data$interceptions +
        1 / 10 * (.data$rushing_yards + .data$receiving_yards) +
        6 * (.data$rushing_tds + .data$receiving_tds + .data$special_teams_tds) +
        2 * (.data$passing_2pt_conversions + .data$rushing_2pt_conversions + .data$receiving_2pt_conversions) +
        -2 * (.data$sack_fumbles_lost + .data$rushing_fumbles_lost + .data$receiving_fumbles_lost),
      
      fantasy_points_ppr = .data$fantasy_points + .data$receptions,
      fantasy_points_half_ppr = .data$fantasy_points + (.data$receptions * .5)
    ) %>%
    dplyr::arrange(.data$player_id, .data$season, .data$week)
  
  
  # if user doesn't want week-by-week input, aggregate the whole df
  if (isFALSE(weekly)) {
    player_df <- player_df %>% 
      mutate(
        potential_offense_snaps = .data$offense_snaps / .data$offense_pct,
        potential_st_snaps = .data$st_snaps / .data$st_pct
      ) %>% 
      dplyr::group_by(.data$player_id) %>%
      dplyr::summarise(
        player_name = custom_mode(.data$player_name),
        games = dplyr::n(),
        recent_team = dplyr::last(.data$recent_team),
        offense_snaps = sum(.data$offense_snaps),
        # potential_offense_snaps = sum(.data$potential_offense_snaps), # total potential_snaps
        offense_pct = sum(.data$offense_snaps) / sum(.data$potential_offense_snaps), # calculate total snaps percentage
        # passing
        completions = sum(.data$completions),
        attempts = sum(.data$attempts),
        passing_yards = sum(.data$passing_yards),
        passing_tds = sum(.data$passing_tds),
        interceptions = sum(.data$interceptions),
        sacks = sum(.data$sacks),
        sack_yards = sum(.data$sack_yards),
        sack_fumbles = sum(.data$sack_fumbles),
        sack_fumbles_lost = sum(.data$sack_fumbles_lost),
        passing_air_yards = sum(.data$passing_air_yards),
        passing_yards_after_catch = sum(.data$passing_yards_after_catch),
        passing_first_downs = sum(.data$passing_first_downs),
        passing_epa = dplyr::if_else(all(is.na(.data$passing_epa)), NA_real_, sum(.data$passing_epa, na.rm = TRUE)),
        passing_2pt_conversions = sum(.data$passing_2pt_conversions),
        pacr = .data$passing_yards / .data$passing_air_yards,
        anya = (passing_yards - sack_yards + (20 * passing_tds) - (45 * interceptions)) / (attempts + sacks),
        
        # rushing
        carries = sum(.data$carries),
        rushing_yards = sum(.data$rushing_yards),
        rushing_tds = sum(.data$rushing_tds),
        rushing_fumbles = sum(.data$rushing_fumbles),
        rushing_fumbles_lost = sum(.data$rushing_fumbles_lost),
        rushing_first_downs = sum(.data$rushing_first_downs),
        rushing_epa = dplyr::if_else(all(is.na(.data$rushing_epa)), NA_real_, sum(.data$rushing_epa, na.rm = TRUE)),
        rushing_2pt_conversions = sum(.data$rushing_2pt_conversions),
        hvt = sum(.data$hvt, na.rm = TRUE),
        
        # receiving
        receptions = sum(.data$receptions),
        targets = sum(.data$targets),
        receiving_yards = sum(.data$receiving_yards),
        receiving_tds = sum(.data$receiving_tds),
        receiving_fumbles = sum(.data$receiving_fumbles),
        receiving_fumbles_lost = sum(.data$receiving_fumbles_lost),
        receiving_air_yards = sum(.data$receiving_air_yards),
        receiving_yards_after_catch = sum(.data$receiving_yards_after_catch),
        receiving_first_downs = sum(.data$receiving_first_downs),
        receiving_epa = dplyr::if_else(all(is.na(.data$receiving_epa)), NA_real_, sum(.data$receiving_epa, na.rm = TRUE)),
        receiving_2pt_conversions = sum(.data$receiving_2pt_conversions),
        racr = .data$receiving_yards / .data$receiving_air_yards,
        target_share = dplyr::if_else(all(is.na(.data$target_share)), NA_real_, mean(.data$target_share, na.rm = TRUE)),
        air_yards_share = dplyr::if_else(all(is.na(.data$air_yards_share)), NA_real_, mean(.data$air_yards_share, na.rm = TRUE)),
        wopr = 1.5 * .data$target_share + 0.7 * .data$air_yards_share,
        
        # special teams
        special_teams_tds = sum(.data$special_teams_tds),
        st_snaps = sum(.data$st_snaps),
        # potential_st_snaps = sum(.data$potential_st_snaps), # total potential_snaps
        st_pct = sum(.data$st_snaps) / sum(.data$potential_st_snaps), # calculate total snaps percentage
        
        # fantasy
        fantasy_points = sum(.data$fantasy_points),
        fantasy_points_ppr = sum(.data$fantasy_points_ppr),
        fantasy_points_half_ppr = sum(.data$fantasy_points_half_ppr)
      ) %>%
      dplyr::ungroup() %>% 
      dplyr::mutate(
        hvt_percentage = hvt / carries,
        hvt_per_game = hvt / games,
        racr = dplyr::case_when(
          is.nan(.data$racr) ~ NA_real_,
          .data$receiving_air_yards == 0 ~ 0,
          # following Josh Hermsmeyer's definition, RACR stays < 0 for RBs (and FBs) and is set to
          # 0 for Receivers. The list "racr_ids" includes all known RB and FB gsis_ids
          .data$receiving_air_yards < 0 & !.data$player_id %in% racr_ids$gsis_id ~ 0,
          TRUE ~ .data$racr
        ),
        pacr = dplyr::case_when(
          is.nan(.data$pacr) ~ NA_real_,
          .data$passing_air_yards <= 0 ~ 0,
          TRUE ~ .data$pacr
        )
      ) %>% 
      add_dakota(pbp = pbp, weekly = weekly) %>%
      dplyr::select(
        .data$player_id:.data$pacr,
        .data$dakota,
        dplyr::everything()
      )
  }
  
  return(player_df)
}


# Team stats --------------------------------------------------------------

calculate_team_stats_mod <- function(pbp, weekly = FALSE) {
  # Prepare data ------------------------------------------------------------
  
  # load plays with multiple laterals
  con <- url("https://github.com/mrcaseb/nfl-data/blob/master/data/lateral_yards/multiple_lateral_yards.rds?raw=true")
  mult_lats <- readRDS(con) %>%
    dplyr::mutate(
      season = substr(.data$game_id, 1, 4) %>% as.integer(),
      week = substr(.data$game_id, 6, 7) %>% as.integer()
    ) %>%
    dplyr::filter(.data$yards != 0) %>%
    # the list includes all plays with multiple laterals
    # and all receivers. Since the last one already is in the
    # pbp data, we have to drop him here so the entry isn't duplicated
    dplyr::group_by(.data$game_id, .data$play_id) %>%
    dplyr::slice(seq_len(dplyr::n() - 1)) %>%
    dplyr::ungroup()
  close(con)
  
  # filter down to the 2 dfs we need
  suppressMessages({
    # 1. for "normal" plays: get plays that count in official stats
    data <- pbp %>%
      dplyr::filter(
        !is.na(.data$down),
        .data$play_type %in% c("pass", "qb_kneel", "qb_spike", "run")
      ) %>%
      decode_player_ids()
    
    if (!"qb_epa" %in% names(data)) data <- add_qb_epa(data)
    
    # 2. for 2pt conversions only, get those plays
    two_points <- pbp %>%
      dplyr::filter(.data$two_point_conv_result == "success") %>%
      dplyr::select(
        "week", "season", "posteam",
        "pass_attempt", "rush_attempt",
        "passer_player_name", "passer_player_id",
        "rusher_player_name", "rusher_player_id",
        "lateral_rusher_player_name", "lateral_rusher_player_id",
        "receiver_player_name", "receiver_player_id",
        "lateral_receiver_player_name", "lateral_receiver_player_id"
      ) %>%
      decode_player_ids()
  })
  
  if (!"special" %in% names(pbp)) {# we need this column for the special teams tds
    pbp <- pbp %>%
      dplyr::mutate(
        special = dplyr::if_else(
          .data$play_type %in% c("extra_point","field_goal","kickoff","punt"),
          1, 0
        )
      )
  }
  
  s_type <- pbp %>%
    dplyr::select(.data$season, .data$season_type, .data$week) %>%
    dplyr::distinct()
  
  # load gsis_ids of FBs and RBs for RACR
  racr_ids <- nflreadr::qs_from_url("https://github.com/nflverse/nflfastR-roster/raw/master/data/nflfastR-RB_ids.qs")
  
  # Passing stats -----------------------------------------------------------
  
  # get passing stats
  pass_df <- data %>%
    dplyr::filter(.data$play_type %in% c("pass", "qb_spike")) %>%
    dplyr::group_by(.data$posteam, .data$week, .data$season) %>% 
    dplyr::summarize(
      passing_yards_after_catch = sum((.data$passing_yards - .data$air_yards) * .data$complete_pass, na.rm = TRUE),
      passing_yards = sum(.data$passing_yards, na.rm = TRUE),
      passing_tds = sum(.data$touchdown == 1 & .data$td_team == .data$posteam & .data$complete_pass == 1),
      interceptions = sum(.data$interception),
      attempts = sum(.data$complete_pass == 1 | .data$incomplete_pass == 1 | .data$interception == 1),
      completions = sum(.data$complete_pass == 1),
      sack_fumbles = sum(.data$fumble == 1 & .data$fumbled_1_player_id == .data$posteam),
      sack_fumbles_lost = sum(.data$fumble_lost == 1 & .data$fumbled_1_player_id == .data$posteam),
      passing_air_yards = sum(.data$air_yards, na.rm = TRUE),
      sacks = sum(.data$sack),
      sack_yards = -1*sum(.data$yards_gained * .data$sack),
      passing_first_downs = sum(.data$first_down_pass),
      passing_epa = sum(.data$qb_epa, na.rm = TRUE),
      pacr = .data$passing_yards / .data$passing_air_yards
    ) %>%
    dplyr::rename(team = .data$posteam) %>%
    dplyr::ungroup()
  
  if (isTRUE(weekly)) pass_df <- add_team_dakota(pass_df, pbp = pbp, weekly = weekly)
  
  pass_two_points <- two_points %>%
    dplyr::filter(.data$pass_attempt == 1) %>%
    dplyr::group_by(.data$posteam, .data$week, .data$season) %>%
    dplyr::summarise(
      passing_2pt_conversions = dplyr::n()
    ) %>%
    dplyr::rename(team = .data$posteam) %>%
    dplyr::ungroup()
  
  pass_df <- pass_df %>%
    # need a full join because players without passing stats that recorded
    # a passing two point (e.g. WRs) are dropped in any other join
    dplyr::full_join(pass_two_points, by = c("team", "week", "season")) %>%
    dplyr::mutate(passing_2pt_conversions = dplyr::if_else(is.na(.data$passing_2pt_conversions), 0L, .data$passing_2pt_conversions)) %>%
    dplyr::filter(!is.na(.data$team))
  
  pass_df_nas <- is.na(pass_df)
  epa_index <- which(dimnames(pass_df_nas)[[2]] %in% c("passing_epa", "dakota", "pacr"))
  pass_df_nas[,epa_index] <- c(FALSE)
  
  pass_df[pass_df_nas] <- 0
  
  # Rushing stats -----------------------------------------------------------
  
  # rush df 1: primary rusher
  rushes <- data %>%
    dplyr::filter(.data$play_type %in% c("run", "qb_kneel")) %>%
    dplyr::group_by(.data$posteam, .data$week, .data$season) %>% 
    dplyr::mutate(
      hvt = ifelse(yardline_100 <= 10, 1, 0)
    ) %>% 
    dplyr::summarize(
      yards = sum(.data$rushing_yards, na.rm = TRUE),
      tds = sum(.data$td_player_id == .data$rusher_player_id, na.rm = TRUE),
      carries = dplyr::n(),
      rushing_fumbles = sum(.data$fumble == 1 & .data$fumbled_1_player_id == .data$rusher_player_id & is.na(.data$lateral_rusher_player_id)),
      rushing_fumbles_lost = sum(.data$fumble_lost == 1 & .data$fumbled_1_player_id == .data$rusher_player_id & is.na(.data$lateral_rusher_player_id)),
      rushing_first_downs = sum(.data$first_down_rush & is.na(.data$lateral_rusher_player_id)),
      rushing_epa = sum(.data$epa, na.rm = TRUE),
      hvts = sum(.data$hvt, na.rm = TRUE)
    ) %>% 
    rename(team = .data$posteam) %>% 
    dplyr::ungroup()
  
  # rush df 2: lateral
  laterals <- data %>%
    dplyr::filter(!is.na(.data$lateral_rusher_player_id)) %>%
    dplyr::group_by(.data$posteam, .data$week, .data$season) %>% 
    dplyr::mutate(
      lateral_hvt = ifelse(yardline_100 <= 10, 1, 0)
    ) %>% 
    dplyr::summarize(
      lateral_hvts = sum(.data$lateral_hvt, na.rm = TRUE) %>% as.double(),
      lateral_yards = sum(.data$lateral_rushing_yards, na.rm = TRUE),
      lateral_fds = sum(.data$first_down_rush, na.rm = TRUE),
      lateral_tds = sum(.data$td_player_id == .data$lateral_rusher_player_id, na.rm = TRUE),
      lateral_att = dplyr::n(),
      lateral_fumbles =  sum(.data$fumble, na.rm = TRUE),
      lateral_fumbles_lost = sum(.data$fumble_lost, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::rename(team = .data$posteam) %>%
    dplyr::bind_rows(
      mult_lats %>%
        dplyr::filter(
          .data$type == "lateral_rushing" & .data$season %in% data$season & .data$week %in% data$week
        ) %>%
        dplyr::select("season", "week", "team" = .data$team_abbr, "lateral_yards" = .data$yards) %>%
        dplyr::mutate(lateral_tds = 0L, lateral_att = 1L)
    )
  
  # rush df: join
  rush_df <- rushes %>%
    dplyr::left_join(laterals, by = c("team", "week", "season")) %>%
    dplyr::mutate(
      lateral_hvts = dplyr::if_else(is.na(.data$lateral_hvts), 0, .data$lateral_hvts),
      lateral_yards = dplyr::if_else(is.na(.data$lateral_yards), 0, .data$lateral_yards),
      lateral_tds = dplyr::if_else(is.na(.data$lateral_tds), 0L, .data$lateral_tds),
      lateral_fumbles = dplyr::if_else(is.na(.data$lateral_fumbles), 0, .data$lateral_fumbles),
      lateral_fumbles_lost = dplyr::if_else(is.na(.data$lateral_fumbles_lost), 0, .data$lateral_fumbles_lost),
      lateral_fds = dplyr::if_else(is.na(.data$lateral_fds), 0, .data$lateral_fds)
    ) %>%
    dplyr::mutate(
      hvts = .data$hvts + .data$lateral_hvts,
      rushing_yards = .data$yards + .data$lateral_yards,
      rushing_tds = .data$tds + .data$lateral_tds,
      rushing_first_downs = .data$rushing_first_downs + .data$lateral_fds,
      rushing_fumbles = .data$rushing_fumbles + .data$lateral_fumbles,
      rushing_fumbles_lost = .data$rushing_fumbles_lost + .data$lateral_fumbles_lost,
      hvt_percentage = hvts / carries
    ) %>%
    # dplyr::rename(team = .data$posteam) %>%
    dplyr::select("team", "week", "season", 
                  "rushing_yards", "carries", "rushing_tds", "rushing_fumbles",
                  "rushing_fumbles_lost", "rushing_first_downs", "rushing_epa", "hvts") %>%
    dplyr::ungroup()
  
  rush_two_points <- two_points %>%
    dplyr::filter(.data$rush_attempt == 1) %>%
    dplyr::group_by(.data$posteam, .data$week, .data$season) %>%
    dplyr::summarise(
      # need name_rush and team_rush here for the full join in the next pipe
      rushing_2pt_conversions = dplyr::n()
    ) %>%
    dplyr::rename(team = .data$posteam) %>%
    dplyr::ungroup()
  
  rush_df <- rush_df %>%
    # need a full join because players without rushing stats that recorded
    # a rushing two point (mostly QBs) are dropped in any other join
    dplyr::full_join(rush_two_points, by = c("team", "week", "season")) %>%
    dplyr::mutate(rushing_2pt_conversions = dplyr::if_else(is.na(.data$rushing_2pt_conversions), 0L, .data$rushing_2pt_conversions)) %>%
    dplyr::filter(!is.na(.data$team))
  
  rush_df_nas <- is.na(rush_df)
  epa_index <- which(dimnames(rush_df_nas)[[2]] == "rushing_epa")
  rush_df_nas[,epa_index] <- c(FALSE)
  
  rush_df[rush_df_nas] <- 0
  
  # Receiving stats ---------------------------------------------------------
  
  # receiver df 1: primary receiver
  rec <- data %>%
    dplyr::filter(!is.na(.data$receiver_player_id)) %>% 
    dplyr::mutate(
      receiving_redzone_target = ifelse(yardline_100 <= 35, 1, 0)
    ) %>% 
    dplyr::group_by(.data$posteam, .data$week, .data$season) %>%
    dplyr::summarize(
      yards = sum(.data$receiving_yards, na.rm = TRUE),
      receptions = sum(.data$complete_pass == 1),
      targets = dplyr::n(),
      receiving_receiving_redzone_targets = sum(.data$receiving_redzone_target, na.rm = TRUE),
      tds = sum(.data$td_player_id == .data$receiver_player_id, na.rm = TRUE),
      receiving_fumbles = sum(.data$fumble == 1 & .data$fumbled_1_player_id == .data$receiver_player_id & is.na(.data$lateral_receiver_player_id)),
      receiving_fumbles_lost = sum(.data$fumble_lost == 1 & .data$fumbled_1_player_id == .data$receiver_player_id & is.na(.data$lateral_receiver_player_id)),
      receiving_air_yards = sum(.data$air_yards, na.rm = TRUE),
      receiving_yards_after_catch = sum(.data$yards_after_catch, na.rm = TRUE),
      receiving_first_downs = sum(.data$first_down_pass & is.na(.data$lateral_receiver_player_id)),
      receiving_epa = sum(.data$epa, na.rm = TRUE)
    ) %>%
    rename(team = .data$posteam) %>% 
    dplyr::ungroup()
  
  # receiver df 2: lateral
  laterals <- data %>%
    dplyr::filter(!is.na(.data$lateral_receiver_player_id)) %>%
    dplyr::mutate(
      lateral_receiving_redzone_target = ifelse(yardline_100 <= 25, 1, 0)
    ) %>% 
    dplyr::group_by(.data$posteam, .data$week, .data$season) %>%
    dplyr::summarize(
      lateral_receiving_receiving_redzone_targets = sum(.data$lateral_receiving_redzone_target, na.rm = TRUE),
      lateral_yards = sum(.data$lateral_receiving_yards, na.rm = TRUE),
      lateral_tds = sum(.data$td_player_id == .data$lateral_receiver_player_id, na.rm = TRUE),
      lateral_att = dplyr::n(),
      lateral_fds = sum(.data$first_down_pass, na.rm = TRUE),
      lateral_fumbles = sum(.data$fumble, na.rm = TRUE),
      lateral_fumbles_lost = sum(.data$fumble_lost, na.rm = TRUE)
    ) %>%
    rename(team = .data$posteam) %>% 
    dplyr::ungroup() %>%
    # dplyr::rename(team = .data$posteam) %>%
    dplyr::bind_rows(
      mult_lats %>%
        dplyr::filter(
          .data$type == "lateral_receiving" & .data$season %in% data$season & .data$week %in% data$week
        ) %>%
        dplyr::select("season", "week", "team" = .data$team_abbr, "lateral_yards" = .data$yards) %>%
        dplyr::mutate(lateral_tds = 0L, lateral_att = 1L)
    ) %>% 
    group_by(team, week, season) %>% 
    summarize(
      lateral_receiving_receiving_redzone_targets = sum(.data$lateral_receiving_receiving_redzone_targets, na.rm = TRUE),
      lateral_yards = sum(.data$lateral_yards, na.rm = TRUE),
      lateral_tds = sum(.data$lateral_tds, na.rm = TRUE),
      lateral_att = sum(.data$lateral_att, na.rm = TRUE),
      lateral_fds = sum(.data$lateral_fds, na.rm = TRUE),
      lateral_fumbles = sum(.data$lateral_fumbles, na.rm = TRUE),
      lateral_fumbles_lost = sum(.data$lateral_fumbles_lost, na.rm = TRUE)
    )
  
  # receiver df 3: team receiving for WOPR
  rec_team <- data %>%
    dplyr::filter(!is.na(.data$receiver_player_id)) %>%
    dplyr::group_by(.data$posteam, .data$week, .data$season) %>%
    dplyr::summarize(
      team_targets = dplyr::n(),
      team_air_yards = sum(.data$air_yards, na.rm = TRUE),
    ) %>%
    rename(team = .data$posteam) %>% 
    dplyr::ungroup()
  
  # rec df: join
  rec_df <- rec %>%
    dplyr::left_join(laterals, by = c("team", "week", "season")) %>%
    dplyr::left_join(rec_team, by = c("team", "week", "season")) %>%
    dplyr::mutate(
      lateral_receiving_receiving_redzone_targets = dplyr::if_else(is.na(.data$lateral_receiving_receiving_redzone_targets), 0, .data$lateral_receiving_receiving_redzone_targets),
      lateral_yards = dplyr::if_else(is.na(.data$lateral_yards), 0, .data$lateral_yards),
      lateral_tds = dplyr::if_else(is.na(.data$lateral_tds), 0L, .data$lateral_tds),
      lateral_fumbles = dplyr::if_else(is.na(.data$lateral_fumbles), 0, .data$lateral_fumbles),
      lateral_fumbles_lost = dplyr::if_else(is.na(.data$lateral_fumbles_lost), 0, .data$lateral_fumbles_lost),
      lateral_fds = dplyr::if_else(is.na(.data$lateral_fds), 0, .data$lateral_fds)
    ) %>%
    dplyr::mutate(
      receiving_receiving_redzone_targets = .data$receiving_receiving_redzone_targets + .data$lateral_receiving_receiving_redzone_targets,
      receiving_yards = .data$yards + .data$lateral_yards,
      receiving_tds = .data$tds + .data$lateral_tds,
      receiving_yards_after_catch = .data$receiving_yards_after_catch + .data$lateral_yards,
      receiving_first_downs = .data$receiving_first_downs + .data$lateral_fds,
      receiving_fumbles = .data$receiving_fumbles + .data$lateral_fumbles,
      receiving_fumbles_lost = .data$receiving_fumbles_lost + .data$lateral_fumbles_lost,
      racr = .data$receiving_yards / .data$receiving_air_yards
    ) %>%
    # dplyr::rename(team = .data$posteam) %>%
    dplyr::select("team", "week", "season", 
                  "receiving_yards", "receiving_air_yards", "receiving_yards_after_catch",
                  "receptions", "targets", "receiving_tds", "receiving_fumbles",
                  "receiving_fumbles_lost", "receiving_first_downs", "receiving_epa",
                  "racr", "receiving_receiving_redzone_targets")
  
  rec_two_points <- two_points %>%
    dplyr::filter(.data$pass_attempt == 1) %>%
    dplyr::group_by(.data$posteam, .data$week, .data$season) %>%
    dplyr::summarise(
      receiving_2pt_conversions = dplyr::n()
    ) %>%
    dplyr::rename(team = .data$posteam) %>%
    dplyr::ungroup()
  
  rec_df <- rec_df %>%
    # need a full join because players without receiving stats that recorded
    # a receiving two point are dropped in any other join
    dplyr::full_join(rec_two_points, by = c("team", "week", "season")) %>%
    dplyr::mutate(receiving_2pt_conversions = dplyr::if_else(is.na(.data$receiving_2pt_conversions), 0L, .data$receiving_2pt_conversions)) %>%
    dplyr::filter(!is.na(.data$team))
  
  rec_df_nas <- is.na(rec_df)
  epa_index <- which(dimnames(rec_df_nas)[[2]] == c("receiving_epa", "racr"))
  rec_df_nas[,epa_index] <- c(FALSE)
  
  rec_df[rec_df_nas] <- 0
  
  
  # Special Teams -----------------------------------------------------------
  
  st_tds <- pbp %>%
    dplyr::filter(.data$special == 1 & !is.na(.data$td_player_id)) %>%
    dplyr::group_by(.data$posteam, .data$week, .data$season) %>%
    dplyr::summarise(
      special_teams_tds = sum(.data$touchdown, na.rm = TRUE)
    ) %>%
    dplyr::rename(team = .data$posteam)
  
  
  # Snap Counts -------------------------------------------------------------
  
  
  
  # Combine all stats -------------------------------------------------------
  
  # combine all the stats together
  team_df <- pass_df %>%
    dplyr::full_join(rush_df, by = c("team", "week", "season")) %>%
    dplyr::full_join(rec_df, by = c("team", "week", "season")) %>%
    dplyr::full_join(st_tds, by = c("team", "week", "season")) %>%
    dplyr::left_join(s_type, by = c("season", "week")) %>% 
    dplyr::left_join(data %>% select(season, game_id, week, team = posteam) %>% unique(), by = c('season', 'team', 'week')) %>% 
    dplyr::select(tidyselect::any_of(c(
      
      # id information
      "team", "season", "week", "game_id", "season_type", # 'offense_snaps', 'defense_snaps',
      
      # passing stats
      "completions", "attempts", "passing_yards", "passing_tds", "interceptions",
      "sacks", "sack_yards", "sack_fumbles", "sack_fumbles_lost", "passing_air_yards", "passing_yards_after_catch",
      "passing_first_downs", "passing_epa", "passing_2pt_conversions", "pacr", "dakota",
      
      # rushing stats
      "carries", "rushing_yards", "rushing_tds", "rushing_fumbles", "rushing_fumbles_lost",
      "rushing_first_downs", "rushing_epa", "rushing_2pt_conversions", "hvts",
      
      # receiving stats
      "receptions", "targets", "receiving_yards", "receiving_tds", "receiving_fumbles",
      "receiving_fumbles_lost", "receiving_air_yards", "receiving_yards_after_catch",
      "receiving_first_downs", "receiving_epa", "receiving_2pt_conversions", "racr",
      "receiving_receiving_redzone_targets",
      
      # special teams
      "special_teams_tds"
      
    ))) %>%
    dplyr::filter(!is.na(.data$team))
  
  team_df_nas <- is.na(team_df)
  epa_index <- which(dimnames(team_df_nas)[[2]] %in% c("passing_epa", "rushing_epa", "receiving_epa", "dakota", "racr", "target_share", "air_yards_share", "wopr", "pacr"))
  team_df_nas[,epa_index] <- c(FALSE)
  
  team_df[team_df_nas] <- 0
  
  team_df <- team_df %>%
    dplyr::mutate(
      fantasy_points =
        1 / 25 * .data$passing_yards +
        4 * .data$passing_tds +
        -2 * .data$interceptions +
        1 / 10 * (.data$rushing_yards + .data$receiving_yards) +
        6 * (.data$rushing_tds + .data$receiving_tds + .data$special_teams_tds) +
        2 * (.data$passing_2pt_conversions + .data$rushing_2pt_conversions + .data$receiving_2pt_conversions) +
        -2 * (.data$sack_fumbles_lost + .data$rushing_fumbles_lost + .data$receiving_fumbles_lost),
      
      fantasy_points_ppr = .data$fantasy_points + .data$receptions,
      fantasy_points_half_ppr = .data$fantasy_points + (.data$receptions * .5)
    ) %>%
    dplyr::arrange(.data$team, .data$season, .data$week)
  
  
  # if user doesn't want week-by-week input, aggregate the whole df
  if (isFALSE(weekly)) {
    team_df <- team_df %>%
      dplyr::group_by(.data$team) %>%
      dplyr::summarise(
        games = dplyr::n(),
        # passing
        completions = sum(.data$completions),
        attempts = sum(.data$attempts),
        passing_yards = sum(.data$passing_yards),
        passing_tds = sum(.data$passing_tds),
        interceptions = sum(.data$interceptions),
        sacks = sum(.data$sacks),
        sack_yards = sum(.data$sack_yards),
        sack_fumbles = sum(.data$sack_fumbles),
        sack_fumbles_lost = sum(.data$sack_fumbles_lost),
        passing_air_yards = sum(.data$passing_air_yards),
        passing_yards_after_catch = sum(.data$passing_yards_after_catch),
        passing_first_downs = sum(.data$passing_first_downs),
        passing_epa = dplyr::if_else(all(is.na(.data$passing_epa)), NA_real_, sum(.data$passing_epa, na.rm = TRUE)),
        passing_2pt_conversions = sum(.data$passing_2pt_conversions),
        pacr = .data$passing_yards / .data$passing_air_yards,
        
        # rushing
        carries = sum(.data$carries),
        rushing_yards = sum(.data$rushing_yards),
        rushing_tds = sum(.data$rushing_tds),
        rushing_fumbles = sum(.data$rushing_fumbles),
        rushing_fumbles_lost = sum(.data$rushing_fumbles_lost),
        rushing_first_downs = sum(.data$rushing_first_downs),
        rushing_epa = dplyr::if_else(all(is.na(.data$rushing_epa)), NA_real_, sum(.data$rushing_epa, na.rm = TRUE)),
        rushing_2pt_conversions = sum(.data$rushing_2pt_conversions),
        hvts = sum(.data$hvts, na.rm = TRUE),
        
        # receiving
        receptions = sum(.data$receptions),
        targets = sum(.data$targets),
        receiving_receiving_redzone_targets = sum(.data$receiving_receiving_redzone_targets),
        receiving_yards = sum(.data$receiving_yards),
        receiving_tds = sum(.data$receiving_tds),
        receiving_fumbles = sum(.data$receiving_fumbles),
        receiving_fumbles_lost = sum(.data$receiving_fumbles_lost),
        receiving_air_yards = sum(.data$receiving_air_yards),
        receiving_yards_after_catch = sum(.data$receiving_yards_after_catch),
        receiving_first_downs = sum(.data$receiving_first_downs),
        receiving_epa = dplyr::if_else(all(is.na(.data$receiving_epa)), NA_real_, sum(.data$receiving_epa, na.rm = TRUE)),
        receiving_2pt_conversions = sum(.data$receiving_2pt_conversions),
        racr = .data$receiving_yards / .data$receiving_air_yards,
        
        # special teams
        special_teams_tds = sum(.data$special_teams_tds),
        
        # fantasy
        fantasy_points = sum(.data$fantasy_points),
        fantasy_points_ppr = sum(.data$fantasy_points_ppr),
        fantasy_points_half_ppr = sum(.data$fantasy_points_half_ppr)
      ) %>%
      dplyr::ungroup() %>% 
      dplyr::mutate(
        hvt_percentage = hvts / carries,
        hvt_per_game = hvts / games
      ) %>% 
      add_team_dakota(pbp = pbp, weekly = weekly) %>%
      dplyr::select(
        .data$team:.data$pacr,
        .data$dakota,
        dplyr::everything()
      )
  }
  
  return(team_df)
}

# XYAC transform ----------------------------------------------------------

# Link: https://github.com/nflverse/nflfastR/blob/master/R/helper_add_xyac.R
add_xyac_mod <- function(pbp, ...) {
  if (nrow(pbp) == 0) {
    # user_message("Nothing to do. Return passed data frame.", "info")
  } else {
    # testing only
    # pbp <- g
    
    pbp <- pbp %>% dplyr::select(-tidyselect::contains('xyac'))
    
    # for joining at the end
    pbp <- pbp %>%
      dplyr::mutate(index = 1:dplyr::n())
    
    # prepare_xyac_data helper function shown below
    passes <- prepare_xyac_data(pbp) %>%
      dplyr::filter(.data$valid_pass == 1, .data$distance_to_goal != 0)
    
    if (!nrow(passes) == 0) {
      # user_message("Computing xyac...", "todo")
      join_data <- passes %>%
        dplyr::select(
          "index", "distance_to_goal", "season", "week", "home", "posteam", "roof",
          "half_seconds_remaining", "down", "ydstogo",
          "posteam_timeouts_remaining", "defteam_timeouts_remaining",
          "original_spot" = "yardline_100", "original_ep" = "ep", "air_epa", "air_yards"
        ) %>%
        dplyr::mutate(
          down = as.integer(.data$down),
          ydstogo = as.integer(.data$ydstogo),
          original_ydstogo = .data$ydstogo
        ) %>%
        dplyr::select("index":"ydstogo", "original_ydstogo", dplyr::everything())
      
      xyac_vars <-
        stats::predict(
          fastrmodels::xyac_model,
          as.matrix(passes %>% xyac_model_select())
        ) %>%
        tibble::as_tibble() %>%
        dplyr::rename(prob = "value") %>%
        dplyr::bind_cols(
          tibble::tibble(
            "yac" = rep_len(-5:70, length.out = nrow(passes) * 76),
            "index" = rep(passes$index, times = rep_len(76, length.out = nrow(passes)))
          ) %>%
            dplyr::left_join(join_data, by = "index") %>%
            dplyr::mutate(
              half_seconds_remaining = dplyr::if_else(
                .data$half_seconds_remaining <= 6,
                0,
                .data$half_seconds_remaining - 6
              )
            )
        ) %>%
        dplyr::group_by(.data$index) %>%
        dplyr::mutate(
          max_loss = dplyr::if_else(.data$distance_to_goal < 95, -5, .data$distance_to_goal - 99),
          max_gain = dplyr::if_else(.data$distance_to_goal > 70, 70, .data$distance_to_goal),
          cum_prob = cumsum(.data$prob),
          prob = dplyr::case_when(
            # truncate probs at loss greater than max loss
            .data$yac == .data$max_loss ~ .data$cum_prob,
            # same for gains bigger than possible
            .data$yac == .data$max_gain ~ 1 - dplyr::lag(.data$cum_prob, 1),
            TRUE ~ .data$prob
          ),
          # get end result for each possibility
          yardline_100 = .data$distance_to_goal - .data$yac
        ) %>%
        dplyr::filter(.data$yac >= .data$max_loss, .data$yac <= .data$max_gain) %>%
        dplyr::select(-.data$cum_prob) %>%
        dplyr::mutate(
          posteam_timeouts_pre = .data$posteam_timeouts_remaining,
          defeam_timeouts_pre = .data$defteam_timeouts_remaining,
          gain = .data$original_spot - .data$yardline_100,
          turnover = dplyr::if_else(.data$down == 4 & .data$gain < .data$ydstogo, as.integer(1), as.integer(0)),
          down = dplyr::if_else(.data$gain >= .data$ydstogo, 1, .data$down + 1),
          ydstogo = dplyr::if_else(.data$gain >= .data$ydstogo, 10, .data$ydstogo - .data$gain),
          # possession change if 4th down failed
          down = dplyr::if_else(.data$turnover == 1, as.integer(1), as.integer(.data$down)),
          ydstogo = dplyr::if_else(.data$turnover == 1, as.integer(10), as.integer(.data$ydstogo)),
          # flip yardline_100 and timeouts for turnovers
          yardline_100 = dplyr::if_else(.data$turnover == 1, as.integer(100 - .data$yardline_100), as.integer(.data$yardline_100)),
          posteam_timeouts_remaining = dplyr::if_else(
            .data$turnover == 1,
            .data$defeam_timeouts_pre,
            .data$posteam_timeouts_pre
          ),
          defteam_timeouts_remaining = dplyr::if_else(
            .data$turnover == 1,
            .data$posteam_timeouts_pre,
            .data$defeam_timeouts_pre
          ),
          # ydstogo can't be bigger than yardline
          ydstogo = dplyr::if_else(.data$ydstogo >= .data$yardline_100, as.integer(.data$yardline_100), as.integer(.data$ydstogo))
        ) %>%
        dplyr::ungroup()
      
      pbp <- pbp %>%
        dplyr::left_join(xyac_vars, by = "index") %>%
        dplyr::select(-.data$index)
      
      # message_completed("added xyac variables", ...)
    } else { # means no valid pass plays in the pbp
      pbp <- pbp %>%
        dplyr::mutate(
          xyac_epa = NA_real_,
          xyac_mean_yardage = NA_real_,
          xyac_median_yardage = NA_real_,
          xyac_success = NA_real_,
          xyac_fd = NA_real_
        ) %>%
        dplyr::select(-.data$index)
      # user_message("No non-NA values for xyac calculation detected. xyac variables set to NA", "info")
    }
  }
  
  return(pbp)
}


# Team Dakota -------------------------------------------------------------

add_team_dakota <- function(add_to_this, pbp, weekly) {
  dakota_model <- NULL
  con <- url("https://github.com/nflverse/nflfastR-data/blob/master/models/dakota_model.Rdata?raw=true")
  try(load(con), silent = TRUE)
  close(con)
  
  if (is.null(dakota_model)) {
    user_message("This function needs to download the model data from GitHub. Please check your Internet connection and try again!", "oops")
    return(add_to_this)
  }
  
  if (!"id" %in% names(pbp)) pbp <- clean_pbp(pbp)
  if (!"qb_epa" %in% names(pbp)) pbp <- add_qb_epa(pbp)
  
  suppressMessages({
    df <- pbp %>%
      dplyr::filter(.data$pass == 1 | .data$rush == 1) %>%
      dplyr::filter(!is.na(.data$posteam) & !is.na(.data$qb_epa) & !is.na(.data$id) & !is.na(.data$down)) %>%
      dplyr::mutate(epa = dplyr::if_else(.data$qb_epa < -4.5, -4.5, .data$qb_epa)) %>%
      decode_player_ids() %>% 
      rename(team = .data$posteam)
  })
  
  if (isTRUE(weekly)) {
    relevant_players <- add_to_this %>% 
      dplyr::rename(team = .data$team) %>% 
      dplyr::filter(.data$attempts >= 5) %>%
      dplyr::mutate(filter_id = paste(.data$team, .data$season, .data$week, sep = "_")) %>%
      dplyr::pull(.data$filter_id)
    
    model_data <- df %>% 
      dplyr::group_by(.data$team, .data$week, .data$season) %>%
      dplyr::summarize(
        n_plays = n(),
        epa_per_play = sum(.data$epa) / .data$n_plays,
        cpoe = mean(.data$cpoe, na.rm = TRUE)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(cpoe = dplyr::if_else(is.na(.data$cpoe), 0, .data$cpoe)) %>%
      dplyr::mutate(filter_id = paste(.data$team, .data$season, .data$week, sep = "_")) %>%
      dplyr::filter(.data$filter_id %in% relevant_players)
    
    model_data$dakota <- mgcv::predict.gam(dakota_model, model_data) %>% as.vector()
    
    out <- add_to_this %>% 
      dplyr::left_join(
        model_data %>%
          dplyr::select(.data$team, .data$week, .data$season, .data$dakota),
        by = c("team", "week", "season")
      )
  } else if (isFALSE(weekly)) {
    relevant_players <- add_to_this %>%
      dplyr::filter(.data$attempts >= 5) %>%
      dplyr::pull(.data$team)
    
    model_data <- df %>%
      dplyr::group_by(.data$team) %>%
      dplyr::summarize(
        n_plays = n(),
        epa_per_play = sum(.data$epa) / .data$n_plays,
        cpoe = mean(.data$cpoe, na.rm = TRUE)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(cpoe = dplyr::if_else(is.na(.data$cpoe), 0, .data$cpoe)) %>%
      dplyr::filter(.data$team %in% relevant_players)
    
    model_data$dakota <- mgcv::predict.gam(dakota_model, model_data) %>% as.vector()
    
    out <- add_to_this %>%
      dplyr::left_join(
        model_data %>%
          dplyr::select(.data$team, .data$dakota),
        by = "team"
      )
  }
  return(out)
}