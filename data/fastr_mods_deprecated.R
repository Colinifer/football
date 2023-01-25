

# Stats -------------------------------------------------------------------


# Player Stats ------------------------------------------------------------
player_information <- c(
  "player_id",
  "player_name",
  "recent_team",
  "games",
  "offense_snaps",
  "offense_pct"
)

passing_stats <- c(
  # passing stats
  "completions",
  "attempts",
  "passing_yards",
  "passing_tds",
  "interceptions",
  "sacks",
  "sack_yards",
  "sack_fumbles",
  "sack_fumbles_lost",
  "passing_air_yards",
  "passing_yards_after_catch",
  "passing_first_downs",
  "passing_epa",
  "passing_2pt_conversions",
  "pacr",
  "anya",
  "dakota"
)

receiving_stats <- c(
  # receiving stats
  "receptions",
  "targets",
  "receiving_yards",
  "receiving_tds",
  "receiving_fumbles",
  "receiving_fumbles_lost",
  "receiving_air_yards",
  "receiving_yards_after_catch",
  "receiving_first_downs",
  "receiving_epa",
  "receiving_2pt_conversions",
  "racr",
  "target_share",
  "air_yards_share",
  "wopr",
  "hvt"
)

rushing_stats <- c(
  "carries",
  "rushing_yards",
  "rushing_tds",
  "rushing_fumbles",
  "rushing_fumbles_lost",
  "rushing_first_downs",
  "rushing_epa",
  "rushing_2pt_conversions",
  "hvt"
)

fantasy_stats <- c(
  "fantasy_points",
  "fantasy_points_ppr",
  "fantasy_points_half_ppr"
)

player_ids <- c(
  'player_id',
  'espn_id',
  'sportradar_id',
  'yahoo_id',
  'rotowire_id',
  'pff_id',
  'pfr_id',
  'fantasy_data_id',
  'sleeper_id',
  'position',
  'headshot_url'   
)

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
  
  pbp_season <- pbp |>
    pull(season) |>
    unique()
  
  pbp_games <- pbp |>
    pull(game_id) |>
    unique()
  
  rosters <- nflreadr::load_rosters(pbp_season) |>
    select(player_id = gsis_id,
           contains('_id'),
           position,
           headshot_url)
  
  # Participation data ------------------------------------------------------
  if (all(pbp_season >= 2016, na.rm = TRUE)) {
    participation_df <- nflreadr::load_participation(pbp_season, 
                                                     include_pbp = F)
    
    offense_snaps <- participation_df |> 
      select(old_game_id,
             play_id,
             offense_players
      ) |> 
      separate(col = offense_players, 
               sep = ';',
               into = paste('offense_on', as.character(c(1:11)), sep = '_') 
      ) |> 
      pivot_longer(offense_on_1:offense_on_11,
                   # names_to = "team",
                   values_to = "player") |> 
      filter(!is.na(player) & player != '') |> 
      # group_by(old_game_id, player) |> 
      # count() |> 
      suppressWarnings() |> 
      left_join(pbp,by = c('old_game_id', 'play_id')) |> 
      filter(play_type != 'no_play') |> 
      group_by(old_game_id, player, play_type, qb_dropback, pass_attempt, complete_pass, sack, penalty) |> 
      count()
    
    defense_snaps <- participation_df |> 
      select(old_game_id,
             play_id,
             defense_players
      ) |> 
      separate(col = defense_players, 
               sep = ';',
               into = paste('defense_on', as.character(c(1:11)), sep = '_') 
      ) |> 
      pivot_longer(defense_on_1:defense_on_11,
                   # names_to = "team",
                   values_to = "player") |> 
      filter(!is.na(player) & player != '') |> 
      # group_by(old_game_id, player) |> 
      # count() |> 
      suppressWarnings() |> 
      left_join(pbp,by = c('old_game_id', 'play_id')) |> 
      filter(play_type != 'no_play') |> 
      group_by(old_game_id, player, play_type) |> 
      count()
  }
  
  # Prepare data ------------------------------------------------------------
  
  # load plays with multiple laterals
  con <- url("https://github.com/mrcaseb/nfl-data/blob/master/data/lateral_yards/multiple_lateral_yards.rds?raw=true")
  mult_lats <- readRDS(con) %>%
    dplyr::mutate(
      season = substr(game_id, 1, 4) %>% as.integer(),
      week = substr(game_id, 6, 7) %>% as.integer()
    ) %>% 
    dplyr::filter(yards != 0) %>%
    # the list includes all plays with multiple laterals
    # and all receivers. Since the last one already is in the
    # pbp data, we have to drop him here so the entry isn't duplicated
    dplyr::group_by(game_id, play_id) %>%
    dplyr::slice(seq_len(dplyr::n() - 1)) %>%
    dplyr::ungroup()
  close(con)
  
  # filter down to the 2 dfs we need
  suppressMessages({
    # 1. for "normal" plays: get plays that count in official stats
    data <- pbp %>%
      dplyr::filter(!is.na(down),
                    play_type %in% c("pass", "qb_kneel", "qb_spike", "run")) %>%
      decode_player_ids()
    
    if (!"qb_epa" %in% names(data)) data <- add_qb_epa(data)
    
    # 2. for 2pt conversions only, get those plays
    two_points <- pbp %>%
      dplyr::filter(two_point_conv_result == "success") %>%
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
          play_type %in% c("extra_point","field_goal","kickoff","punt"),
          1, 0
        )
      )
  }
  
  s_type <- pbp %>%
    dplyr::select(season, season_type, week) %>%
    dplyr::distinct()
  
  # Get data seasons
  data_seasons <- pbp %>% pull(season) %>% unique()
  
  # load gsis_ids of FBs and RBs for RACR
  racr_ids <- nflreadr::qs_from_url("https://github.com/nflverse/nflfastR-roster/raw/master/data/nflfastR-RB_ids.qs")
  
  # Passing stats -----------------------------------------------------------
  
  # get passing stats
  pass_df <- data %>%
    dplyr::filter(play_type %in% c("pass", "qb_spike")) %>%
    dplyr::group_by(passer_player_id, week, season) %>% 
    dplyr::summarize(
      name_pass = dplyr::first(passer_player_name),
      team_pass = dplyr::first(posteam),
      dropbacks = sum(qb_dropback, na.rm = T),
      passing_yards_after_catch = sum((passing_yards - air_yards) * complete_pass, na.rm = TRUE),
      passing_yards = sum(passing_yards, na.rm = TRUE),
      passing_tds = sum(touchdown == 1 & td_team == posteam & complete_pass == 1),
      interceptions = sum(interception),
      attempts = sum(complete_pass == 1 | incomplete_pass == 1 | interception == 1),
      completions = sum(complete_pass == 1),
      sack_fumbles = sum(fumble == 1 & fumbled_1_player_id == passer_player_id),
      sack_fumbles_lost = sum(fumble_lost == 1 & fumbled_1_player_id == passer_player_id),
      passing_air_yards = sum(air_yards, na.rm = TRUE),
      sacks = sum(sack),
      sack_yards = -1*sum(yards_gained * sack),
      passing_first_downs = sum(first_down_pass),
      passing_epa = sum(qb_epa, na.rm = TRUE),
      pacr = passing_yards / passing_air_yards,
      pacr = dplyr::case_when(
        is.nan(pacr) ~ NA_real_,
        passing_air_yards <= 0 ~ 0,
        TRUE ~ pacr
      ),
      anya = (passing_yards - sack_yards + (20 * passing_tds) - (45 * interceptions)) / (attempts + sacks)
    ) %>%
    dplyr::rename(player_id = passer_player_id) %>%
    dplyr::ungroup()
  
  if (isTRUE(weekly)) pass_df <- add_dakota(pass_df, pbp = pbp, weekly = weekly)
  
  pass_two_points <- two_points %>%
    dplyr::filter(pass_attempt == 1) %>%
    dplyr::group_by(passer_player_id, week, season) %>%
    dplyr::summarise(
      # need name_pass and team_pass here for the full join in the next pipe
      name_pass = custom_mode(passer_player_name),
      team_pass = custom_mode(posteam),
      passing_2pt_conversions = dplyr::n()
    ) %>%
    dplyr::rename(player_id = passer_player_id) %>%
    dplyr::ungroup()
  
  pass_df <- pass_df %>%
    # need a full join because players without passing stats that recorded
    # a passing two point (e.g. WRs) are dropped in any other join
    dplyr::full_join(pass_two_points, by = c("player_id", "week", "season", "name_pass", "team_pass")) %>%
    dplyr::mutate(passing_2pt_conversions = dplyr::if_else(is.na(passing_2pt_conversions), 0L, passing_2pt_conversions)) %>%
    dplyr::filter(!is.na(player_id))
  
  pass_df_nas <- is.na(pass_df)
  epa_index <- which(dimnames(pass_df_nas)[[2]] %in% c("passing_epa", "dakota", "pacr"))
  pass_df_nas[,epa_index] <- c(FALSE)
  
  pass_df[pass_df_nas] <- 0
  
  # Rushing stats -----------------------------------------------------------
  
  # rush df 1: primary rusher
  rushes <- data %>%
    dplyr::filter(play_type %in% c("run", "qb_kneel")) %>%
    dplyr::group_by(rusher_player_id, week, season) %>% 
    dplyr::mutate(
      hvt = ifelse(yardline_100 <= 10, 1, 0)
    ) %>% 
    dplyr::summarize(
      name_rush = dplyr::first(rusher_player_name),
      team_rush = dplyr::first(posteam),
      yards = sum(rushing_yards, na.rm = TRUE),
      tds = sum(td_player_id == rusher_player_id, na.rm = TRUE),
      scrambles = sum(qb_scramble, na.rm = T),
      carries = dplyr::n(),
      rushing_fumbles = sum(fumble == 1 & fumbled_1_player_id == rusher_player_id & is.na(lateral_rusher_player_id)),
      rushing_fumbles_lost = sum(fumble_lost == 1 & fumbled_1_player_id == rusher_player_id & is.na(lateral_rusher_player_id)),
      rushing_first_downs = sum(first_down_rush & is.na(lateral_rusher_player_id)),
      rushing_epa = sum(epa, na.rm = TRUE),
      hvt = sum(hvt)
    ) %>%
    dplyr::ungroup()
  
  # rush df 2: lateral
  laterals <- data %>%
    dplyr::filter(!is.na(lateral_rusher_player_id)) %>%
    dplyr::group_by(lateral_rusher_player_id, week, season) %>%
    dplyr::summarize(
      lateral_yards = sum(lateral_rushing_yards, na.rm = TRUE),
      lateral_fds = sum(first_down_rush, na.rm = TRUE),
      lateral_tds = sum(td_player_id == lateral_rusher_player_id, na.rm = TRUE),
      lateral_att = dplyr::n(),
      lateral_fumbles =  sum(fumble, na.rm = TRUE),
      lateral_fumbles_lost = sum(fumble_lost, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::rename(rusher_player_id = lateral_rusher_player_id) %>%
    dplyr::bind_rows(
      mult_lats %>%
        dplyr::filter(
          type == "lateral_rushing" & season %in% data$season & week %in% data$week
        ) %>%
        dplyr::select("season", "week", "rusher_player_id" = gsis_player_id, "lateral_yards" = yards) %>%
        dplyr::mutate(lateral_tds = 0L, lateral_att = 1L)
    )
  
  # rush df: join
  rush_df <- rushes %>%
    dplyr::left_join(laterals, by = c("rusher_player_id", "week", "season")) %>%
    dplyr::mutate(
      lateral_yards = dplyr::if_else(is.na(lateral_yards), 0, lateral_yards),
      lateral_tds = dplyr::if_else(is.na(lateral_tds), 0L, lateral_tds),
      lateral_fumbles = dplyr::if_else(is.na(lateral_fumbles), 0, lateral_fumbles),
      lateral_fumbles_lost = dplyr::if_else(is.na(lateral_fumbles_lost), 0, lateral_fumbles_lost),
      lateral_fds = dplyr::if_else(is.na(lateral_fds), 0, lateral_fds)
    ) %>%
    dplyr::mutate(
      rushing_yards = yards + lateral_yards,
      rushing_tds = tds + lateral_tds,
      rushing_first_downs = rushing_first_downs + lateral_fds,
      rushing_fumbles = rushing_fumbles + lateral_fumbles,
      rushing_fumbles_lost = rushing_fumbles_lost + lateral_fumbles_lost,
      hvt_percentage = hvt / carries
    ) %>%
    dplyr::rename(player_id = rusher_player_id) %>%
    dplyr::select("player_id", "week", "season", "name_rush", "team_rush",
                  "rushing_yards", "carries", "rushing_tds", "rushing_fumbles",
                  "rushing_fumbles_lost", "rushing_first_downs", "rushing_epa", "hvt") %>%
    dplyr::ungroup()
  
  rush_two_points <- two_points %>%
    dplyr::filter(rush_attempt == 1) %>%
    dplyr::group_by(rusher_player_id, week, season) %>%
    dplyr::summarise(
      # need name_rush and team_rush here for the full join in the next pipe
      name_rush = custom_mode(rusher_player_name),
      team_rush = custom_mode(posteam),
      rushing_2pt_conversions = dplyr::n()
    ) %>%
    dplyr::rename(player_id = rusher_player_id) %>%
    dplyr::ungroup()
  
  rush_df <- rush_df %>%
    # need a full join because players without rushing stats that recorded
    # a rushing two point (mostly QBs) are dropped in any other join
    dplyr::full_join(rush_two_points, by = c("player_id", "week", "season", "name_rush", "team_rush")) %>%
    dplyr::mutate(rushing_2pt_conversions = dplyr::if_else(is.na(rushing_2pt_conversions), 0L, rushing_2pt_conversions)) %>%
    dplyr::filter(!is.na(player_id))
  
  rush_df_nas <- is.na(rush_df)
  epa_index <- which(dimnames(rush_df_nas)[[2]] == "rushing_epa")
  rush_df_nas[,epa_index] <- c(FALSE)
  
  rush_df[rush_df_nas] <- 0
  
  # Receiving stats ---------------------------------------------------------
  
  # receiver df 1: primary receiver
  rec <- data %>%
    dplyr::filter(!is.na(receiver_player_id)) %>% 
    dplyr::mutate(
      hvt = ifelse(yardline_100 <= 10, 1, 0)
    ) %>% 
    dplyr::group_by(receiver_player_id, week, season) %>%
    dplyr::summarize(
      name_receiver = dplyr::first(receiver_player_name),
      team_receiver = dplyr::first(posteam),
      yards = sum(receiving_yards, na.rm = TRUE),
      receptions = sum(complete_pass == 1),
      targets = dplyr::n(),
      tds = sum(td_player_id == receiver_player_id, na.rm = TRUE),
      receiving_fumbles = sum(fumble == 1 & fumbled_1_player_id == receiver_player_id & is.na(lateral_receiver_player_id)),
      receiving_fumbles_lost = sum(fumble_lost == 1 & fumbled_1_player_id == receiver_player_id & is.na(lateral_receiver_player_id)),
      receiving_air_yards = sum(air_yards, na.rm = TRUE),
      receiving_yards_after_catch = sum(yards_after_catch, na.rm = TRUE),
      receiving_first_downs = sum(first_down_pass & is.na(lateral_receiver_player_id)),
      receiving_epa = sum(epa, na.rm = TRUE)
    ) %>%
    dplyr::ungroup()
  
  # receiver df 2: lateral
  laterals <- data %>%
    dplyr::filter(!is.na(lateral_receiver_player_id)) %>%
    dplyr::mutate(
      hvt = ifelse(yardline_100 <= 10, 1, 0)
    ) %>% 
    dplyr::group_by(lateral_receiver_player_id, week, season) %>%
    dplyr::summarize(
      lateral_yards = sum(lateral_receiving_yards, na.rm = TRUE),
      lateral_tds = sum(td_player_id == lateral_receiver_player_id, na.rm = TRUE),
      lateral_att = dplyr::n(),
      lateral_fds = sum(first_down_pass, na.rm = T),
      lateral_fumbles = sum(fumble, na.rm = T),
      lateral_fumbles_lost = sum(fumble_lost, na.rm = T)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::rename(receiver_player_id = lateral_receiver_player_id) %>%
    dplyr::bind_rows(
      mult_lats %>%
        dplyr::filter(
          type == "lateral_receiving" & season %in% data$season & week %in% data$week
        ) %>%
        dplyr::select("season", "week", "receiver_player_id" = gsis_player_id, "lateral_yards" = yards) %>%
        dplyr::mutate(lateral_tds = 0L, lateral_att = 1L)
    )
  
  # receiver df 3: team receiving for WOPR
  rec_team <- data %>%
    dplyr::filter(!is.na(receiver_player_id)) %>%
    dplyr::group_by(posteam, week, season) %>%
    dplyr::summarize(
      team_targets = dplyr::n(),
      team_air_yards = sum(air_yards, na.rm = TRUE),
    ) %>%
    dplyr::ungroup()
  
  # rec df: join
  rec_df <- rec %>%
    dplyr::left_join(laterals, by = c("receiver_player_id", "week", "season")) %>%
    dplyr::left_join(rec_team, by = c("team_receiver" = "posteam", "week", "season")) %>%
    dplyr::mutate(
      lateral_yards = dplyr::if_else(is.na(lateral_yards), 0, lateral_yards),
      lateral_tds = dplyr::if_else(is.na(lateral_tds), 0L, lateral_tds),
      lateral_fumbles = dplyr::if_else(is.na(lateral_fumbles), 0, lateral_fumbles),
      lateral_fumbles_lost = dplyr::if_else(is.na(lateral_fumbles_lost), 0, lateral_fumbles_lost),
      lateral_fds = dplyr::if_else(is.na(lateral_fds), 0, lateral_fds)
    ) %>%
    dplyr::mutate(
      receiving_yards = yards + lateral_yards,
      receiving_tds = tds + lateral_tds,
      receiving_yards_after_catch = receiving_yards_after_catch + lateral_yards,
      receiving_first_downs = receiving_first_downs + lateral_fds,
      receiving_fumbles = receiving_fumbles + lateral_fumbles,
      receiving_fumbles_lost = receiving_fumbles_lost + lateral_fumbles_lost,
      racr = receiving_yards / receiving_air_yards,
      racr = dplyr::case_when(
        is.nan(racr) ~ NA_real_,
        receiving_air_yards == 0 ~ 0,
        # following Josh Hermsmeyer's definition, RACR stays < 0 for RBs (and FBs) and is set to
        # 0 for Receivers. The list "racr_ids" includes all known RB and FB gsis_ids
        receiving_air_yards < 0 & !receiver_player_id %in% racr_ids$gsis_id ~ 0,
        TRUE ~ racr
      ),
      target_share = targets / team_targets,
      air_yards_share = receiving_air_yards / team_air_yards,
      wopr = 1.5 * target_share + 0.7 * air_yards_share
    ) %>%
    dplyr::rename(player_id = receiver_player_id) %>%
    dplyr::select("player_id", "week", "season", "name_receiver", "team_receiver",
                  "receiving_yards", "receiving_air_yards", "receiving_yards_after_catch",
                  "receptions", "targets", "receiving_tds", "receiving_fumbles",
                  "receiving_fumbles_lost", "receiving_first_downs", "receiving_epa",
                  "racr", "target_share", "air_yards_share", "wopr")
  
  rec_two_points <- two_points %>%
    dplyr::filter(pass_attempt == 1) %>%
    dplyr::group_by(receiver_player_id, week, season) %>%
    dplyr::summarise(
      # need name_receiver and team_receiver here for the full join in the next pipe
      name_receiver = custom_mode(receiver_player_name),
      team_receiver = custom_mode(posteam),
      receiving_2pt_conversions = dplyr::n()
    ) %>%
    dplyr::rename(player_id = receiver_player_id) %>%
    dplyr::ungroup()
  
  rec_df <- rec_df %>%
    # need a full join because players without receiving stats that recorded
    # a receiving two point are dropped in any other join
    dplyr::full_join(rec_two_points, by = c("player_id", "week", "season", "name_receiver", "team_receiver")) %>%
    dplyr::mutate(receiving_2pt_conversions = dplyr::if_else(is.na(receiving_2pt_conversions), 0L, receiving_2pt_conversions)) %>%
    dplyr::filter(!is.na(player_id))
  
  rec_df_nas <- is.na(rec_df)
  epa_index <- which(dimnames(rec_df_nas)[[2]] == c("receiving_epa", "racr", "target_share", "air_yards_share", "wopr"))
  rec_df_nas[,epa_index] <- c(FALSE)
  
  rec_df[rec_df_nas] <- 0
  
  
  # Special Teams -----------------------------------------------------------
  
  st_tds <- pbp %>%
    dplyr::filter(special == 1 & !is.na(td_player_id)) %>%
    dplyr::group_by(td_player_id, week, season) %>%
    dplyr::summarise(
      name_st = custom_mode(td_player_name),
      team_st = custom_mode(td_team),
      special_teams_tds = sum(touchdown, na.rm = TRUE)
    ) %>%
    dplyr::rename(player_id = td_player_id)
  
  
  # Combine all stats -------------------------------------------------------
  
  if (length(which(data_seasons >= 2013)) > 0) {
    snaps_df <-
      nflreadr::load_snap_counts(seasons = data_seasons[data_seasons >= 2013]) %>%
      left_join(roster_df %>%
                  select(gsis_id, pfr_id, first_name, last_name),
                by = c('pfr_player_id' = 'pfr_id')) %>% 
      left_join(schedule_df %>% 
                  select(game_id, week),
                by = c('game_id', 'week')) %>% 
      mutate(
        name_snaps = paste0(substr(first_name, 1, 1), '.', last_name),
        offense_snaps = offense_snaps %>% as.integer(),
        offense_pct = offense_pct,
        defense_snaps = defense_snaps %>% as.integer(),
        defense_pct = defense_pct,
        st_snaps = st_snaps %>% as.integer(),
        st_pct = st_pct
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
             name_snaps = as.character(NA),
             team_snaps = as.integer(NA),
             offense_snaps = as.integer(NA))
  }
  
  if (length(which(data_seasons >= 2016)) > 0) {
    routes_df <- nflreadr::load_participation(seasons = data_seasons[data_seasons >= 2016]) %>%
      select(old_game_id,
             play_id,
             offense_players
      ) |> 
      separate(col = offense_players, 
               sep = ';',
               into = paste('offense_on', as.character(c(1:11)), sep = '_') 
      ) |> 
      pivot_longer(offense_on_1:offense_on_11,
                   # names_to = "team",
                   values_to = "player_id") |> 
      filter(!is.na(player_id) & player_id != '') |> 
      # group_by(old_game_id, player) |> 
      # count() |> 
      suppressWarnings() |> 
      left_join(pbp,by = c('old_game_id', 'play_id')) |> 
      filter(play_type != 'no_play') |> 
      # group_by(season, old_game_id, week, player_id, play_type, qb_dropback, pass_attempt, complete_pass, sack, penalty) |> 
      # count() |> 
      left_join(roster_df %>%
                  select(season, gsis_id, first_name, last_name, position),
                by = c('season', 'player_id' = 'gsis_id')) |> 
      mutate(route_run = case_when(position %in% c('RB', 'TE', 'WR') & qb_dropback == 1 ~ 1,
                                   TRUE ~ 0)
      ) |> 
      group_by(player_id, first_name, last_name, position, week, season) |> 
      summarise(routes_run = sum(route_run, na.rm = T)) |> 
      filter(position %in% c('QB', 'RB', 'TE', 'WR') &
               !is.na(player_id))
  } else {
    routes_df <- tibble() %>%  
      mutate(player_id = as.character(NA),
             first_name = as.character(NA),
             last_name = as.character(NA),
             position = as.character(NA),
             week = as.integer(NA),
             season = as.integer(NA),
             routes_run = as.double(NA))
  }
  
  # Combine all stats -------------------------------------------------------
  
  # combine all the stats together
  player_df <- pass_df %>%
    dplyr::full_join(rush_df, by = c("player_id", "week", "season")) %>%
    dplyr::full_join(rec_df, by = c("player_id", "week", "season")) %>%
    dplyr::full_join(routes_df, by = c("player_id", "week", "season")) %>%
    dplyr::full_join(st_tds, by = c("player_id", "week", "season")) %>% 
    dplyr::full_join(snaps_df, by = c("player_id", "week", "season")) %>% 
    dplyr::left_join(s_type, by = c("season", "week")) %>%
    dplyr::mutate(
      player_name = dplyr::case_when(
        !is.na(name_pass) ~ name_pass,
        !is.na(name_rush) ~ name_rush,
        !is.na(name_receiver) ~ name_receiver,
        !is.na(name_snaps) ~ name_snaps,
        TRUE ~ name_st
      ),
      recent_team = dplyr::case_when(
        !is.na(team_pass) ~ team_pass,
        !is.na(team_rush) ~ team_rush,
        !is.na(team_receiver) ~ team_receiver,
        !is.na(team_snaps) ~ team_snaps,
        TRUE ~ team_st
      )
    ) %>% 
    dplyr::left_join(data %>% select(season, game_id, week, recent_team = posteam) %>% unique(), 
                     by = c('season', 'week', 'recent_team')) %>% 
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
      "routes_run", "receptions", "targets", "receiving_yards", "receiving_tds", "receiving_fumbles",
      "receiving_fumbles_lost", "receiving_air_yards", "receiving_yards_after_catch",
      "receiving_first_downs", "receiving_epa", "receiving_2pt_conversions", "racr",
      "target_share", "air_yards_share", "wopr", "hvt",
      
      # special teams
      "special_teams_tds", "st_snaps", "st_pct"
      
    ))) %>%
    dplyr::filter(!is.na(player_id))
  
  player_df_nas <- is.na(player_df)
  epa_index <- which(dimnames(player_df_nas)[[2]] %in% c("player_name", "recent_team", "season_type", "game_id", "week", "passing_epa", "rushing_epa", "receiving_epa", "dakota", "racr", "target_share", "air_yards_share", "wopr", "pacr"))
  player_df_nas[,epa_index] <- c(FALSE)
  
  player_df[player_df_nas] <- 0
  
  player_df <- player_df %>%
    dplyr::mutate(
      fantasy_points =
        1 / 25 * passing_yards +
        4 * passing_tds +
        -2 * interceptions +
        1 / 10 * (rushing_yards + receiving_yards) +
        6 * (rushing_tds + receiving_tds + special_teams_tds) +
        2 * (passing_2pt_conversions + rushing_2pt_conversions + receiving_2pt_conversions) +
        -2 * (sack_fumbles_lost + rushing_fumbles_lost + receiving_fumbles_lost),
      
      fantasy_points_ppr = fantasy_points + receptions,
      fantasy_points_half_ppr = fantasy_points + (receptions * .5)
    ) %>%
    dplyr::arrange(player_id, season, week)
  
  
  # if user doesn't want week-by-week input, aggregate the whole df
  if (isFALSE(weekly)) {
    player_df <- player_df %>% 
      mutate(
        potential_offense_snaps = offense_snaps / offense_pct,
        potential_st_snaps = st_snaps / st_pct
      ) %>% 
      dplyr::group_by(player_id) %>%
      dplyr::summarise(
        player_name = custom_mode(player_name),
        games = dplyr::n(),
        recent_team = dplyr::last(recent_team),
        offense_snaps = sum(offense_snaps),
        # potential_offense_snaps = sum(potential_offense_snaps), # total potential_snaps
        offense_pct = sum(offense_snaps) / sum(potential_offense_snaps), # calculate total snaps percentage
        # passing
        completions = sum(completions),
        attempts = sum(attempts),
        passing_yards = sum(passing_yards),
        passing_tds = sum(passing_tds),
        interceptions = sum(interceptions),
        sacks = sum(sacks),
        sack_yards = sum(sack_yards),
        sack_fumbles = sum(sack_fumbles),
        sack_fumbles_lost = sum(sack_fumbles_lost),
        passing_air_yards = sum(passing_air_yards),
        passing_yards_after_catch = sum(passing_yards_after_catch),
        passing_first_downs = sum(passing_first_downs),
        passing_epa = dplyr::if_else(all(is.na(passing_epa)), NA_real_, sum(passing_epa, na.rm = TRUE)),
        passing_2pt_conversions = sum(passing_2pt_conversions),
        pacr = passing_yards / passing_air_yards,
        anya = (passing_yards - sack_yards + (20 * passing_tds) - (45 * interceptions)) / (attempts + sacks),
        
        # rushing
        carries = sum(carries),
        rushing_yards = sum(rushing_yards),
        rushing_tds = sum(rushing_tds),
        rushing_fumbles = sum(rushing_fumbles),
        rushing_fumbles_lost = sum(rushing_fumbles_lost),
        rushing_first_downs = sum(rushing_first_downs),
        rushing_epa = dplyr::if_else(all(is.na(rushing_epa)), NA_real_, sum(rushing_epa, na.rm = TRUE)),
        rushing_2pt_conversions = sum(rushing_2pt_conversions),
        hvt = sum(hvt, na.rm = TRUE),
        
        # receiving
        # routes_run = sum(qb_dropbacks),
        receptions = sum(receptions),
        targets = sum(targets),
        receiving_yards = sum(receiving_yards),
        receiving_tds = sum(receiving_tds),
        receiving_fumbles = sum(receiving_fumbles),
        receiving_fumbles_lost = sum(receiving_fumbles_lost),
        receiving_air_yards = sum(receiving_air_yards),
        receiving_yards_after_catch = sum(receiving_yards_after_catch),
        receiving_first_downs = sum(receiving_first_downs),
        receiving_epa = dplyr::if_else(all(is.na(receiving_epa)), NA_real_, sum(receiving_epa, na.rm = TRUE)),
        receiving_2pt_conversions = sum(receiving_2pt_conversions),
        racr = receiving_yards / receiving_air_yards,
        target_share = dplyr::if_else(all(is.na(target_share)), NA_real_, mean(target_share, na.rm = TRUE)),
        air_yards_share = dplyr::if_else(all(is.na(air_yards_share)), NA_real_, mean(air_yards_share, na.rm = TRUE)),
        wopr = 1.5 * target_share + 0.7 * air_yards_share,
        
        # special teams
        special_teams_tds = sum(special_teams_tds),
        st_snaps = sum(st_snaps),
        # potential_st_snaps = sum(potential_st_snaps), # total potential_snaps
        st_pct = sum(st_snaps) / sum(potential_st_snaps), # calculate total snaps percentage
        
        # fantasy
        fantasy_points = sum(fantasy_points),
        fantasy_points_ppr = sum(fantasy_points_ppr),
        fantasy_points_half_ppr = sum(fantasy_points_half_ppr)
      ) %>%
      dplyr::ungroup() %>% 
      dplyr::mutate(
        hvt_percentage = hvt / carries,
        hvt_per_game = hvt / games,
        racr = dplyr::case_when(
          is.nan(racr) ~ NA_real_,
          receiving_air_yards == 0 ~ 0,
          # following Josh Hermsmeyer's definition, RACR stays < 0 for RBs (and FBs) and is set to
          # 0 for Receivers. The list "racr_ids" includes all known RB and FB gsis_ids
          receiving_air_yards < 0 & !player_id %in% racr_ids$gsis_id ~ 0,
          TRUE ~ racr
        ),
        pacr = dplyr::case_when(
          is.nan(pacr) ~ NA_real_,
          passing_air_yards <= 0 ~ 0,
          TRUE ~ pacr
        )
      ) %>% 
      add_dakota(pbp = pbp, weekly = weekly) %>%
      dplyr::select(
        player_id:pacr,
        dakota,
        dplyr::everything()
      )
  }
  
  return(player_df)
}


# Player Stats Update DB --------------------------------------------------

update_player_stats_weekly_db <- function(con = fx.db_con(x.host = 'localhost'), pbp = pbp_df) {
  on.exit(dbDisconnect(con))
  
  table <- 'nflfastR_player_stats_weekly'
  
  existing_ids <- tbl(con, table) |>
    filter(season == nflreadr::get_current_season()) |>
    select(game_id) |>
    collect() |>
    pull(game_id) |> 
    unique()
  
  filtered_pbp <- pbp |> 
    filter(!game_id %in% existing_ids) 
  
  if (nrow(filtered_pbp) > 0) {
    player_stats_weekly <- filtered_pbp |> 
      calculate_player_stats_mod(weekly = TRUE)
    
    filtered_delete_game_ids <- filtered_pbp |> 
      pull(game_id) |> 
      unique()
    
    if (dbExistsTable(con, table) == FALSE){
      dbCreateTable(con, table, player_stats_weekly)
    }
    
    dbExecute(con, 
              glue('DELETE FROM "{table}" WHERE game_id IN ({paste0(as.character(paste0("\'", filtered_delete_game_ids, "\'")), collapse = ", ")});')
    )
    
    dbWriteTable(con, 
                 table, 
                 player_stats_weekly, 
                 append = TRUE)
  } else {
    print('Player Stats is already up-to-date!')
  }
}

update_player_stats_db <- function(con = fx.db_con(x.host = 'localhost'), pbp = pbp_df) {
  on.exit(dbDisconnect(con))
  
  table <- 'nflfastR_player_stats'
  
  player_stats <- pbp |> 
    calculate_player_stats_mod(weekly = FALSE) |> 
    mutate(season = unique(pbp$season)) |> 
    select(season, everything())
  
  delete_seasons <- pbp |> 
    pull(season) |> 
    unique()
  
  dbExecute(con, 
            glue('DELETE FROM "{table}" WHERE season IN ({paste0(as.character(paste0("\'", delete_seasons, "\'")), collapse = ", ")});')
  )
  
  dbWriteTable(con, 
               table, 
               player_stats, 
               append = TRUE)
}

# Team stats --------------------------------------------------------------

calculate_team_stats_mod <- function(pbp, weekly = FALSE) {
  # Prepare data ------------------------------------------------------------
  
  # load plays with multiple laterals
  con <- url("https://github.com/mrcaseb/nfl-data/blob/master/data/lateral_yards/multiple_lateral_yards.rds?raw=true")
  mult_lats <- readRDS(con) %>%
    dplyr::mutate(
      season = substr(game_id, 1, 4) %>% as.integer(),
      week = substr(game_id, 6, 7) %>% as.integer()
    ) %>%
    dplyr::filter(yards != 0) %>%
    # the list includes all plays with multiple laterals
    # and all receivers. Since the last one already is in the
    # pbp data, we have to drop him here so the entry isn't duplicated
    dplyr::group_by(game_id, play_id) %>%
    dplyr::slice(seq_len(dplyr::n() - 1)) %>%
    dplyr::ungroup()
  close(con)
  
  # filter down to the 2 dfs we need
  suppressMessages({
    # 1. for "normal" plays: get plays that count in official stats
    data <- pbp %>%
      dplyr::filter(
        !is.na(down),
        play_type %in% c("pass", "qb_kneel", "qb_spike", "run")
      ) %>%
      decode_player_ids()
    
    if (!"qb_epa" %in% names(data)) data <- add_qb_epa(data)
    
    # 2. for 2pt conversions only, get those plays
    two_points <- pbp %>%
      dplyr::filter(two_point_conv_result == "success") %>%
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
          play_type %in% c("extra_point","field_goal","kickoff","punt"),
          1, 0
        )
      )
  }
  
  s_type <- pbp %>%
    dplyr::select(season, season_type, week) %>%
    dplyr::distinct()
  
  # load gsis_ids of FBs and RBs for RACR
  racr_ids <- nflreadr::qs_from_url("https://github.com/nflverse/nflfastR-roster/raw/master/data/nflfastR-RB_ids.qs")
  
  # Passing stats -----------------------------------------------------------
  
  # get passing stats
  pass_df <- data %>%
    dplyr::filter(play_type %in% c("pass", "qb_spike")) %>%
    dplyr::group_by(posteam, week, season) %>% 
    dplyr::summarize(
      passing_yards_after_catch = sum((passing_yards - air_yards) * complete_pass, na.rm = TRUE),
      passing_yards = sum(passing_yards, na.rm = TRUE),
      passing_tds = sum(touchdown == 1 & td_team == posteam & complete_pass == 1),
      interceptions = sum(interception),
      attempts = sum(complete_pass == 1 | incomplete_pass == 1 | interception == 1),
      completions = sum(complete_pass == 1),
      sack_fumbles = sum(fumble == 1 & fumbled_1_player_id == posteam),
      sack_fumbles_lost = sum(fumble_lost == 1 & fumbled_1_player_id == posteam),
      passing_air_yards = sum(air_yards, na.rm = TRUE),
      sacks = sum(sack),
      sack_yards = -1*sum(yards_gained * sack),
      passing_first_downs = sum(first_down_pass),
      passing_epa = sum(qb_epa, na.rm = TRUE),
      pacr = passing_yards / passing_air_yards
    ) %>%
    dplyr::rename(team = posteam) %>%
    dplyr::ungroup()
  
  if (isTRUE(weekly)) pass_df <- add_team_dakota(pass_df, pbp = pbp, weekly = weekly)
  
  pass_two_points <- two_points %>%
    dplyr::filter(pass_attempt == 1) %>%
    dplyr::group_by(posteam, week, season) %>%
    dplyr::summarise(
      passing_2pt_conversions = dplyr::n()
    ) %>%
    dplyr::rename(team = posteam) %>%
    dplyr::ungroup()
  
  pass_df <- pass_df %>%
    # need a full join because players without passing stats that recorded
    # a passing two point (e.g. WRs) are dropped in any other join
    dplyr::full_join(pass_two_points, by = c("team", "week", "season")) %>%
    dplyr::mutate(passing_2pt_conversions = dplyr::if_else(is.na(passing_2pt_conversions), 0L, passing_2pt_conversions)) %>%
    dplyr::filter(!is.na(team))
  
  pass_df_nas <- is.na(pass_df)
  epa_index <- which(dimnames(pass_df_nas)[[2]] %in% c("passing_epa", "dakota", "pacr"))
  pass_df_nas[,epa_index] <- c(FALSE)
  
  pass_df[pass_df_nas] <- 0
  
  # Rushing stats -----------------------------------------------------------
  
  # rush df 1: primary rusher
  rushes <- data %>%
    dplyr::filter(play_type %in% c("run", "qb_kneel")) %>%
    dplyr::group_by(posteam, week, season) %>% 
    dplyr::mutate(
      hvt = ifelse(yardline_100 <= 10, 1, 0)
    ) %>% 
    dplyr::summarize(
      yards = sum(rushing_yards, na.rm = TRUE),
      tds = sum(td_player_id == rusher_player_id, na.rm = TRUE),
      carries = dplyr::n(),
      rushing_fumbles = sum(fumble == 1 & fumbled_1_player_id == rusher_player_id & is.na(lateral_rusher_player_id)),
      rushing_fumbles_lost = sum(fumble_lost == 1 & fumbled_1_player_id == rusher_player_id & is.na(lateral_rusher_player_id)),
      rushing_first_downs = sum(first_down_rush & is.na(lateral_rusher_player_id)),
      rushing_epa = sum(epa, na.rm = TRUE),
      hvts = sum(hvt, na.rm = TRUE)
    ) %>% 
    rename(team = posteam) %>% 
    dplyr::ungroup()
  
  # rush df 2: lateral
  laterals <- data %>%
    dplyr::filter(!is.na(lateral_rusher_player_id)) %>%
    dplyr::group_by(posteam, week, season) %>% 
    dplyr::mutate(
      lateral_hvt = ifelse(yardline_100 <= 10, 1, 0)
    ) %>% 
    dplyr::summarize(
      lateral_hvts = sum(lateral_hvt, na.rm = TRUE) %>% as.double(),
      lateral_yards = sum(lateral_rushing_yards, na.rm = TRUE),
      lateral_fds = sum(first_down_rush, na.rm = TRUE),
      lateral_tds = sum(td_player_id == lateral_rusher_player_id, na.rm = TRUE),
      lateral_att = dplyr::n(),
      lateral_fumbles =  sum(fumble, na.rm = TRUE),
      lateral_fumbles_lost = sum(fumble_lost, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::rename(team = posteam) %>%
    dplyr::bind_rows(
      mult_lats %>%
        dplyr::filter(
          type == "lateral_rushing" & season %in% data$season & week %in% data$week
        ) %>%
        dplyr::select("season", "week", "team" = team_abbr, "lateral_yards" = yards) %>%
        dplyr::mutate(lateral_tds = 0L, lateral_att = 1L)
    )
  
  # rush df: join
  rush_df <- rushes %>%
    dplyr::left_join(laterals, by = c("team", "week", "season")) %>%
    dplyr::mutate(
      lateral_hvts = dplyr::if_else(is.na(lateral_hvts), 0, lateral_hvts),
      lateral_yards = dplyr::if_else(is.na(lateral_yards), 0, lateral_yards),
      lateral_tds = dplyr::if_else(is.na(lateral_tds), 0L, lateral_tds),
      lateral_fumbles = dplyr::if_else(is.na(lateral_fumbles), 0, lateral_fumbles),
      lateral_fumbles_lost = dplyr::if_else(is.na(lateral_fumbles_lost), 0, lateral_fumbles_lost),
      lateral_fds = dplyr::if_else(is.na(lateral_fds), 0, lateral_fds)
    ) %>%
    dplyr::mutate(
      hvts = hvts + lateral_hvts,
      rushing_yards = yards + lateral_yards,
      rushing_tds = tds + lateral_tds,
      rushing_first_downs = rushing_first_downs + lateral_fds,
      rushing_fumbles = rushing_fumbles + lateral_fumbles,
      rushing_fumbles_lost = rushing_fumbles_lost + lateral_fumbles_lost,
      hvt_percentage = hvts / carries
    ) %>%
    # dplyr::rename(team = posteam) %>%
    dplyr::select("team", "week", "season", 
                  "rushing_yards", "carries", "rushing_tds", "rushing_fumbles",
                  "rushing_fumbles_lost", "rushing_first_downs", "rushing_epa", "hvts") %>%
    dplyr::ungroup()
  
  rush_two_points <- two_points %>%
    dplyr::filter(rush_attempt == 1) %>%
    dplyr::group_by(posteam, week, season) %>%
    dplyr::summarise(
      # need name_rush and team_rush here for the full join in the next pipe
      rushing_2pt_conversions = dplyr::n()
    ) %>%
    dplyr::rename(team = posteam) %>%
    dplyr::ungroup()
  
  rush_df <- rush_df %>%
    # need a full join because players without rushing stats that recorded
    # a rushing two point (mostly QBs) are dropped in any other join
    dplyr::full_join(rush_two_points, by = c("team", "week", "season")) %>%
    dplyr::mutate(rushing_2pt_conversions = dplyr::if_else(is.na(rushing_2pt_conversions), 0L, rushing_2pt_conversions)) %>%
    dplyr::filter(!is.na(team))
  
  rush_df_nas <- is.na(rush_df)
  epa_index <- which(dimnames(rush_df_nas)[[2]] == "rushing_epa")
  rush_df_nas[,epa_index] <- c(FALSE)
  
  rush_df[rush_df_nas] <- 0
  
  # Receiving stats ---------------------------------------------------------
  
  # receiver df 1: primary receiver
  rec <- data %>%
    dplyr::filter(!is.na(receiver_player_id)) %>% 
    dplyr::mutate(
      receiving_redzone_target = ifelse(yardline_100 <= 35, 1, 0)
    ) %>% 
    dplyr::group_by(posteam, week, season) %>%
    dplyr::summarize(
      yards = sum(receiving_yards, na.rm = TRUE),
      receptions = sum(complete_pass == 1),
      targets = dplyr::n(),
      receiving_receiving_redzone_targets = sum(receiving_redzone_target, na.rm = TRUE),
      tds = sum(td_player_id == receiver_player_id, na.rm = TRUE),
      receiving_fumbles = sum(fumble == 1 & fumbled_1_player_id == receiver_player_id & is.na(lateral_receiver_player_id)),
      receiving_fumbles_lost = sum(fumble_lost == 1 & fumbled_1_player_id == receiver_player_id & is.na(lateral_receiver_player_id)),
      receiving_air_yards = sum(air_yards, na.rm = TRUE),
      receiving_yards_after_catch = sum(yards_after_catch, na.rm = TRUE),
      receiving_first_downs = sum(first_down_pass & is.na(lateral_receiver_player_id)),
      receiving_epa = sum(epa, na.rm = TRUE)
    ) %>%
    rename(team = posteam) %>% 
    dplyr::ungroup()
  
  # receiver df 2: lateral
  laterals <- data %>%
    dplyr::filter(!is.na(lateral_receiver_player_id)) %>%
    dplyr::mutate(
      lateral_receiving_redzone_target = ifelse(yardline_100 <= 25, 1, 0)
    ) %>% 
    dplyr::group_by(posteam, week, season) %>%
    dplyr::summarize(
      lateral_receiving_receiving_redzone_targets = sum(lateral_receiving_redzone_target, na.rm = TRUE),
      lateral_yards = sum(lateral_receiving_yards, na.rm = TRUE),
      lateral_tds = sum(td_player_id == lateral_receiver_player_id, na.rm = TRUE),
      lateral_att = dplyr::n(),
      lateral_fds = sum(first_down_pass, na.rm = TRUE),
      lateral_fumbles = sum(fumble, na.rm = TRUE),
      lateral_fumbles_lost = sum(fumble_lost, na.rm = TRUE)
    ) %>%
    rename(team = posteam) %>% 
    dplyr::ungroup() %>%
    # dplyr::rename(team = posteam) %>%
    dplyr::bind_rows(
      mult_lats %>%
        dplyr::filter(
          type == "lateral_receiving" & season %in% data$season & week %in% data$week
        ) %>%
        dplyr::select("season", "week", "team" = team_abbr, "lateral_yards" = yards) %>%
        dplyr::mutate(lateral_tds = 0L, lateral_att = 1L)
    ) %>% 
    group_by(team, week, season) %>% 
    summarize(
      lateral_receiving_receiving_redzone_targets = sum(lateral_receiving_receiving_redzone_targets, na.rm = TRUE),
      lateral_yards = sum(lateral_yards, na.rm = TRUE),
      lateral_tds = sum(lateral_tds, na.rm = TRUE),
      lateral_att = sum(lateral_att, na.rm = TRUE),
      lateral_fds = sum(lateral_fds, na.rm = TRUE),
      lateral_fumbles = sum(lateral_fumbles, na.rm = TRUE),
      lateral_fumbles_lost = sum(lateral_fumbles_lost, na.rm = TRUE)
    )
  
  # receiver df 3: team receiving for WOPR
  rec_team <- data %>%
    dplyr::filter(!is.na(receiver_player_id)) %>%
    dplyr::group_by(posteam, week, season) %>%
    dplyr::summarize(
      team_targets = dplyr::n(),
      team_air_yards = sum(air_yards, na.rm = TRUE),
    ) %>%
    rename(team = posteam) %>% 
    dplyr::ungroup()
  
  # rec df: join
  rec_df <- rec %>%
    dplyr::left_join(laterals, by = c("team", "week", "season")) %>%
    dplyr::left_join(rec_team, by = c("team", "week", "season")) %>%
    dplyr::mutate(
      lateral_receiving_receiving_redzone_targets = ifelse(is.na(lateral_receiving_receiving_redzone_targets), 0, lateral_receiving_receiving_redzone_targets),
      lateral_yards = dplyr::if_else(is.na(lateral_yards), 0, lateral_yards),
      lateral_tds = dplyr::if_else(is.na(lateral_tds), 0L, lateral_tds),
      lateral_fumbles = dplyr::if_else(is.na(lateral_fumbles), 0, lateral_fumbles),
      lateral_fumbles_lost = dplyr::if_else(is.na(lateral_fumbles_lost), 0, lateral_fumbles_lost),
      lateral_fds = dplyr::if_else(is.na(lateral_fds), 0, lateral_fds)
    ) %>%
    dplyr::mutate(
      receiving_receiving_redzone_targets = receiving_receiving_redzone_targets + lateral_receiving_receiving_redzone_targets,
      receiving_yards = yards + lateral_yards,
      receiving_tds = tds + lateral_tds,
      receiving_yards_after_catch = receiving_yards_after_catch + lateral_yards,
      receiving_first_downs = receiving_first_downs + lateral_fds,
      receiving_fumbles = receiving_fumbles + lateral_fumbles,
      receiving_fumbles_lost = receiving_fumbles_lost + lateral_fumbles_lost,
      racr = receiving_yards / receiving_air_yards
    ) %>%
    # dplyr::rename(team = posteam) %>%
    dplyr::select("team", "week", "season", 
                  "receiving_yards", "receiving_air_yards", "receiving_yards_after_catch",
                  "receptions", "targets", "receiving_tds", "receiving_fumbles",
                  "receiving_fumbles_lost", "receiving_first_downs", "receiving_epa",
                  "racr", "receiving_receiving_redzone_targets")
  
  rec_two_points <- two_points %>%
    dplyr::filter(pass_attempt == 1) %>%
    dplyr::group_by(posteam, week, season) %>%
    dplyr::summarise(
      receiving_2pt_conversions = dplyr::n()
    ) %>%
    dplyr::rename(team = posteam) %>%
    dplyr::ungroup()
  
  rec_df <- rec_df %>%
    # need a full join because players without receiving stats that recorded
    # a receiving two point are dropped in any other join
    dplyr::full_join(rec_two_points, by = c("team", "week", "season")) %>%
    dplyr::mutate(receiving_2pt_conversions = dplyr::if_else(is.na(receiving_2pt_conversions), 0L, receiving_2pt_conversions)) %>%
    dplyr::filter(!is.na(team))
  
  rec_df_nas <- is.na(rec_df)
  epa_index <- which(dimnames(rec_df_nas)[[2]] == c("receiving_epa", "racr"))
  rec_df_nas[,epa_index] <- c(FALSE)
  
  rec_df[rec_df_nas] <- 0
  
  
  # Special Teams -----------------------------------------------------------
  
  st_tds <- pbp %>%
    dplyr::filter(special == 1 & !is.na(td_player_id)) %>%
    dplyr::group_by(posteam, week, season) %>%
    dplyr::summarise(
      special_teams_tds = sum(touchdown, na.rm = TRUE)
    ) %>%
    dplyr::rename(team = posteam)
  
  
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
    dplyr::filter(!is.na(team))
  
  team_df_nas <- is.na(team_df)
  epa_index <- which(dimnames(team_df_nas)[[2]] %in% c("passing_epa", "rushing_epa", "receiving_epa", "dakota", "racr", "target_share", "air_yards_share", "wopr", "pacr"))
  team_df_nas[,epa_index] <- c(FALSE)
  
  team_df[team_df_nas] <- 0
  
  team_df <- team_df %>%
    dplyr::mutate(
      fantasy_points =
        1 / 25 * passing_yards +
        4 * passing_tds +
        -2 * interceptions +
        1 / 10 * (rushing_yards + receiving_yards) +
        6 * (rushing_tds + receiving_tds + special_teams_tds) +
        2 * (passing_2pt_conversions + rushing_2pt_conversions + receiving_2pt_conversions) +
        -2 * (sack_fumbles_lost + rushing_fumbles_lost + receiving_fumbles_lost),
      
      fantasy_points_ppr = fantasy_points + receptions,
      fantasy_points_half_ppr = fantasy_points + (receptions * .5)
    ) %>%
    dplyr::arrange(team, season, week)
  
  
  # if user doesn't want week-by-week input, aggregate the whole df
  if (isFALSE(weekly)) {
    team_df <- team_df %>%
      dplyr::group_by(team) %>%
      dplyr::summarise(
        games = dplyr::n(),
        # passing
        completions = sum(completions),
        attempts = sum(attempts),
        passing_yards = sum(passing_yards),
        passing_tds = sum(passing_tds),
        interceptions = sum(interceptions),
        sacks = sum(sacks),
        sack_yards = sum(sack_yards),
        sack_fumbles = sum(sack_fumbles),
        sack_fumbles_lost = sum(sack_fumbles_lost),
        passing_air_yards = sum(passing_air_yards),
        passing_yards_after_catch = sum(passing_yards_after_catch),
        passing_first_downs = sum(passing_first_downs),
        passing_epa = dplyr::if_else(all(is.na(passing_epa)), NA_real_, sum(passing_epa, na.rm = TRUE)),
        passing_2pt_conversions = sum(passing_2pt_conversions),
        pacr = passing_yards / passing_air_yards,
        
        # rushing
        carries = sum(carries),
        rushing_yards = sum(rushing_yards),
        rushing_tds = sum(rushing_tds),
        rushing_fumbles = sum(rushing_fumbles),
        rushing_fumbles_lost = sum(rushing_fumbles_lost),
        rushing_first_downs = sum(rushing_first_downs),
        rushing_epa = dplyr::if_else(all(is.na(rushing_epa)), NA_real_, sum(rushing_epa, na.rm = TRUE)),
        rushing_2pt_conversions = sum(rushing_2pt_conversions),
        hvts = sum(hvts, na.rm = TRUE),
        
        # receiving
        receptions = sum(receptions),
        targets = sum(targets),
        receiving_receiving_redzone_targets = sum(receiving_receiving_redzone_targets),
        receiving_yards = sum(receiving_yards),
        receiving_tds = sum(receiving_tds),
        receiving_fumbles = sum(receiving_fumbles),
        receiving_fumbles_lost = sum(receiving_fumbles_lost),
        receiving_air_yards = sum(receiving_air_yards),
        receiving_yards_after_catch = sum(receiving_yards_after_catch),
        receiving_first_downs = sum(receiving_first_downs),
        receiving_epa = dplyr::if_else(all(is.na(receiving_epa)), NA_real_, sum(receiving_epa, na.rm = TRUE)),
        receiving_2pt_conversions = sum(receiving_2pt_conversions),
        racr = receiving_yards / receiving_air_yards,
        
        # special teams
        special_teams_tds = sum(special_teams_tds),
        
        # fantasy
        fantasy_points = sum(fantasy_points),
        fantasy_points_ppr = sum(fantasy_points_ppr),
        fantasy_points_half_ppr = sum(fantasy_points_half_ppr)
      ) %>%
      dplyr::ungroup() %>% 
      dplyr::mutate(
        hvt_percentage = hvts / carries,
        hvt_per_game = hvts / games
      ) %>% 
      add_team_dakota(pbp = pbp, weekly = weekly) %>%
      dplyr::select(
        team:pacr,
        dakota,
        dplyr::everything()
      )
  }
  
  return(team_df)
}


# Team Stats Update DB --------------------------------------------------

update_team_stats_weekly_db <- function(con = fx.db_con(x.host = 'localhost'), pbp = pbp_df) {
  
  table <- 'nflfastR_team_stats_weekly'
  
  existing_ids <- tbl(con, table) |>
    filter(season == nflreadr::get_current_season()) |> 
    select(game_id) |>
    collect() |>
    pull(game_id) |> 
    unique()
  
  filtered_pbp <- pbp |> 
    filter(!game_id %in% existing_ids)
  
  if(nrow(filtered_pbp) > 0){
    team_stats_weekly <- filtered_pbp |> 
      calculate_team_stats_mod(weekly = TRUE)
    
    filtered_delete_game_ids <- filtered_pbp |> 
      pull(game_id) |> 
      unique()
    
    delete_game_ids <- team_stats_weekly |> pull(game_id) |> unique()
    
    dbExecute(con, 
              glue('DELETE FROM "{table}" WHERE game_id IN ({paste0(as.character(paste0("\'", filtered_delete_game_ids, "\'")), collapse = ", ")});')
    )
    
    dbWriteTable(con, table, team_stats_weekly, append = TRUE)
  } else {
    print('Player Stats is already up-to-date!')
  }
  on.exit(dbDisconnect(con))
}

update_team_stats_db <- function(con = fx.db_con(x.host = 'localhost'), pbp = pbp_df) {
  on.exit(dbDisconnect(con))
  
  table <- 'nflfastR_team_stats'
  
  player_stats <- pbp |> 
    calculate_team_stats_mod(weekly = FALSE) |> 
    mutate(season = unique(pbp$season)) |> 
    select(season, everything())
  
  delete_seasons <- pbp |> 
    pull(season) |> 
    unique()
  
  dbExecute(con, 
            glue('DELETE FROM "{table}" WHERE season IN ({paste0(as.character(paste0("\'", delete_seasons, "\'")), collapse = ", ")});')
  )
  
  dbWriteTable(con, 
               table, 
               player_stats, 
               append = TRUE)
}


# XYAC transform ----------------------------------------------------------

# Link: https://github.com/nflverse/nflfastR/blob/master/R/helper_add_xyac.R
add_xyac_mod <- function(pbp, ...) {
  if (nrow(pbp) == 0) {
    # user_message("Nothing to do. Return passed data frame.", "info")
  } else {
    # testing only
    # pbp <- g
    
    source('https://raw.githubusercontent.com/nflverse/nflfastR/master/R/helper_additional_functions.R')
    
    pbp <- pbp |> dplyr::select(-tidyselect::contains('xyac'))
    
    # for joining at the end
    pbp <- pbp |>
      dplyr::mutate(index = 1:dplyr::n())
    
    # prepare_xyac_data helper function shown below
    passes <- prepare_xyac_data(pbp) |>
      dplyr::filter(valid_pass == 1, distance_to_goal != 0)
    
    if (!nrow(passes) == 0) {
      # user_message("Computing xyac...", "todo")
      join_data <- passes |>
        dplyr::select(
          "index", "distance_to_goal", "season", "week", "home", "posteam", "roof",
          "half_seconds_remaining", "down", "ydstogo",
          "posteam_timeouts_remaining", "defteam_timeouts_remaining",
          "original_spot" = "yardline_100", "original_ep" = "ep", "air_epa", "air_yards"
        ) |>
        dplyr::mutate(
          down = as.integer(down),
          ydstogo = as.integer(ydstogo),
          original_ydstogo = ydstogo
        ) |>
        dplyr::select("index":"ydstogo", "original_ydstogo", dplyr::everything())
      
      xyac_vars <-
        stats::predict(
          fastrmodels::xyac_model,
          as.matrix(passes |> xyac_model_select())
        ) |>
        tibble::as_tibble() |>
        dplyr::rename(prob = "value") |>
        dplyr::bind_cols(
          tibble::tibble(
            "yac" = rep_len(-5:70, length.out = nrow(passes) * 76),
            "index" = rep(passes$index, times = rep_len(76, length.out = nrow(passes)))
          ) |>
            dplyr::left_join(join_data, by = "index") |>
            dplyr::mutate(
              half_seconds_remaining = dplyr::if_else(
                half_seconds_remaining <= 6,
                0,
                half_seconds_remaining - 6
              )
            )
        ) |>
        dplyr::group_by(index) |>
        dplyr::mutate(
          max_loss = dplyr::if_else(distance_to_goal < 95, -5, distance_to_goal - 99),
          max_gain = dplyr::if_else(distance_to_goal > 70, 70, distance_to_goal),
          cum_prob = cumsum(prob),
          prob = dplyr::case_when(
            # truncate probs at loss greater than max loss
            yac == max_loss ~ cum_prob,
            # same for gains bigger than possible
            yac == max_gain ~ 1 - dplyr::lag(cum_prob, 1),
            TRUE ~ prob
          ),
          # get end result for each possibility
          yardline_100 = distance_to_goal - yac
        ) |>
        dplyr::filter(yac >= max_loss, yac <= max_gain) |>
        dplyr::select(-cum_prob) |>
        dplyr::mutate(
          posteam_timeouts_pre = posteam_timeouts_remaining,
          defeam_timeouts_pre = defteam_timeouts_remaining,
          gain = original_spot - yardline_100,
          turnover = dplyr::if_else(down == 4 & gain < ydstogo, as.integer(1), as.integer(0)),
          down = dplyr::if_else(gain >= ydstogo, 1, down + 1),
          ydstogo = dplyr::if_else(gain >= ydstogo, 10, ydstogo - gain),
          # possession change if 4th down failed
          down = dplyr::if_else(turnover == 1, as.integer(1), as.integer(down)),
          ydstogo = dplyr::if_else(turnover == 1, as.integer(10), as.integer(ydstogo)),
          # flip yardline_100 and timeouts for turnovers
          yardline_100 = dplyr::if_else(turnover == 1, as.integer(100 - yardline_100), as.integer(yardline_100)),
          posteam_timeouts_remaining = dplyr::if_else(
            turnover == 1,
            defeam_timeouts_pre,
            posteam_timeouts_pre
          ),
          defteam_timeouts_remaining = dplyr::if_else(
            turnover == 1,
            posteam_timeouts_pre,
            defeam_timeouts_pre
          ),
          # ydstogo can't be bigger than yardline
          ydstogo = dplyr::if_else(ydstogo >= yardline_100, as.integer(yardline_100), as.integer(ydstogo))
        ) |>
        dplyr::ungroup()
      
      pbp <- pbp |>
        dplyr::left_join(xyac_vars, by = "index") |>
        dplyr::select(-index)
      
      # message_completed("added xyac variables", ...)
    } else { # means no valid pass plays in the pbp
      pbp <- pbp |>
        dplyr::mutate(
          xyac_epa = NA_real_,
          xyac_mean_yardage = NA_real_,
          xyac_median_yardage = NA_real_,
          xyac_success = NA_real_,
          xyac_fd = NA_real_
        ) |>
        dplyr::select(-index)
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
      dplyr::filter(pass == 1 | rush == 1) %>%
      dplyr::filter(!is.na(posteam) & !is.na(qb_epa) & !is.na(id) & !is.na(down)) %>%
      dplyr::mutate(epa = dplyr::if_else(qb_epa < -4.5, -4.5, qb_epa)) %>%
      decode_player_ids() %>% 
      rename(team = posteam)
  })
  
  if (isTRUE(weekly)) {
    relevant_players <- add_to_this %>% 
      dplyr::rename(team = team) %>% 
      dplyr::filter(attempts >= 5) %>%
      dplyr::mutate(filter_id = paste(team, season, week, sep = "_")) %>%
      dplyr::pull(filter_id)
    
    model_data <- df %>% 
      dplyr::group_by(team, week, season) %>%
      dplyr::summarize(
        n_plays = n(),
        epa_per_play = sum(epa) / n_plays,
        cpoe = mean(cpoe, na.rm = TRUE)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(cpoe = dplyr::if_else(is.na(cpoe), 0, cpoe)) %>%
      dplyr::mutate(filter_id = paste(team, season, week, sep = "_")) %>%
      dplyr::filter(filter_id %in% relevant_players)
    
    model_data$dakota <- mgcv::predict.gam(dakota_model, model_data) %>% as.vector()
    
    out <- add_to_this %>% 
      dplyr::left_join(
        model_data %>%
          dplyr::select(team, week, season, dakota),
        by = c("team", "week", "season")
      )
  } else if (isFALSE(weekly)) {
    relevant_players <- add_to_this %>%
      dplyr::filter(attempts >= 5) %>%
      dplyr::pull(team)
    
    model_data <- df %>%
      dplyr::group_by(team) %>%
      dplyr::summarize(
        n_plays = n(),
        epa_per_play = sum(epa) / n_plays,
        cpoe = mean(cpoe, na.rm = TRUE)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(cpoe = dplyr::if_else(is.na(cpoe), 0, cpoe)) %>%
      dplyr::filter(team %in% relevant_players)
    
    model_data$dakota <- mgcv::predict.gam(dakota_model, model_data) %>% as.vector()
    
    out <- add_to_this %>%
      dplyr::left_join(
        model_data %>%
          dplyr::select(team, dakota),
        by = "team"
      )
  }
  return(out)
}