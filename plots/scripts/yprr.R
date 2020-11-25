# Data --------------------------------------------------------------------

# pbp_df <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2020.rds'))
if (exists("pbp_df") == FALSE) {
  pbp_df <- readRDS(url(glue('https://github.com/guga31bb/nflfastR-data/blob/master/data/play_by_play_{year}.rds?raw=true'))) %>% decode_player_ids(fast = TRUE)
}

my_week <- pbp_df %>% select(week) %>% max()

yprr <- pbp_df %>% 
  select(-game_id_SR) %>% 
  left_join(sr_part_df %>% 
              left_join(sr_games_df) %>% 
              mutate(play_id = as.integer(play_id)), 
            by = c('game_id' = 'game_id', 'play_id' = 'play_id', 'play_id_SR' = 'play_id_SR'), suffix = c('_fastR', '_SR')) %>% 
  mutate(route_run = ifelse(pass_attempt == 1 & posteam == team & (position == "TE" | position == "WR"), 1, 0)) %>% 
  filter(position == "WR" | position == "TE") %>% 
  group_by(name_SR, reference) %>% 
  summarize(routes_run = sum(route_run, na.rm = T)) %>% 
  filter(routes_run > 0) %>% 
  arrange(-routes_run)

# Receivers ---------------------------------------------------------------

# Average Expected Fantasy Points - Receivers
pbp_xyac <- pbp_df %>% 
  filter(pass_attempt==1 & season_type=='REG' & two_point_attempt==0 & !is.na(receiver_id)) %>% 
  add_xyac_dist

avg_exp_fp_df <- pbp_xyac %>% 
  select(season = season.x, game_id, play_id, posteam = posteam.x, receiver, receiver_player_id, receiver_id, yardline_100 = yardline_100.x, air_yards = air_yards.x, actual_yards_gained = yards_gained, complete_pass, cp, yac_prob = prob, gain) %>% 
  mutate(
    gain = ifelse(yardline_100==air_yards, yardline_100, gain),
    yac_prob = ifelse(yardline_100==air_yards, 1, yac_prob),
    PPR_points = 1 + gain/10 + ifelse(gain == yardline_100, 6, 0),
    half_PPR_points = .5 + gain/10 + ifelse(gain == yardline_100, 6, 0),
    catch_run_prob = cp * yac_prob,
    exp_PPR_points = PPR_points * catch_run_prob,
    exp_half_PPR_points = half_PPR_points * catch_run_prob,
    exp_yards = gain * catch_run_prob,
    actual_outcome = ifelse(actual_yards_gained==gain & complete_pass==1, 1, 0),
    actual_PPR_points = ifelse(actual_outcome==1, PPR_points, 0),
    actual_half_PPR_points = ifelse(actual_outcome==1, half_PPR_points, 0),
    target = 0,
    game_played = 0
  )  %>% 
  group_by(game_id, receiver) %>% 
  mutate(game_played = ifelse(row_number()==1,1,0)) %>% 
  ungroup %>% 
  group_by(game_id, play_id, receiver) %>% 
  mutate(target = ifelse(row_number()==1,1,0)) %>% 
  ungroup %>% 
  group_by(posteam, receiver) %>% 
  summarize(
    receiver_id = unique(receiver_id),
    games = sum(game_played, na.rm = T),
    targets = sum(target, na.rm = T),
    targets_pg = targets / games,
    catches = sum(actual_outcome, na.rm = T),
    catches_pg = catches / games,
    yards = sum(ifelse(actual_outcome==1, gain, 0), na.rm = T),
    yards_pg = yards / games,
    air_yards = sum(ifelse(actual_outcome==1, air_yards, 0), na.rm = T),
    air_yards_pg = air_yards / games,
    td = sum(ifelse(gain==yardline_100, actual_outcome, 0), na.rm = T),
    td_pg = td / games,
    PPR_pts = sum(actual_PPR_points, na.rm = T),
    PPR_pts_pg = PPR_pts / games,
    half_PPR_pts = sum(actual_half_PPR_points, na.rm = T),
    half_PPR_pts_pg = half_PPR_pts / games,
    exp_catches = sum(ifelse(target==1, cp, NA), na.rm = T),
    exp_catches_pg = exp_catches / games,
    exp_yards = sum(exp_yards, na.rm = T),
    exp_yards_pg = exp_yards / games,
    exp_td = sum(ifelse(gain==yardline_100, catch_run_prob, 0), na.rm = T),
    exp_td_pg = exp_td / games,
    exp_td_pg = exp_td / games,
    exp_PPR_pts = sum(exp_PPR_points, na.rm = T),
    exp_PPR_pts_pg = exp_PPR_pts / games,
    exp_half_PPR_pts = sum(exp_half_PPR_points, na.rm = T),
    exp_half_PPR_pts_pg = exp_half_PPR_pts / games,
  ) %>% 
  mutate(
    half_PPR_pts_diff = half_PPR_pts - exp_half_PPR_pts,
    PPR_pts_diff = PPR_pts - exp_PPR_pts
  ) %>% 
  ungroup %>% 
  left_join(
    sleeper_players_df %>%
      select(position, sportradar_id, gsis_id, espn_id, headshot_url),
    by = c("receiver_id" = "gsis_id")
  ) %>%
  left_join(espn_players_df %>%
              select(id, status, onTeamId),
            by = c("espn_id" = "id"))