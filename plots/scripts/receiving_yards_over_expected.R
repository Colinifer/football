exp_air_yards_df <- pbp_df %>%
  add_xyac_mod() %>%
  filter(
    season.x == current_season &
      pass_attempt == 1 &
      season_type == 'REG' &
      two_point_attempt == 0 &
      !is.na(receiver_id) &
      !is.na(air_yards.x) &
      !is.na(complete_pass) &
      # wp > .2 &
      # wp < .8 &
      air_yards.x > 0
  ) %>%
  select(
    season = season.x,
    week = week.x,
    game_id,
    play_id,
    posteam = posteam.x,
    receiver,
    receiver_player_id,
    yardline_100 = yardline_100.x,
    air_yards = air_yards.x,
    actual_yards_gained = yards_gained,
    complete_pass,
    cp,
    cpoe,
    yac_prob = prob,
    gain,
    pass_attempt
  ) %>%
  mutate(
    gain = ifelse(yardline_100 == air_yards, yardline_100, gain),
    air_yards = air_yards,
    comp_air_yards = ifelse(complete_pass == 1, air_yards, 0),
    yac_prob = ifelse(yardline_100 == air_yards, 1, yac_prob),
    # PPR_points = 1 + gain / 10 + ifelse(gain == yardline_100, 6, 0),
    # half_PPR_points = .5 + gain / 10 + ifelse(gain == yardline_100, 6, 0),
    catch_run_prob = cp * yac_prob,
    # exp_PPR_points = PPR_points * catch_run_prob,
    # exp_half_PPR_points = half_PPR_points * catch_run_prob,
    exp_yards = gain * catch_run_prob,
    exp_air_yards = cp * air_yards,
    actual_outcome = ifelse(actual_yards_gained == gain &
                              complete_pass == 1, 1, 0),
    # actual_PPR_points = ifelse(actual_outcome == 1, PPR_points, 0),
    # actual_half_PPR_points = ifelse(actual_outcome == 1, half_PPR_points, 0),
    # completion = 0,
    attempt = 0,
    game_played = 0,
    cayoe = cpoe * air_yards,
    sum_cayoe = 0
  ) %>%  
  group_by(game_id, receiver_player_id) %>%
  mutate(game_played = ifelse(row_number() == 1, 1, 0)) %>%
  ungroup() %>%
  # group_by(game_id, play_id, receiver_player_id) %>%
  # mutate(completion = ifelse(row_number() == 1, 1, 0)) %>%
  # ungroup %>%
  group_by(game_id, play_id, receiver_player_id) %>%
  mutate(attempt = ifelse(row_number() == 1, 1, 0)) %>%
  ungroup() %>%
  group_by(posteam, receiver_player_id) %>%
  # filter()
  summarize(
    receiver = first(receiver),
    season = unique(season),
    games = sum(game_played, na.rm = T),
    targets = sum(attempt, na.rm = T),
    completions = sum(actual_outcome, na.rm = T),
    air_yards = sum(ifelse(actual_outcome == 1, air_yards, 0), na.rm = T),
    comp_air_yards = sum(ifelse(actual_outcome == 1, comp_air_yards, 0), na.rm = T),
    yards = sum(ifelse(actual_outcome == 1, gain, 0), na.rm = T),
    td = sum(ifelse(gain == yardline_100, actual_outcome, 0), na.rm = T),
    # PPR_pts = sum(actual_PPR_points, na.rm = T),
    # half_PPR_pts = sum(actual_half_PPR_points, na.rm = T),
    exp_completions = sum(ifelse(attempt == 1, cp, NA), na.rm = T),
    exp_air_yards = sum(ifelse(attempt == 1, exp_air_yards, NA), na.rm = T),
    exp_yards = sum(exp_yards, na.rm = T),
    exp_td = sum(ifelse(gain == yardline_100, catch_run_prob, 0), na.rm = T),
    # exp_PPR_pts = sum(exp_PPR_points, na.rm = T),
    # exp_half_PPR_pts = sum(exp_half_PPR_points, na.rm = T),
    sum_cayoe = sum(comp_air_yards - exp_air_yards, na.rm = T)
  ) %>%
  mutate(# half_ppr_pts_diff = half_PPR_pts - exp_half_PPR_pts,
    # ppr_pts_diff = PPR_pts - exp_PPR_pts,
    cayoe_a = sum_cayoe / targets) %>%
  ungroup() %>% 
  arrange(-exp_air_yards) %>% 
  left_join(roster_df %>% 
              filter(season == current_season) %>% 
              select(receiver_player_id = gsis_id, espn_id, position, headshot_url)) %>% 
  left_join(fx.ff_free_agents(league_name = 'Beep Boop') %>% 
              select(receiver_player_id = player_id, on_roster)) %>% 
  filter(position == c('WR')) %>% 
  mutate(rk = row_number()) %>% 
  select(rk, receiver, exp_air_yards, air_yards, on_roster) %>% 
  filter(rk <= 30)

exp_air_yards_df %>% 
  ggplot(aes(x = rk, y = exp_air_yards)) + 
  geom_segment(aes(xend = rk, yend = air_yards), color = color_cw[5]) + 
  geom_text(aes(y = exp_air_yards, label = receiver), color = color_cw[5]) + 
  scale_x_reverse() + 
  coord_flip() + 
  theme_cw_dark

  
  
  
  