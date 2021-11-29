year <- current_season

my_week <- fx.n_week(pbp_df)

# Eckel Rate/Ratio --------------------------------------------------------

team_points <- pbp_df %>% 
  mutate(
    posteam_result = case_when(
      posteam == home_team ~ home_score,
      posteam == away_team ~ away_score
    )
  ) %>% 
  group_by(game_id, posteam) %>% 
  summarise(
    posteam_result = custom_mode(posteam_result)
  ) %>% 
  group_by(posteam) %>% 
  summarise(
    total_points_for = sum(posteam_result, na.rm = T)
  )

pbp_df %>% 
  filter(!is.na(posteam)) %>% 
  mutate(
    eckel_success = ifelse((down == 1 & yardline_100 <= 40) | (yards_gained > 40 & touchdown == 1), 1, 0)
  ) %>% 
  group_by(
    posteam, game_id, drive
  ) %>% 
  summarise(
    n_drive = n_distinct(game_id, drive),
    total_eckel = sum(eckel_success, na.rm = T)
  ) %>% 
  mutate(
    total_eckel = ifelse(total_eckel > 0, 1, 0)
  ) %>% 
  ungroup() %>% 
  group_by(posteam) %>% 
  summarise(
    n_drive = sum(n_drive, na.rm = T),
    total_eckel = sum(total_eckel, na.rm =T)
  ) %>% 
  left_join( # join team points to 
    team_points,
    by = c('posteam')
  ) %>% 
  mutate(
    eckel_rate = total_eckel / n_drive,
    eckel_ratio = round(total_points_for / total_eckel, 2)
  ) %>% 
  left_join(
    teams_colors_logos %>% 
      select(team_abbr, team_logo_espn),
    by = c('posteam' = 'team_abbr')
  ) %>% 
  select(
    team_logo_espn,
    posteam, 
    everything()
  ) %>% 
  arrange(-eckel_rate) %>% 
  gt() %>% 
  tab_header(title = 'Team Quality Possesions', 
             subtitle = glue('Through week {my_week}')) %>% 
  cols_label(
    team_logo_espn = 'Team',
    posteam = '',
    n_drive = 'Total Drives',
    total_eckel = '# Eckel Drives',
    total_points_for = 'Points For',
    eckel_rate = 'Eckel Rate',
    eckel_ratio = 'Pts/Eckel'
  ) %>% 
  fmt_percent(columns = c(eckel_rate), decimals = 1) %>% 
  gt_theme_cw() %>% 
  gtsave(filename = glue("team_stats/eckel_rate_{current_season}.png"), path = "plots/desktop")


pbp_df %>% 
  filter(!is.na(passer_player_id)) %>% 
  group_by(passer_player_id) %>% 
  summarise(
    passer_player_name = custom_mode(passer_player_name),
    posteam = custom_mode(posteam),
    games_played = n_distinct(game_id),
    total_air_yards = sum(air_yards, na.rm = TRUE),
    mean_air_yards = mean(air_yards, na.rm = TRUE),
    total_pass_attempts = sum(pass_attempt, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  mutate(
    adot = total_air_yards / total_pass_attempts
  ) %>% 
  filter(total_pass_attempts >= (max(games_played)*8)) %>% 
  arrange(-mean_air_yards) %>% 
  left_join(
    teams_colors_logos %>% 
      select(team_abbr, team_logo_espn),
    by = c('posteam' = 'team_abbr')
  ) %>% 
  select(
    passer_player_name,
    team_logo_espn,
    posteam,
    games_played,
    total_air_yards,
    mean_air_yards,
    total_pass_attempts,
    adot
  ) %>% 
  gt() %>% 
  tab_header(title = 'Air Yards', 
             subtitle = glue('Through week {my_week}')) %>% 
  cols_label(
    passer_player_name = 'Player',
    team_logo_espn = 'Team',
    posteam = '',
    games_played = 'GP',
    total_air_yards = 'Total Air Yards',
    mean_air_yards = 'Mean Air Yards',
    total_pass_attempts = 'Pass Attempts',
    adot = 'ADoT'
  ) %>% 
  gt_theme_cw() %>% 
  gtsave(filename = glue("qb_passing/air_yards_{current_season}.png"), path = "plots/desktop")

pbp_df %>% 
  group_by(passer_player_id) %>% 
  filter(qb_dropback == 1 & !is.na(qb_dropback) & !is.na(passer_player_id) & penalty == 0 & two_point_attempt == 0) %>% 
  summarise(passer_player_name = first(passer_player_name),
            posteam = first(posteam),
            games_played = n_distinct(game_id),
            n_dropbacks = sum(qb_dropback, na.rm = TRUE), 
            n_first_downs = sum(first_down, na.rm = TRUE),
            n_success = sum(success, na.rm = TRUE)) %>% 
  mutate(success_rate = n_success / n_dropbacks,
         first_down_rate = n_first_downs / n_dropbacks) %>% 
  arrange(-first_down_rate) %>% 
  filter(n_dropbacks > max(games_played)*8) %>% 
  mutate(rk = row_number()) %>% 
  left_join(
    teams_colors_logos %>% 
      select(team_abbr, team_logo_espn),
    by = c('posteam' = 'team_abbr')
  ) %>% 
  select(rk, passer_player_name, team_logo_espn, posteam, first_down_rate, success_rate) %>% 
  gt() %>% 
  tab_header(title = 'Passer 1st Down Rate', 
             subtitle = glue('Through week {my_week} | Min. {my_week*8} dropbacks')) %>% 
  cols_label(
    rk = 'Rank',
    passer_player_name = 'Player',
    team_logo_espn = '',
    posteam = 'Team',
    first_down_rate = '1D/DB',
    success_rate = 'Success Rate'
  ) %>% 
  fmt_percent(columns = c(first_down_rate, success_rate), decimals = 1) %>% 
  gt_theme_cw() %>% 
  gtsave(filename = glue("qb_passing/1db_{current_season}.png"), path = "plots/desktop")
