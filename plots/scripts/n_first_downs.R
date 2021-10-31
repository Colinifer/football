year <- current_season

my_week <- fx.n_week(pbp_df)

# Eckel Ratio
pbp_df %>% 
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
  mutate(
    eckel_rate = total_eckel / n_drive
  )

pbp_df %>% 
  filter(!is.na(passer_player_id)) %>% 
  group_by(passer_player_id) %>% 
  summarise(
    passer_player_name = first(passer_player_name),
    games_played = n_distinct(game_id),
    total_air_yards = sum(air_yards, na.rm = TRUE),
    total_pass_attempts = sum(pass_attempt, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  mutate(
    adot = total_air_yards / total_pass_attempts
  ) %>% 
  filter(total_pass_attempts >= (max(games_played)*8)) %>% 
  arrange(-adot)

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
  select(rk, passer_player_name, posteam, first_down_rate, success_rate) %>% 
  gt() %>% 
  tab_header(title = 'Passer 1st Down Rate', 
             subtitle = glue('Through week {my_week} | Min. {my_week*8} dropbacks')) %>% 
  cols_label(
    rk = 'Rank',
    passer_player_name = 'Player',
    posteam = 'Team',
    first_down_rate = '1D/DB',
    success_rate = 'Success Rate'
  ) %>% 
  fmt_percent(columns = c(first_down_rate, success_rate), decimals = 1)
