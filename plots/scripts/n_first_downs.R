year <- current_season

my_week <- fx.n_week(pbp_df)

pbp_df %>% 
  group_by(passer_player_id) %>% 
  filter(qb_dropback == 1 & !is.na(qb_dropback) & !is.na(passer_player_id) & penalty == 0 & two_point_attempt == 0) %>% 
  summarise(passer_player_name = first(passer_player_name),
            posteam = first(posteam),
            n_dropbacks = sum(qb_dropback, na.rm = TRUE), 
            n_first_downs = sum(first_down, na.rm = TRUE),
            n_success = sum(success, na.rm = TRUE)) %>% 
  mutate(success_rate = n_success / n_dropbacks,
         first_down_rate = n_first_downs / n_dropbacks) %>% 
  arrange(-first_down_rate) %>% 
  filter(n_dropbacks > 50) %>% 
  mutate(rk = row_number()) %>% 
  select(rk, passer_player_name, posteam, first_down_rate, success_rate) %>% 
  gt() %>% 
  tab_header(title = 'Passer 1st Down Rate', 
             subtitle = glue('Through week {my_week} | Min. 50 dropbacks')) %>% 
  cols_label(
    rk = 'Rank',
    passer_player_name = 'Player',
    posteam = 'Team',
    first_down_rate = '1D/DB',
    success_rate = 'Success Rate'
  ) %>% 
  fmt_percent(columns = c(first_down_rate, success_rate), decimals = 1)
