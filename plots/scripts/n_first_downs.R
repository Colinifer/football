year <- current_season

pbp_df %>% 
  group_by(passer_player_id) %>% 
  filter(qb_dropback == 1 * !is.na(passer_player_id)) %>% 
  summarise(passer_player_name = first(passer_player_name),
            n_dropbacks = sum(qb_dropback, na.rm = TRUE), 
            n_first_downs = sum(first_down, na.rm = TRUE),
            n_success = sum(success, na.rm = TRUE)) %>% 
  mutate(success_rate = n_success / n_dropbacks,
         first_down_rate = n_first_downs / n_dropbacks) %>% 
  arrange(-first_down_rate) %>% 
  filter(n_dropbacks > 50)
