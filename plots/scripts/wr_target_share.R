# Data --------------------------------------------------------------------

current_season <- year

con <- fx.db_con()
pbp_df <- tbl(con, 'nflfastR_pbp') %>% 
  filter(season == current_season) %>% 
  collect()

stats <- pbp_df %>% 
  calculate_player_stats_mod() %>% 
  unique()

stats %>%
  ggplot(aes(x = target_share, y = air_yards_share)) +
  geom_point() +
  geom_text(label = stats$player_name) + 
  theme_cw_dark
