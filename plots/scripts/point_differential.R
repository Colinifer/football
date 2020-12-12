all_pbp_df <-
  do.call(rbind, lapply(dir(glue('data/pbp'), 
                     full = T) %>% 
                       .[which(grepl('.rds', .) &
                                 !grepl('xyac', .))], 
                     readRDS)
          )

full_pbp_df %>% 
  mutate(home_result = home_score - away_score,
         away_result = away_score - home_score) %>% 
  group_by(season, game_id, week, home_team, home_result) %>% 
  summarize(result = last(home_result)) %>% 
  select(-home_result) %>% 
  rename(team = home_team) %>% 
  rbind(full_pbp_df %>% 
          mutate(home_result = home_score - away_score,
                 away_result = away_score - home_score) %>% 
          group_by(season, game_id, week, away_team, away_result) %>% 
          summarize(result = last(away_result)) %>% 
          select(-away_result) %>% 
          rename(team = away_team)) %>%
  arrange(team, week) %>% 
  group_by(team) %>% 
  # mutate(result = lag(result) + result)
  mutate(season_point_diff = cumsum(result)) %>% 
  ggplot(x = week, y = season_point_diff) + 
  geom_line(aes(x = week, y = season_point_diff))
