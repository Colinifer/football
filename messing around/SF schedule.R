game_ids %>%
  select(game_id, week, away_team, away_score, home_team, home_score) %>%
  filter(away_team == "SF" | home_team == "SF")
