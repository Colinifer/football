receiver_stats_model <- player_stats |> 
  left_join(roster_df |> 
              filter(season == year) |> 
              select(gsis_id, full_name, position), 
            by = c('player_id' = 'gsis_id')) |> 
  filter(position == 'WR') |> 
  select(full_name, position, target_share, receiving_air_yards, wopr, fantasy_points_half_ppr) %>% 
  # arrange(-receiving_air_yards) |> 
  replace(is.na(.), 0) |> 
  arrange(-fantasy_points_half_ppr)

receiver_model <- lm(fantasy_points_half_ppr ~ target_share + receiving_air_yards + wopr, 
   data = receiver_stats_model)

hist(residuals(receiver_model), col = "steelblue")

plot(fitted(receiver_model), residuals(receiver_model))
abline(h = 0, lty = 2)

summary(receiver_model)

receiver_model$coefficients['(Intercept)']
receiver_model$coefficients['target_share']
receiver_model$coefficients['receiving_air_yards']
receiver_model$coefficients['wopr']
# 
# receiver_stats_model |> write_csv('~/Desktop/receiver_stats.csv')

receiver_stats_model |> 
  mutate(
    exp_fantasy_points = receiver_model$coefficients['(Intercept)'] + receiver_model$coefficients['target_share']*target_share + receiver_model$coefficients['receiving_air_yards']*receiving_air_yards + receiver_model$coefficients['wopr']*wopr
  ) |> 
  arrange(-exp_fantasy_points)
