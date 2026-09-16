{
  library(GGally)
}

receiver_stats_model <- player_stats |> 
  left_join(roster_df |> 
              filter(season == year) |> 
              select(gsis_id, full_name, position), 
            by = c('player_id' = 'gsis_id')) |> 
  filter(position == 'WR' & 
           target_share > 0 & 
           receiving_air_yards > 0 & 
           fantasy_points_half_ppr > 0) |> 
  select(full_name, position, target_share, receiving_air_yards, wopr, fantasy_points_half_ppr) %>% 
  # arrange(-receiving_air_yards) |> 
  replace(is.na(.), 0) |> 
  arrange(-fantasy_points_half_ppr) |> 
  mutate(
    log_target_share = log(target_share),
    log_receiving_air_yards = log(receiving_air_yards),
    log_fantasy_points_half_ppr = log(fantasy_points_half_ppr)
  )

pairs(receiver_stats_model |> select(contains('log_')), pch = 18, col = 'steelblue')

qqnorm(receiver_stats_model$log_fantasy_points_half_ppr)
qqnorm(receiver_stats_model$log_target_share)
qqnorm(receiver_stats_model$log_receiving_air_yards)


ggpairs(receiver_stats_model |> 
          select(log_fantasy_points_half_ppr,
                 log_target_share,
                 log_receiving_air_yards))

ggpairs(receiver_stats_model |> 
          select(fantasy_points_half_ppr,
                 target_share,
                 receiving_air_yards))

hist(receiver_stats_model$log_target_share)
hist(receiver_stats_model$log_receiving_air_yards)
hist(receiver_stats_model$log_fantasy_points_half_ppr)


receiver_model <- lm(fantasy_points_half_ppr ~ target_share + receiving_air_yards, 
   data = receiver_stats_model)

lm(fantasy_points_half_ppr ~ target_share + receiving_air_yards, 
   data = receiver_stats_model) |> 
  summary()

lm(log_fantasy_points_half_ppr ~ log_target_share + log_receiving_air_yards, 
   data = receiver_stats_model) |> 
  summary()

receiver_stats_model |> 
  add_predictions(receiver_model) |> 
  mutate(diff = fantasy_points_half_ppr - pred) |> 
  arrange(-diff)

hist(residuals(receiver_model), col = "steelblue")

plot(fitted(receiver_model), residuals(receiver_model))
abline(h = 0, lty = 2)

summary(receiver_model)


# receiver_stats_model |> write_csv('~/Desktop/receiver_stats.csv')

receiver_stats_model |> 
  mutate(
    exp_fantasy_points = predict(receiver_model, cur_data()),
    fantasy_point_delta = fantasy_points_half_ppr - exp_fantasy_points) |> 
  arrange(fantasy_point_delta)

