
ff_sim_df <- ff_simulate(conn = ff_conn_beep_boop, n_seasons = 1000, n_weeks = 14)

plot(ff_sim_df)

plot(ff_sim_df, type = "rank")
plot(ff_sim_df, type = "points")

ff_sim_df$summary_simulation
ff_sim_df$summary_season
ff_sim_df$summary_week
ff_sim_df$roster_scores
ff_sim_df$league_info

player_rankings <- ffs_latest_rankings(type = "week")

player_rankings |> 
  as_tibble() |> 
  arrange(ecr) |> 
  filter(pos %in% c('QB', 'WR', 'RB', 'TE'))


ff_sim_week <- ff_simulate_week(conn = ff_conn_beep_boop, n = 10000)

ff_sim_week

plot(ff_sim_week,type = "luck")
plot(ff_sim_week,type = "points")


