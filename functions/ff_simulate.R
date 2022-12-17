
ff_sim_df <- ff_simulate(conn = ff_conn_beep_boop, n_seasons = 5000, n_weeks = 14)

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

<<<<<<< Updated upstream
ff_sim_week <- ff_simulate_week(conn = ff_conn_beep_boop, n = 10000)
=======

ff_sim_week <- ff_simulate_week(conn = ff_conn_beep_boop, n = 5000)
>>>>>>> Stashed changes

ff_sim_week

plot(ff_sim_week,type = "luck")
plot(ff_sim_week,type = "points")
<<<<<<< Updated upstream
=======



# Simulations
n_seasons <- 100
n_weeks <- 11

scoring_history <- ffscrapr::ff_scoringhistory(ff_conn_beep_boop, seasons = 2012:2020)

latest_rankings <- ffs_latest_rankings(type = "draft") # also "week", for inseason sims

rosters <- ffs_rosters(ff_conn_beep_boop)

lineup_constraints <- ffs_starter_positions(ff_conn_beep_boop)

league_info <- ffscrapr::ff_league(ff_conn_beep_boop)

adp_outcomes <- ffs_adp_outcomes(
  scoring_history = scoring_history,
  gp_model = "simple", # or "none"
  pos_filter = c("QB","RB","WR","TE","K")
)

projected_scores <- ffs_generate_projections(
  adp_outcomes = adp_outcomes,
  latest_rankings = latest_rankings,
  n_seasons = n_seasons, # number of seasons
  weeks = n_weeks, # specifies which weeks to generate projections for
  rosters = rosters # optional, reduces the sample to just rostered players
)

roster_scores <- ffs_score_rosters(
  projected_scores = projected_scores,
  rosters = rosters
)

tictoc::tic("ffs_optimize_lineups {Sys.time()}")
optimal_scores <- ffs_optimise_lineups(
  roster_scores = roster_scores,
  lineup_constraints = lineup_constraints,
  lineup_efficiency_mean = 0.775,
  lineup_efficiency_sd = 0.05,
  best_ball = FALSE, # or TRUE
  pos_filter = c("QB","RB","WR","TE","K")
)
tictoc::toc()


schedules <- ffs_build_schedules(
  n_seasons = n_seasons,
  n_weeks = n_weeks,
  seed = NULL,
  franchises = ffs_franchises(ff_conn_beep_boop)
)

summary_week <- ffs_summarise_week(optimal_scores, schedules) |> 
  as_tibble()
summary_season <- ffs_summarise_season(summary_week) |> 
  as_tibble()
summary_simulation <- ffs_summarise_simulation(summary_season) |> 
  as_tibble()
>>>>>>> Stashed changes
