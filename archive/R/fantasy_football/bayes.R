source("assets/density_functions.R")
source("assets/ffpros.R")

# Rankings ----------------------------------------------------------------

fp_ranks <- ffpros::fp_rankings(
  page = "half-point-ppr-flex", 
  include_metadata = TRUE
)

fp_ros_ranks <- ffpros::fp_rankings(
  page = "ros-half-point-ppr-flex",
  include_metadata = TRUE
)

ffpros::fp_projections()

# fp_ranks$ecr |>
#   glimpse()
# 
# fp_ros_ranks$ecr |> 
#   glimpse()

ff_player_ids <- nflreadr::load_ff_playerids() |> 
  select(
    gsis_id,
    fantasypros_id,
    espn_id,
    position
  )


# weekly rankings
clean_fp_ranks_df <- clean_fp_ranks(fp_ranks = fp_ranks)
fseason = clean_fp_ranks_df$season |> min()
fweek = clean_fp_ranks_df$week |> min()
clean_fp_ranks_df |> 
  write_csv(glue("data/ffpros/ecr/{fseason}_ecr_{fdate}.csv"))

# update dbs
clean_fp_ranks(fp_ranks) |> 
  update_fp_ranks_db()

clean_fp_ranks(fp_ranks) |> 
  update_fp_ranks_db(con = initR::fx.db_con(x.port = '4433'))
  
# plots
clean_fp_ranks(fp_ranks = fp_ranks, "Beep Boop") |> 
  create_fp_dist_df() |> 
  create_fp_dist_rankings()

# plots
clean_fp_ranks(fp_ranks = fp_ranks, "Kepler") |> 
  create_fp_dist_df() |> 
  create_fp_dist_rankings()


# rest of season
clean_fp_ros_ranks_df <- clean_fp_ranks(fp_ranks = fp_ros_ranks)
fseason = clean_fp_ros_ranks_df$season |> min()
fdate = clean_fp_ros_ranks_df$date |> min()
clean_fp_ros_ranks_df |> 
  write_csv(glue("data/ffpros/ecr/{fseason}_ros_ecr_{fdate}.csv"))

# update dbs
clean_fp_ranks(fp_ros_ranks) |> 
  update_fp_ranks_db()

clean_fp_ranks(fp_ros_ranks) |> 
  update_fp_ranks_db(con = initR::fx.db_con(x.port = '4433'))

# plots
clean_fp_ranks(fp_ranks = fp_ros_ranks, fantasy_league = "Beep Boop") |> 
  create_fp_dist_df() |> 
  create_fp_dist_rankings()

clean_fp_ranks(fp_ranks = fp_ros_ranks, fantasy_league = "Kepler") |> 
  create_fp_dist_df() |> 
  create_fp_dist_rankings()


# Projections -------------------------------------------------------------

projections_flex <- fp_projections(
  "flex", 
  year = 2024, 
  week = 3, 
  scoring = "HALF", 
  `min-yes`="true", 
  `max-yes`="true"
)

projections_qb <- fp_projections(
  "qb", 
  year = 2024, 
  week = 2, 
  scoring = "HALF", 
  `min-yes`="true", 
  `max-yes`="true"
)

fp_projections(
  "qb", 
  year = 2024, 
  week = 1, 
  scoring = "HALF", 
  `min-yes`="true", 
  `max-yes`="true"
)
  


`# Vegas Spread Distribution -----------------------------------------------

pbp_df |> 
  filter(season >= 2024) |> 
  arrange(game_date) |> 
  mutate(
    diff = result,
    exp_diff = spread_line*-1,
    delta = diff - exp_diff
  ) |> 
  select(game_date, game_id, diff, exp_diff, delta) |> 
  unique() |> 
  ggplot(aes(delta)) + 
  geom_density()



# Distributions -----------------------------------------------------------

x1 = seq(0, 40, 1/1000); y1 = dgamma(x1, shape = 2, rate = 1/3)
x2 = seq(0, 40, 1/1000); y2 = dgamma(x2, shape = 2, rate = 1/2)
x3 = seq(0, 40, 1/1000); y3 = dgamma(x3, shape = 7, rate = 1/3)
plot(x1, y1, type = "l",
     xlim = c(0, 25), ylim = range(c(y1, y2, y3)),
     main = "Probability Density Functions of Gamma Distributions",
     xlab = "x", ylab = "Density",
     lwd = 2, col = "blue")
points(x2, y2, type = "l", lwd = 2, col = "red")
points(x3, y3, type = "l", lwd = 2, col = "green")
# Add legend
legend("topright", c("shape = 2, rate = 1/3",
                     "shape = 2, rate = 1/2",
                     "shape = 4, rate = 1/2"),
       lwd = c(2, 2, 2),
       col = c("blue", "red", "green"),
       bty = "n")



# Data --------------------------------------------------------------------

con <- fx.db_con(x.host = 'localhost')

pbp_df <- tbl(con, 'nflfastR_pbp') |> 
  filter(season >= 2021) |> 
  collect() # |> 
# fix_rookies()

dbDisconnect(con)

player_stats_mod <- pbp_df |> 
  calculate_player_stats_mod(weekly = TRUE)

player_stats <- pbp_df |> 
  calculate_player_stats(weekly = TRUE)

player_stats_mod |> 
  filter(player_id == "00-0035676") |> 
  select(season, recent_team, player_id, player_name, fantasy_points_ppr) |> 
  group_by(recent_team) |> 
  ggplot(aes(fantasy_points_ppr, color = recent_team)) + 
  geom_density()

player_stats |> 
  filter(player_id == "00-0035676") |> 
  select(season, recent_team, player_id, player_name, fantasy_points_ppr) |> 
  group_by(recent_team) |> 
  density()

filtered_ids <- nflreadr::load_depth_charts() |> 
  select(-week) |> 
  filter(
    club_code == "PHI"
    & formation == "Offense"
    & position %in% c("RB", "TE", "WR")
    & depth_team == 1
  ) |> 
  unique() |> 
  pull(gsis_id)

player_stats_mod |>
  group_by(player_id) |> 
  filter(
    season >= 2023 
    # & position %in% c("RB", "TE", "WR")
    & player_id %in% filtered_ids
  ) |> 
  select(season,
         recent_team,
         player_id,
         player_name,
         fantasy_points_half_ppr) |>
  mutate(mean_fp = mean(fantasy_points_half_ppr, na.rm = TRUE)) |> 
  arrange(desc(mean_fp)) |> 
  ggplot(aes(fantasy_points_half_ppr, color = player_name)) +
  geom_density()


player_stats |>
  filter(
    season >= 2023 &
      position == "QB"
  ) |> 
  select(season,
         recent_team,
         player_id,
         player_name,
         fantasy_points_ppr) |>
  ggplot(aes(fantasy_points_ppr, color = player_name)) +
  geom_density()
