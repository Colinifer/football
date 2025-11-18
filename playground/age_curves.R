# age curves

library(mgcv)
library(dplyr)
library(ggplot2)

player_stats <- calculate_stats(
  seasons = c(2010:2024),
  summary_level = c("season"),
  stat_type = c("player", "team"),
  season_type = c("REG")
)

con <- fx.db_con(x.host = 'localhost')

# pbp_df <- tbl(con, 'nflfastR_pbp') |> 
#   filter(season == 2010) |> 
#   collect() # |> 
# 
# pbp <- tbl(con, 'nflfastR_pbp') |> 
#   filter(season == current_season & 
#            season_type == 'REG' &
#            !is.na(posteam) & 
#            (rush == 1 | pass == 1)) |> 
#   select(season,
#          week,
#          game_id,
#          posteam,
#          defteam,
#          down,
#          play_type,
#          pass,
#          rush,
#          epa,
#          success,
#          home_team,
#          away_team,
#          NULL
#   ) |> 
#   collect()
# # fix_rookies()

roster_df <- tbl(con, 'nflfastR_rosters') |> 
  filter(season >= 2010) |>
  collect()

dbDisconnect(con)


df <- player_stats |> 
  filter(position == 'QB' & attempts >= 40) |> 
  left_join(
    roster_df, 
    by = c("player_id" = "gsis_id", "season" = "season")
  ) |> 
  filter(rookie_year >= 2010) |> 
  mutate(
    passing_epa = replace_na(passing_epa, 0),
    rushing_epa = replace_na(rushing_epa, 0),
    receiving_epa = replace_na(receiving_epa, 0),
    age = as.period(interval(birth_date, paste0(season, "-09-01")))$year,
    years_since_rookie_year = season - rookie_year,
    epa = passing_epa + rushing_epa + receiving_epa,
    epa_per_game = (passing_epa + rushing_epa) / games
  ) |> 
  select(
    season,
    rookie_year,
    player_id,
    player_name,
    age,
    years_since_rookie_year,
    # passing_epa, 
    # rushing_epa,
    # receiving_epa,
    epa,
    games,
    epa_per_game
  ) |> 
  arrange(
    player_id, years_since_rookie_year
  ) |> 
  group_by(player_id) |>  # Group the data by player
  mutate(
    # Index epa_per_game to the value of the player's rookie season
    indexed_epa_per_game = epa_per_game / first(epa_per_game)
  )

# For a single variable
gam_model <- gam(epa_per_game ~ s(years_since_rookie_year), data = df)

# To account for individual player effects (highly recommended)
# You can treat player_id as a random effect
# The 'bs = "re"' specifies a random effects smooth
gam_model_re <- gam(epa_per_game ~ s(years_since_rookie_year) + s(player_id, bs = "re"), data = df)

df |>
  select(player_id, player_name) |> distinct()

prediction_data <- tibble(
  age = seq(min(df$age), max(df$age), by = 1),
  years_since_rookie_year = seq(min(df$age)-21, max(df$age)-21, by = 1),
  player_id = "00-0027939" # Set player_id to NA for prediction on the population curve
)

prediction_data

predictions <- predict(gam_model, newdata = prediction_data, se.fit = TRUE)

# Combine predictions with the new data
prediction_data <- prediction_data %>%
  mutate(
    fit = predictions$fit,
    se = predictions$se.fit
  ) %>%
  # Calculate upper and lower bounds for the 95% confidence interval
  mutate(
    upper_ci = fit + (1.96 * se),
    lower_ci = fit - (1.96 * se)
  )

# Plot the results
ggplot(data = prediction_data, aes(x = age, y = fit)) +
  # Add the confidence interval as a shaded ribbon
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), fill = "skyblue", alpha = 0.5) +
  # Add the smoothed line itself
  geom_line(linewidth = 1.2) +
  labs(
    title = "GAM Smoothed Age Curve for EPA per Game",
    x = "Player Age",
    y = "Predicted EPA per Game"
  ) +
  theme_minimal()

ggplot(data = df,
       aes(x = years_since_rookie_year, y = epa_per_game)) +
  geom_point(alpha = 0.5) +  # Plot the raw data points
  geom_smooth(method = "loess", se = TRUE) + # Add the smooth line and confidence interval
  labs(title = "LOESS Smoothing of EPA per Game by Age",
       x = "Years since Rookie Season",
       y = "EPA per Game")

# Assuming your data frame is named 'df', you would use the following code:
ggplot(
  data = df |> filter(player_name == 'D.Prescott'),
  aes(x = years_since_rookie_year,
      y = epa_per_game,
      group = player_id)
) +
  geom_line() +
  labs(title = "EPA per Game by Player Age",
       x = "Years since Rookie Season",
       y = "EPA per Game") +
  theme(legend.position = "none")

