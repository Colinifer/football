library(DescTools)

format_top <- function(seconds) {
  td <- lubridate::seconds_to_period(seconds)
  return(
    sprintf('%02d:%02d', floor(seconds/60), second(td))
  )
}

pbp_df |> 
  filter(grepl("PHI", game_id)) |> 
  arrange(game_id, desc(game_seconds_remaining)) |> 
  select(play_id, game_id, home_team, away_team, posteam, defteam, game_seconds_remaining, play, special_teams_play, punt_attempt, desc) |>
  group_by(game_id) |> 
  mutate(
    posteam = case_when(
      is.na(posteam) ~ lag(posteam),
      TRUE ~ posteam
    ),
    seconds = lag(game_seconds_remaining) - game_seconds_remaining
  ) |> 
  group_by(game_id, posteam) |> 
  summarise(
    time_of_possession = sum(seconds, na.rm = T),
    top = format_top(time_of_possession),
    plays = sum(play, na.rm = T)
  ) |> 
  mutate(
    pace = time_of_possession / plays,
    possession_percentage = time_of_possession/sum(time_of_possession, na.rm = TRUE)
  ) |> 
  filter(!is.na(posteam))

pbp_df |> 
  filter(grepl("MIA", game_id)) |> 
  arrange(game_id, desc(game_seconds_remaining)) |> 
  filter(play_type == "run" & qb_scramble == 0) |> 
  # select(play_id, game_id, home_team, away_team, posteam, rusher_player_id, rusher_player_name, rush_attempt, run_gap, run_location, game_seconds_remaining, play, desc, play_type) |> 
  group_by(season, posteam, rusher_player_id, rusher_player_name, run_gap, run_location, play_type, qb_scramble) |> 
  count(name = "rushes") |> 
  group_by(season, posteam, rusher_player_id, rusher_player_name) |> 
  filter(posteam == "MIA") |> 
  mutate(
    rush_percent = rushes / sum(rushes, na.rm = TRUE),
    total_rushes = sum(rushes, na.rm = TRUE)
  ) |> 
  add_player_data(player_id_col = "rusher_player_id")

pbp_df |> 
  filter(grepl("LA", game_id)) |> 
  filter(posteam == "LA") |> 
  arrange(game_id, desc(game_seconds_remaining)) |> 
  filter(play_type == "run" & qb_scramble == 0) |> 
  # select(play_id, game_id, home_team, away_team, posteam, rusher_player_id, rusher_player_name, rush_attempt, run_gap, run_location, game_seconds_remaining, play, desc, play_type) |> 
  group_by(season, posteam, run_gap, run_location, play_type, qb_scramble) |> 
  count(name = "rushes") |> 
  group_by(season, posteam) |> 
  mutate(
    rush_percent = rushes / sum(rushes, na.rm = TRUE),
    total_rushes = sum(rushes, na.rm = TRUE)
  )
  

pbp_df |> 
  filter(play_type == "run" &
           !is.na(rusher_player_id) & 
           is.na(run_gap) & 
           is.na(run_location)
  ) |> 
  select(season, rusher_player_id, rusher_player_name, run_gap, run_location, posteam, desc)


add_player_data <- function(df, player_id_col) {
  join_cols <- c("season", "gsis_id")
  join_cols <- setNames(join_cols, c("season", player_id_col))
  print(df)
  return(
    df |> 
      left_join(
        roster_df |> 
          select(season, team, gsis_id, full_name, position, depth_chart_position),
        by = join_cols
      )
    )
}
