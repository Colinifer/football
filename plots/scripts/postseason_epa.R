con <- fx.db_con(x.host = 'localhost')
tbl(con, 'nflfastR_pbp') |> 
  filter(season_type == 'POST') |> 
  collect() |> 
  calculate_player_stats_mod(weekly = TRUE) |> 
  filter(!is.na(game_id)) |> 
  group_by(season, player_id, player_name, recent_team) |> 
  mutate(games = 1,
         passing_epa = ifelse(is.na(passing_epa), 0, passing_epa),
         rushing_epa = ifelse(is.na(rushing_epa), 0, rushing_epa),
         receiving_epa = ifelse(is.na(receiving_epa), 0, receiving_epa), 
         total_epa = passing_epa + rushing_epa + receiving_epa) |> 
  summarize(games = sum(games, na.rm = T), 
            total_epa = sum(total_epa, na.rm = T),
            passing_epa = sum(passing_epa, na.rm = T),
            rushing_epa = sum(rushing_epa, na.rm = T),
            receiving_epa = sum(receiving_epa, na.rm = T)) |> 
  ungroup() |> 
  inner_join(
    tbl(con, 'nflfastR_rosters') |> 
      select(season, position, player_id = gsis_id) |> 
      collect(),
    by = c('season', 'player_id')
  ) |> 
  select(season, position, any_of(player_information), -player_id, contains('epa')) |> 
  rename(team = recent_team) |> 
  arrange(-total_epa)


player_stats_weekly |> 
  filter(week > 18) |> 
  group_by(season, player_id, player_name, recent_team) |> 
  mutate(games = 1,
         passing_epa = ifelse(is.na(passing_epa), 0, passing_epa),
         rushing_epa = ifelse(is.na(rushing_epa), 0, rushing_epa),
         receiving_epa = ifelse(is.na(receiving_epa), 0, receiving_epa), 
         total_epa = passing_epa + rushing_epa + receiving_epa) |> 
  summarize(games = sum(games, na.rm = T),
            total_epa = sum(total_epa, na.rm = T),
            passing_epa = sum(passing_epa, na.rm = T),
            rushing_epa = sum(rushing_epa, na.rm = T),
            receiving_epa = sum(receiving_epa, na.rm = T)) |> 
  ungroup() |> 
  inner_join(
    tbl(con, 'nflfastR_rosters') |> 
      select(season, position, player_id = gsis_id) |> 
      collect(),
    by = c('season', 'player_id')
  ) |> 
  select(season, position, any_of(player_information), -player_id, contains('epa')) |> 
  rename(team = recent_team) |> 
  arrange(-total_epa)
