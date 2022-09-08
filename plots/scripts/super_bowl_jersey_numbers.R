tbl(con, 'nflfastR_pbp') |> 
  filter((season < 2021 & week == 21) | 
           (season > 2021 & week == 22)) |> 
  collect() |> 
  calculate_player_stats(weekly = T) |> 
  inner_join(
    tbl(con, 'nflfastR_rosters') |> 
      select(season, position, player_id = gsis_id, jersey_number) |> 
      collect(),
    by = c('season', 'player_id')
  ) |> 
  filter(position == 'QB') |> 
  select(season, player_name, recent_team, position, jersey_number) |> 
  arrange(-season)
