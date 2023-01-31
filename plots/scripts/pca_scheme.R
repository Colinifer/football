
part_df <- nflreadr::load_participation(include_pbp = T)

prcomp_df <- part_df |> 
  filter(!is.na(play_type) & 
           play_type %in% c('run', 'pass')) |> 
  select(posteam, qtr, game_seconds_remaining, down, ydstogo, yardline_100, play_type, offense_formation, offense_personnel,
         run_location, run_gap, pass_location, air_yards, qb_dropback, no_huddle) |> 
  select(-play_type, -offense_formation, -offense_personnel, -pass_location, -run_location, -run_gap)



prcomp(prcomp_df
       ,scale = TRUE)

part_df$
