pbp_df |> 
  arrange(old_game_id, play_id) |> 
  filter(qb_dropback == 1 & !is.na(passer_player_id)) |> 
  # select(passer_player_id, down, qb_dropback, first_down) |> 
  group_by(passer_player_id, passer_player_name) |> 
  summarise(dropbacks = sum(qb_dropback, na.rm = TRUE), 
            first_downs = sum(first_down, na.rm = TRUE),
            db_1d = sum(first_down, na.rm = TRUE) / sum(qb_dropback, na.rm = TRUE)) |> 
  arrange(desc(db_1d)) |> 
  filter(dropbacks > 50)
