pbp_df |>
  arrange(game_date, game_id) |>
  filter(play_type %in% c("pass")) |> 
  select(game_id,
         game_date,
         week,
         play_id,
         play_type,
         # desc,
         posteam,
         defteam,
         passer_player_id,
         passer_player_name,
         air_yards,
         epa) |>
  inner_join(
    participation_df |>
      select(
        game_id = nflverse_game_id,
        play_id,
        # offense_formation,
        # offense_personnel,
        # defenders_in_box,
        # defense_personnel,
        number_of_pass_rushers,
        ngs_air_yards,
        time_to_throw,
        was_pressure,
        route,
        defense_man_zone_type,
        defense_coverage_type
      )
  ) |> 
  group_by(game_id,
           game_date,
           posteam,
           defteam,
           passer_player_id
           ) |> 
  summarise(
    passer_player_name = first(passer_player_name, na_rm = TRUE),
    ngs_air_yards = mean(ngs_air_yards, na.rm = TRUE),
    time_to_throw = mean(time_to_throw, na.rm = TRUE),
    air_yards = mean(air_yards, na.rm = TRUE),
    epa = mean(epa, na.rm = TRUE)
  ) |> 
  ungroup() |> 
  mutate(
    air_yards_ptile = ntile(ngs_air_yards, 100),
    time_to_throw_ptile = ntile(time_to_throw, 100),
    NULL
  ) |> 
  filter(!is.na(passer_player_id)
         & posteam == "PHI" 
         & passer_player_name == "J.Hurts") |> 
  ggplot(aes(x = time_to_throw, y = ngs_air_yards)) + 
  nflplotR::geom_nfl_logos(aes(team_abbr = defteam), width = 0.05, alpha = 0.9) + 
  scale_x_reverse()
