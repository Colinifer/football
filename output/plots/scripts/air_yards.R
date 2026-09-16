air_yards_df <- pbp_df |> 
  group_by(season, posteam, defteam, week, game_id, receiver_player_id) |> 
  summarize(receiver_player_name = first(receiver_player_name),
            air_yards = sum(air_yards, na.rm = T)) |> 
  ungroup() |> 
  filter(!is.na(receiver_player_id)) |> 
  left_join(
    roster_df |> 
      select(season,
             gsis_id,
             position) |> 
      filter(position %in% c('RB', 'WR', 'TE')),
    by=c("season",
         "receiver_player_id"="gsis_id")
  ) |> 
  arrange(desc(air_yards))

air_yards_df |> 
  filter(position == 'TE'
         & week == max(week)) |> 
  head(10)

air_yards_df |> 
  filter(position == 'WR'
         & posteam == 'DET'
         & week == max(week)) |> 
  head(15)

air_yards_df |> 
  filter(position == 'RB'
         & week == max(week)) |> 
  head(10)


air_yards_df |> 
  filter(receiver_player_name %in% c('A.Pierce','D.Chark','M.Mims')) |> 
  ggplot(aes(x=week, y=air_yards)) + 
  geom_line(aes(color=receiver_player_name)) + 
  nflplotR::geom_nfl_logos(aes(team_abbr = defteam), width = 0.065, alpha = 0.8) +
  theme_cw_light + 
  labs(
    title="Air Yards by Week"
  )

