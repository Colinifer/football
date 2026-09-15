pace_df <- pbp_df |> 
  arrange(season, game_id, play_id) |> 
  filter(play == 1) |>
  group_by(season, game_id) |> 
  select(season, game_id, posteam, drive_game_clock_start, drive_game_clock_end, time, drive, game_seconds_remaining, play, qb_dropback, rush_attempt) |> 
  mutate(
    drive_game_clock_start = lubridate::period_to_seconds(ms(drive_game_clock_start)),
    drive_game_clock_end = lubridate::period_to_seconds(ms(drive_game_clock_end)),
    time_elapsed = game_seconds_remaining - lead(game_seconds_remaining),
    time_elapsed = ifelse(is.na(time_elapsed), game_seconds_remaining, time_elapsed),
    rushing_play = ifelse(rush_attempt == 1 & qb_dropback == 0, 1, 0)
  ) |> 
  ungroup() |> 
  group_by(season, posteam, game_id) |> 
  summarise(pace = mean(time_elapsed, na.rm = T),
            top = sum(time_elapsed, na.rm = T),
            total_plays = sum(play, na.rm = T),
            dropbacks = sum(qb_dropback, na.rm = T),
            rushes = sum(rushing_play, na.rm = T)
  ) |>  
  arrange(desc(pace))

team_order <- pace_df |>
  arrange(pace) |> 
  pull(posteam)

pace_df |> 
  ggplot(aes(y=factor(posteam, team_order), x=pace)) +
  geom_col(aes(fill = posteam)) +
  nflplotR::scale_fill_nfl(alpha = 0.9) +
  coord_cartesian(xlim = c(25, 35)) + 
  theme_cw_dark +
  labs(x='Offensive Pace',
       y='Team') +
  theme(
    axis.text.y = nflplotR::element_nfl_logo(size = 0.5),
    axis.title = element_blank(),
    legend.position = "top",
    legend.key.size = unit(.25, 'cm'),
    legend.title = element_text(size=4),
    legend.text = element_text(size=4),
    NULL
  )
  
  

pbp_df |> 
  arrange(season, game_id, play_id) |> 
  filter(play_type %in% c("pass", "run")) |> 
  group_by(season, game_id, posteam, drive) |> 
  # select(season, game_id, posteam, drive, game_seconds_remaining, wp) |> 
  mutate(
    time_elapsed = lag(game_seconds_remaining) - game_seconds_remaining,
  ) |> 
  ungroup() |> 
  filter(!is.na(time_elapsed)) |> 
  group_by(posteam) |> 
  filter(posteam == 'NO' & 
           half_seconds_remaining > 120) |> 
  ggplot(aes(x = wp, y = time_elapsed)) + 
  geom_point()


pbp_df |> 
  arrange(season, game_id, play_id) |> 
  filter(play == 1) |>
  group_by(season, game_id) |> 
  select(season, game_id, posteam, qtr, drive_game_clock_start, drive_game_clock_end, drive) |> 
  mutate(
    drive_game_clock_start = lubridate::period_to_seconds(ms(drive_game_clock_start))+((4-qtr)*900),
    drive_game_clock_end = lubridate::period_to_seconds(ms(drive_game_clock_end))+((4-qtr)*900),
    time_of_possession = drive_game_clock_start - drive_game_clock_end
  ) |> 
  unique() |> 
  group_by(season, game_id, posteam) |> 
  summarise(time_of_possession = sum(time_of_possession, na.rm = T))
