
pace_df <- pbp_df |> 
  arrange(game_id, posteam, play_id) |> 
  filter(season == current_season) |> 
  group_by(posteam) |> 
  filter(play == 1) |> 
  mutate(possession_time = lag(game_seconds_remaining)-game_seconds_remaining) |> 
  summarise(
    plays = n(),
    possession_time = sum(possession_time, na.rm = T)
  ) |> 
  mutate(
    pace = plays / (possession_time/60)
  ) |> 
  arrange(
    desc(pace)
  )

pace_df |> 
  arrange(pace) |> 
  mutate(posteam=factor(posteam, levels=posteam)) |> 
  ggplot2::ggplot(aes(y = posteam, x = pace)) +
  ggplot2::geom_col(aes(color = posteam, fill = posteam), width = 0.5) +
  nflplotR::scale_color_nfl(type = "secondary") +
  nflplotR::scale_fill_nfl(alpha = 0.8) +
  ggplot2::labs(
    title = tools::toTitleCase(glue("{current_season} NFL Plays per Minute")),
    x = "Offensive Pace (Plays per Minute)"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"),
    plot.title.position = "plot",
    # it's obvious what the x-axis is so we remove the title
    axis.title.y = ggplot2::element_blank(),
    # this line triggers the replacement of team abbreviations with logos
    axis.text.y = nflplotR::element_nfl_wordmark(size=3),
    plot.tag.position = c(1, 1)
  )
