pbp_df |> 
  filter(!is.na(passer_player_id)) |> 
  group_by(
    season, posteam, passer_player_name, 
    passer_player_id) |> 
  summarise(n = n()) |> 
  arrange(-n) |> 
  head(25)

data <- pbp_df |> 
  filter(
    # week == 16 &
    passer_player_id == '00-0023459' & 
      pass_attempt == 1 & 
      !is.na(pass_location)
  ) |> 
  mutate(
    pass_distance = case_when(
      air_yards < 0 ~ 0,
      air_yards >= 0 & air_yards < 10 ~ 1,
      air_yards >= 10 & air_yards < 20 ~ 2,
      air_yards >= 20 ~ 3
    )
  ) |> 
  group_by(season, passer_player_id, pass_distance, pass_location) |> 
  summarise(
    complete_pass = sum(complete_pass, na.rm = T),
    yards_gained = sum(yards_gained, na.rm = T),
    n = n()
  ) |> 
  mutate(
    completion_pct = round(complete_pass / n, 2),
    pass_distance = pass_distance |> as.character(),
    heatmap_label = glue('{complete_pass}/{n}\nYds: {yards_gained}')
  ) |> 
  left_join(
    roster_df |> 
      select(
        gsis_id,
        full_name
      ),
    by = c('passer_player_id' = 'gsis_id')
  )

contrast_point <- .7

ggplot(
  data = data, 
  aes(x = pass_location, 
      y = pass_distance, 
      fill = completion_pct)
  ) + 
  geom_tile(
    color = color_cw[3],
    lwd = 1.5,
    linetype = 1
  ) + 
  # scale_fill_viridis(option = 'G') +
  scale_fill_gradient(low = color_cw[3], high = color_cw[6], limits=c(0, 1)) + 
  geom_text(
    data = data |> filter(pass_distance == 0 & pass_location == 'left'),
    aes(label = heatmap_label),
    color = ifelse(
      data |> filter(pass_distance == 0 &
                        pass_location == 'left') |> pull(completion_pct) < contrast_point,
      color_cw[5],
      color_cw[3]
    )
  ) + 
  geom_text(
    data = data |> filter(pass_distance == 0 & pass_location == 'middle'),
    aes(label = heatmap_label),
    color = ifelse(
      data |> filter(pass_distance == 0 &
                        pass_location == 'middle') |> pull(completion_pct) < contrast_point,
      color_cw[5],
      color_cw[3]
    )
  ) + 
  geom_text(
    data = data |> filter(pass_distance == 0 & pass_location == 'right'),
    aes(label = heatmap_label),
    color = ifelse(
      data |> filter(pass_distance == 0 &
                        pass_location == 'right') |> pull(completion_pct) < contrast_point,
      color_cw[5],
      color_cw[3]
    )
  ) + 
  geom_text(
    data = data |> filter(pass_distance == 1 & pass_location == 'left'),
    aes(label = heatmap_label),
    color = ifelse(
      data |> filter(pass_distance == 1 &
                        pass_location == 'left') |> pull(completion_pct) < contrast_point,
      color_cw[5],
      color_cw[3]
    )
  ) + 
  geom_text(
    data = data |> filter(pass_distance == 1 & pass_location == 'middle'),
    aes(label = heatmap_label),
    color = ifelse(
      data |> filter(pass_distance == 1 &
                        pass_location == 'middle') |> pull(completion_pct) < contrast_point,
      color_cw[5],
      color_cw[3]
    )
  ) + 
  geom_text(
    data = data |> filter(pass_distance == 1 & pass_location == 'right'),
    aes(label = heatmap_label),
    color = ifelse(
      data |> filter(pass_distance == 1 &
                        pass_location == 'right') |> pull(completion_pct) < contrast_point,
      color_cw[5],
      color_cw[3]
    )
  ) + 
  geom_text(
    data = data |> filter(pass_distance == 2 & pass_location == 'left'),
    aes(label = heatmap_label),
    color = ifelse(
      data |> filter(pass_distance == 2 &
                        pass_location == 'left') |> pull(completion_pct) < contrast_point,
      color_cw[5],
      color_cw[3]
    )
  ) + 
  geom_text(
    data = data |> filter(pass_distance == 2 &
                             pass_location == 'middle'),
    aes(label = heatmap_label),
    color = ifelse(
      data |> filter(pass_distance == 2 &
                        pass_location == 'middle') |> pull(completion_pct) < contrast_point,
      color_cw[5],
      color_cw[3]
    )
  ) + 
  geom_text(
    data = data |> filter(pass_distance == 2 &
                             pass_location == 'right'),
    aes(label = heatmap_label),
    color = ifelse(
      data |> filter(pass_distance == 2 &
                        pass_location == 'right') |> pull(completion_pct) < contrast_point,
      color_cw[5],
      color_cw[3]
    )
  ) + 
  geom_text(
    data = data |> filter(pass_distance == 3 & pass_location == 'left'),
    aes(label = heatmap_label),
    color = ifelse(
      data |> filter(pass_distance == 3 &
                        pass_location == 'left') |> pull(completion_pct) < contrast_point,
      color_cw[5],
      color_cw[3]
    )
  ) + 
  geom_text(
    data = data |> filter(pass_distance == 3 & pass_location == 'middle'),
    aes(label = heatmap_label),
    color = ifelse(
      data |> filter(pass_distance == 3 &
                        pass_location == 'middle') |> pull(completion_pct) < contrast_point,
      color_cw[5],
      color_cw[3]
    )
  ) + 
  geom_text(
    data = data |> filter(pass_distance == 3 & pass_location == 'right'),
    aes(label = heatmap_label),
    color = ifelse(
      data |> filter(pass_distance == 3 &
                        pass_location == 'right') |> pull(completion_pct) < contrast_point,
      color_cw[5],
      color_cw[3]
    )
  ) + 
  coord_fixed() + 
  scale_x_discrete(
    labels = c(
      'left' = 'Left',
      'middle' = 'Middle',
      'right' = 'Right'
    )
  ) +
  scale_y_discrete(
    labels = c(
      '0' = 'LOS',
      '1' = '0-9 yds',
      '2' = '10-19 yds',
      '3' = '20+ yds'
    )
  ) + 
  labs(
    title = glue('{first(data$full_name)} Completion Percentage by Zone'),
    subtitle = glue('{first(data$season)} Season')
  ) + 
  theme_cw_dark

