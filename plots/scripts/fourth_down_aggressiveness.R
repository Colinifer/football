current_season <- 2020

nfl_colors <- tibble(
  NFL_pri %>% names(), 
  NFL_pri
)

names(nfl_colors) <- c('team_name', 'team_color')

# con <- fx.db_con()
pbp <- 
  # tbl(con, 'nflfastR_pbp') %>% 
  nflfastR::load_pbp(current_season) %>%
  filter(season == current_season & 
           down == 4) %>% 
  # collect() %>% 
  identity()
# dbDisconnect(con)

decisions <- 
  readRDS(url(glue('https://github.com/guga31bb/fourth_calculator/blob/main/data/decisions_{current_season}.rds?raw=true'))) %>% 
  group_by(posteam) %>% 
  filter(
    prior_wp > .1
  ) %>% 
  left_join(
    pbp %>% 
      select(game_id, play_id, fixed_drive, fixed_drive_result),
    by = c('game_id', 'play_id', 'fixed_drive')
  ) %>% 
  mutate(
    is_touchdown_drive = ifelse(fixed_drive_result == 'Touchdown', 1, 0)
  ) %>% 
  summarise(
    sum_should_go = sum(should_go, na.rm = TRUE),
    total_go = sum(go),
    go_rate = total_go / sum_should_go,
    touchdown_drives = sum(is_touchdown_drive, na.rm = TRUE)
  ) %>% 
  mutate(
    posteam = case_when(posteam == 'LA' ~ 'LAR',
                        TRUE ~ posteam)
  ) %>% 
  left_join(
    nfl_colors,
    by = c('posteam' = 'team_name')
  ) %>% 
  left_join(
    nflfastR::teams_colors_logos %>% select(team_abbr, team_logo_espn, team_logo_wikipedia),
    by = c('posteam' = 'team_abbr')
  ) %>%
  mutate(
    team_color = case_when(
      posteam != 'PHI' ~ '#a7a7a7ff',
      TRUE ~ team_color
    ),
    grob = map(seq_along(team_logo_espn), function(x) {
      grid::rasterGrob(magick::image_modulate(magick::image_read(team_logo_espn[[x]]), saturation = 0))
    })
  ) %>% 
  mutate(
    posteam = fct_reorder(posteam, -go_rate)
  )

go_rate_df <- pbp %>% 
  select(
    play_id,
    game_id,
    home_team,
    away_team,
    week,
    play_type,
    punt_attempt,
    field_goal_attempt,
    rush_attempt,
    pass_attempt,
    posteam,
    down,
    desc,
    half_seconds_remaining,
    wp,
    yardline_100,
    fixed_drive_result,
    NULL
  ) %>% 
  # filter(
  #   half_seconds_remaining > 60 & 
  #     wp > .2
  # ) %>%
  mutate(
    go = ifelse(play_type %notin% c('punt', 'field_goal', 'no_play', 'qb_kneel') & !is.na(play_type), 1, 0)
  ) %>% 
  group_by(posteam) %>% 
  mutate(
    is_touchdown_drive = ifelse(fixed_drive_result == 'Touchdown', 1, 0)
  ) %>% 
  summarise(
    n = n(),
    total_go = sum(go),
    go_rate = sum(go) / n,
    touchdown_drives = sum(is_touchdown_drive, na.rm = TRUE)
  ) %>% 
  mutate(
    posteam = case_when(posteam == 'LA' ~ 'LAR',
                        TRUE ~ posteam)
  ) %>% 
  left_join(
    nfl_colors,
    by = c('posteam' = 'team_name')
  ) %>% 
  left_join(
    nflfastR::teams_colors_logos %>% select(team_abbr, team_logo_espn, team_logo_wikipedia),
    by = c('posteam' = 'team_abbr')
  ) %>%
  mutate(
    team_color = case_when(
      posteam != 'PHI' ~ '#a7a7a7ff',
      TRUE ~ team_color
    ),
    grob = map(seq_along(team_logo_espn), function(x) {
      grid::rasterGrob(magick::image_modulate(magick::image_read(team_logo_espn[[x]]), saturation = 0))
    })
  )


# nflfastR situations -----------------------------------------------------

# 4th down "go" rate
go_rate_bar <- go_rate_df %>% 
  mutate(
    posteam = fct_reorder(posteam, -go_rate)
  ) %>%
  ggplot(aes(x = posteam, y = go_rate)) +
  geom_hline(aes(yintercept = mean(go_rate)),
             color = color_cw[8],
             linetype = 'dashed') +
  geom_col(
    aes(x = posteam, y = go_rate),
    width = 0.5,
    colour = go_rate_df$team_color,
    fill = go_rate_df$team_color,
    alpha = 1
  ) +
  ggpp::geom_grob(aes(x = posteam, y = go_rate, label = grob), vp.width = 0.035) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = 'Team',
    y = '"Go for it" Rate',
    title = glue('NFL 4th down aggressiveness, {current_season} '),
    subtitle = glue('How frequently did teams "go for it" on 4th down in {current_season}? Red line = League avg.'),
    caption = glue('')
  )
# ggthemes::theme_stata(scheme = 'sj', base_size = 8) +


brand_plot(
  go_rate_bar +
    theme_cw_light +
    theme(
      plot.title = element_text(size = 20),
      plot.subtitle = element_text(size = 12),
      plot.caption = element_text(hjust = 1),
      axis.title = element_text(size = 20),
      axis.title.y = element_text(angle = 90),
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_text(
        angle = 0,
        vjust = 0.5,
        size = 12
      ),
      legend.title = element_text(
        size = 8,
        hjust = 0,
        vjust = 0.5,
        face = 'bold'
      ),
      legend.position = 'top'
    ),
  asp = 16 / 10,
  save_name = glue(
    'plots/desktop/team_stats/fourth_down_aggressiveness_{current_season}_light.png'
  ),
  dark = FALSE,
  data_author = 'Chart: Colin Welsh',
  data_home = 'Data: nflfastR',
  fade_borders = ''
)

brand_plot(
  go_rate_bar +
    theme_cw_dark +
    theme(
      plot.title = element_text(size = 20),
      plot.subtitle = element_text(size = 12),
      plot.caption = element_text(hjust = 1),
      axis.title = element_text(size = 20),
      axis.title.y = element_text(angle = 90),
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_text(
        angle = 0,
        vjust = 0.5,
        size = 12
      ),
      legend.title = element_text(
        size = 8,
        hjust = 0,
        vjust = 0.5,
        face = 'bold'
      ),
      legend.position = 'top'
    ),
  asp = 16 / 10,
  save_name = glue(
    'plots/desktop/team_stats/fourth_down_aggressiveness_{current_season}_dark.png'
  ),
  dark = TRUE,
  data_author = 'Chart: Colin Welsh',
  data_home = 'Data: nflfastR',
  fade_borders = ''
)


# NYT Recommended ---------------------------------------------------------

# 4th down "go" rate
go_rate_nyt_bar <- decisions %>% 
  mutate(
    posteam = fct_reorder(posteam, -go_rate)
  ) %>%
  ggplot(aes(x = posteam, y = go_rate)) +
  geom_hline(aes(yintercept = mean(go_rate)),
             color = color_cw[8],
             linetype = 'dashed') +
  geom_col(
    aes(x = posteam, y = go_rate),
    width = 0.5,
    colour = decisions$team_color,
    fill = decisions$team_color,
    alpha = 1
  ) +
  ggpp::geom_grob(aes(x = posteam, y = go_rate, label = grob), vp.width = 0.035) +
  coord_cartesian(ylim = c(0,.50)) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = 'Team',
    y = '"Go for it" Rate',
    title = glue('NFL 4th down aggressiveness, {current_season} '),
    subtitle = glue('Red line = League avg.\nFilters: NYT recommended "go" situations, nflfastR win probability > 10%, and exclude final 1 minute of each half'))

# ggthemes::theme_stata(scheme = 'sj', base_size = 8) +

brand_plot(
  go_rate_nyt_bar +
    theme_cw_light +
    theme(
      plot.title = element_text(size = 20),
      plot.subtitle = element_text(size = 9),
      axis.title = element_text(size = 20),
      axis.title.y = element_text(angle = 90),
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_text(
        angle = 0,
        vjust = 0.5,
        size = 12
      ),
      legend.title = element_text(
        size = 8,
        hjust = 0,
        vjust = 0.5,
        face = 'bold'
      ),
      legend.position = 'top'
    ),
  asp = 16 / 10,
  save_name = glue(
    'plots/desktop/team_stats/fourth_down_aggressiveness_{current_season}_nyt_light.png'
  ),
  dark = FALSE,
  data_author = 'Chart: Colin Welsh',
  data_home = 'Data: nflfastR & NYT 4th Down Bot',
  fade_borders = ''
)

brand_plot(
  go_rate_nyt_bar +
    theme_cw_dark +
    theme(
      plot.title = element_text(size = 20),
      plot.subtitle = element_text(size = 9),
      axis.title = element_text(size = 20),
      axis.title.y = element_text(angle = 90),
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_text(
        angle = 0,
        vjust = 0.5,
        size = 12
      ),
      legend.title = element_text(
        size = 8,
        hjust = 0,
        vjust = 0.5,
        face = 'bold'
      ),
      legend.position = 'top'
    ),
  asp = 16 / 10,
  save_name = glue(
    'plots/desktop/team_stats/fourth_down_aggressiveness_{current_season}_nyt_dark.png'
  ),
  dark = TRUE,
  data_author = 'Chart: Colin Welsh',
  data_home = 'Data: nflfastR & NYT 4th Down Bot',
  fade_borders = ''
)


# Touchdowns generated from drives with 4th down conversions
touchdowns_nyt_bar <- decisions %>% 
  mutate(
    posteam = fct_reorder(posteam, -touchdown_drives)
  ) %>%
  ggplot(aes(x = posteam, y = touchdown_drives)) +
  geom_hline(aes(yintercept = mean(touchdown_drives)),
             color = color_cw[8],
             linetype = 'dashed') +
  geom_col(
    aes(x = posteam, y = touchdown_drives),
    width = 0.5,
    colour = decisions$team_color,
    fill = decisions$team_color,
    alpha = 1
  ) +
  ggpp::geom_grob(aes(x = posteam, y = touchdown_drives, label = grob), vp.width = 0.035) + 
  coord_cartesian(ylim = c(
    0,
    decisions %>% 
      pull(touchdown_drives) %>% 
      max() %>% 
      DescTools::RoundTo(5)
  )) + 
  labs(
    x = 'Team',
    y = 'Touchdowns Generated',
    title = glue('# of touchdowns from drives with 4th down conversions, {current_season} '),
    subtitle = glue('Red line = League avg.\nFilters: NYT recommended "go" situations, nflfastR win probability > 10%, and exclude final 1 minute of each half'))

# ggthemes::theme_stata(scheme = 'sj', base_size = 8) +

brand_plot(
  touchdowns_nyt_bar +
    theme_cw_light +
    theme(
      plot.title = element_text(size = 16),
      plot.subtitle = element_text(size = 9),
      axis.title = element_text(size = 16),
      axis.title.y = element_text(angle = 90),
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_text(
        angle = 0,
        vjust = 0.5,
        size = 12
      ),
      legend.title = element_text(
        size = 8,
        hjust = 0,
        vjust = 0.5,
        face = 'bold'
      ),
      legend.position = 'top'
    ),
  asp = 16 / 10,
  save_name = glue(
    'plots/desktop/team_stats/touchdowns_from_fourth_attempts_{current_season}_nyt_light.png'
  ),
  dark = FALSE,
  data_author = 'Chart: Colin Welsh',
  data_home = 'Data: nflfastR & NYT 4th Down Bot',
  fade_borders = ''
)

brand_plot(
  touchdowns_nyt_bar +
    theme_cw_dark +
    theme(
      plot.title = element_text(size = 16),
      plot.subtitle = element_text(size = 9),
      axis.title = element_text(size = 16),
      axis.title.y = element_text(angle = 90),
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_text(
        angle = 0,
        vjust = 0.5,
        size = 12
      ),
      legend.title = element_text(
        size = 8,
        hjust = 0,
        vjust = 0.5,
        face = 'bold'
      ),
      legend.position = 'top'
    ),
  asp = 16 / 10,
  save_name = glue(
    'plots/desktop/team_stats/touchdowns_from_fourth_attempts_{current_season}_nyt_dark.png'
  ),
  dark = TRUE,
  data_author = 'Chart: Colin Welsh',
  data_home = 'Data: nflfastR & NYT 4th Down Bot',
  fade_borders = ''
)

# League avg. -------------------------------------------------------------
pbp <- 
  # tbl(con, 'nflfastR_pbp') %>% 
  nflfastR::load_pbp(2017:2020) %>%
  filter(down == 4) %>% 
  # collect() %>% 
  identity()
# dbDisconnect(con)

# Playoff teams
playoff_teams <- pbp %>% 
  filter(week > 17) %>% 
  select(season, posteam) %>% 
  filter(!is.na(posteam)) %>% 
  unique() %>% 
  mutate(
    is_playoff_team = TRUE
  )

decisions <- 
  map_df(2017:2020, function(x){readRDS(url(glue('https://github.com/guga31bb/fourth_calculator/blob/main/data/decisions_{x}.rds?raw=true')))}) %>% 
  group_by(posteam, season) %>% 
  filter(
    prior_wp > .1
  ) %>% 
  left_join(
    pbp %>% 
      select(game_id, play_id, fixed_drive, fixed_drive_result),
    by = c('game_id', 'play_id', 'fixed_drive')
  ) %>% 
  mutate(
    is_touchdown_drive = ifelse(fixed_drive_result == 'Touchdown', 1, 0)
  ) %>% 
  summarise(
    sum_should_go = sum(should_go, na.rm = TRUE),
    total_go = sum(go, na.rm = TRUE),
    go_rate = total_go / sum_should_go,
    touchdown_drives = sum(is_touchdown_drive, na.rm = TRUE)
  ) %>% 
  mutate(
    posteam = case_when(posteam == 'LA' ~ 'LAR',
                        TRUE ~ posteam)
  ) %>% 
  left_join(
    nfl_colors,
    by = c('posteam' = 'team_name')
  ) %>% 
  left_join(
    nflfastR::teams_colors_logos %>% select(team_abbr, team_logo_espn, team_logo_wikipedia),
    by = c('posteam' = 'team_abbr')
  ) %>%
  mutate(
    team_color = case_when(
      posteam != 'PHI' ~ '#a7a7a7ff',
      TRUE ~ team_color
    ),
    grob = map(seq_along(team_logo_espn), function(x) {
      grid::rasterGrob(magick::image_modulate(magick::image_read(team_logo_espn[[x]]), saturation = 0))
    })
  ) %>% 
  mutate(
    posteam = fct_reorder(posteam, -go_rate)
  ) %>%
  left_join(
    playoff_teams,
    by = c('posteam', 'season')
  ) %>% 
  mutate(
    is_playoff_team = ifelse(!is.na(is_playoff_team) == TRUE, TRUE, FALSE)
  )

season_avg <- decisions %>% 
  group_by(season) %>% 
  summarise(
    go_rate_avg = mean(go_rate)
  ) %>% 
  left_join(
    decisions %>% 
      group_by(season) %>% 
      filter(
        is_playoff_team == TRUE
      ) %>% 
      summarise(
        playoff_team_go_rate_avg = mean(go_rate)
      ),
    by = c('season')
  )

league_conversion_rate <- decisions %>%
  ggplot(aes(x = season, y = go_rate)) +
  # geom_beeswarm() +
  geom_point(
    aes(x = season,
        y = go_rate, 
        alpha = 0.10),
    shape = 16,
    color = 'black',
    size = 3
  ) +
  geom_line(
    data = season_avg,
    aes(x = season, y = go_rate_avg),
    color = color_cw[8],
    linetype = 'dashed',
    size = 1.5
  ) +
  geom_line(
    data = decisions %>%
      filter(posteam == 'PHI'),
    aes(x = season,
        y = go_rate),
    color = decisions %>% filter(posteam == 'PHI') %>% pull(team_color),
    linetype = 'solid',
    size = 2
  ) +
  geom_point(data = season_avg,
             aes(x = season,
                     y = go_rate_avg),
             shape = 16,
             color = color_cw[8],
             size = 3) +
  geom_point(data = decisions %>%
               filter(posteam == 'PHI'),
             aes(x = season,
                 y = go_rate),
             shape = 16,
             color = decisions %>% filter(posteam == 'PHI') %>% pull(team_color),
             size = 3) +
  geom_label(
    data = decisions %>%
      filter(posteam == 'PHI'),
    aes(x = season, 
        y = go_rate,
        label = scales::percent(round(go_rate, digits = 4))),
    family = 'Montserrat',
    fill = decisions %>% filter(posteam == 'PHI') %>% pull(team_color),
    color = 'white',
    nudge_y = -.03,
    size = 2.5,
    label.size = 0 
  ) + 
  geom_label(
    data = season_avg,
    aes(x = season, 
        y = go_rate_avg,
        label = scales::percent(round(go_rate_avg, digits = 4))),
    family = 'Montserrat',
    fill = color_cw[8],
    color = 'white',
    nudge_y = -.03,
    size = 2.5,
    label.size = 0
  ) + 
  ggpp::geom_grob(data = decisions %>%
                    filter(posteam == 'PHI'), 
                  aes(x = season, 
                      y = go_rate, 
                      label = grob), 
                  vp.width = 0.035) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = 'Season',
    y = '"Go for it" Rate by Team',
    title = glue('League-wide "go rate" on 4th down, 2017-2020'),
    subtitle = glue(
      'Red line = League avg.\nFilters: NYT recommended "go" situations, nflfastR win probability > 10%, and exclude final 1 minute of each half'
    )
  )

# ggthemes::theme_stata(scheme = 'sj', base_size = 8) +

brand_plot(
  league_conversion_rate +
    theme_cw_light +
    theme(
      plot.title = element_text(size = 20),
      plot.subtitle = element_text(size = 9),
      axis.title = element_text(size = 20),
      axis.title.y = element_text(angle = 90),
      axis.text = element_text(
        angle = 0,
        vjust = 0.5,
        size = 12
      ),
      legend.title = element_text(
        size = 8,
        hjust = 0,
        vjust = 0.5,
        face = 'bold'
      ),
      legend.position = 'none'
    ),
  asp = 16 / 10,
  save_name = glue(
    'plots/desktop/team_stats/league_wide_go_rate_2017-2020_light.png'
  ),
  dark = FALSE,
  data_author = 'Chart: Colin Welsh',
  data_home = 'Data: nflfastR & NYT 4th Down Bot',
  fade_borders = ''
)

brand_plot(
  league_conversion_rate +
    theme_cw_dark +
    theme(
      plot.title = element_text(size = 20),
      plot.subtitle = element_text(size = 9),
      axis.title = element_text(size = 20),
      axis.title.y = element_text(angle = 90),
      axis.text = element_text(
        angle = 0,
        vjust = 0.5,
        size = 12
      ),
      legend.title = element_text(
        size = 8,
        hjust = 0,
        vjust = 0.5,
        face = 'bold'
      ),
      legend.position = 'none'
    ),
  asp = 16 / 10,
  save_name = glue(
    'plots/desktop/team_stats/league_wide_go_rate_2017-2020_dark.png'
  ),
  dark = TRUE,
  data_author = 'Chart: Colin Welsh',
  data_home = 'Data: nflfastR & NYT 4th Down Bot',
  fade_borders = ''
)
