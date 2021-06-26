nfl_colors <- tibble(
  NFL_pri %>% names(), 
  NFL_pri
)

names(nfl_colors) <- c('team_name', 'team_color')

con <- fx.db_con()
pbp <- tbl(con, 'nflfastR_pbp') %>% 
  filter(season == 2017 & 
           down == 4) %>% 
  collect()
dbDisconnect(con)

decisions <- readRDS(url('https://github.com/guga31bb/fourth_calculator/blob/main/data/decisions_2017.rds?raw=true'))

decisions %>% 
  group_by(posteam) %>% 
  filter(
    prior_wp > .1
  ) %>% 
  summarise(
    sum_should_go = sum(should_go),
    total_go = sum(go),
    go_rate = total_go / sum_should_go
  ) %>% 
  arrange(
    -go_rate
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
  summarise(
    n = n(),
    total_go = sum(go),
    go_rate = sum(go) / n
  ) %>% 
  arrange(
    -total_go
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
    grob = map(seq_along(team_logo_espn), function(x) {
      grid::rasterGrob(magick::image_read(team_logo_espn[[x]]))
    })
  ) 

go_rate_bar <- go_rate_df %>%
  arrange(-go_rate) %>% 
  ggplot(aes(x = posteam, y = go_rate)) +
  geom_hline(aes(yintercept = mean(go_rate)), color = 'red', linetype = 'dashed') +
  geom_col(width = 0.5, colour = go_rate_df$team_color, fill = go_rate_df$team_color, alpha = 0.5) +
  ggpp::geom_grob(aes(x = posteam, y = go_rate, label = grob), vp.width = 0.035) +
  # scale_x_continuous(expand = c(0,0)) +
  labs(
    x = 'Rank',
    y = 'Win Percentage Over Expectation',
    title = glue('NFL Team Efficiency {current_season}'),
    subtitle = glue('How Lucky are the Teams? Through week {n_week}')
  ) +
  # ggthemes::theme_stata(scheme = 'sj', base_size = 8) +
  theme_cw +
  theme(
    plot.title = element_text(face = 'bold'),
    plot.caption = element_text(hjust = 1),
    axis.title.y = element_text(angle = 90),
    axis.text.y = element_text(angle = 0, vjust = 0.5),
    legend.title = element_text(size = 8, hjust = 0, vjust = 0.5, face = 'bold'),
    legend.position = 'top'
  ) +
  NULL

brand_plot(wins_above_expected_bar, asp = 16/10, save_name = glue('plots/desktop/team_wins/wins_above_expected_bar_{current_season}.png'), data_home = 'Data: @nflfastR', fade_borders = '')
