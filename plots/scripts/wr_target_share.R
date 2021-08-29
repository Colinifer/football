# Data --------------------------------------------------------------------

current_season <- year

con <- fx.db_con()
pbp_df <- tbl(con, 'nflfastR_pbp') %>% 
  filter(season >= current_season-6) %>% 
  collect()


player_stats <- map_df(pbp_df %>%
                         pull(season) %>%
                         unique(),
                       function(x) {
                         pbp_df %>%
                           filter(season == x) %>%
                           calculate_player_stats_mod() %>%
                           mutate(season = x) %>%
                           left_join(
                             roster_df %>% 
                               filter(season == x) %>% 
                               select(season,
                                      gsis_id,
                                      position),
                             by = c('player_id' = 'gsis_id', 'season')
                           ) %>% 
                           left_join(
                             teams_colors_logos %>% select(team_abbr, team_color, team_color2),
                             by = c('recent_team' = 'team_abbr')
                           ) %>%
                           select(season,
                                  recent_team,
                                  player_id,
                                  player_name,
                                  position,
                                  everything())
                       })

top_rushers <- player_stats %>%
  select(
    season,
    recent_team,
    player_id,
    player_name,
    position,
    games,
    hvt,
    hvt_percentage,
    receiving_air_yards,
    receiving_yards
  ) %>%
  arrange(-hvt)


# RB HVT ------------------------------------------------------------------
# https://fantasyevaluator.com/nfl-tools/rb-hvt/
player_stats %>%
  filter(season >= 2019 &
           carries >= 50) %>%
  ggplot(aes(x = hvt_percentage, y = hvt_per_game)) +
  geom_point(
    color = player_stats %>%
      filter(season >= 2019 &
               carries >= 50) %>% pull(team_color2),
    fill = player_stats %>%
      filter(season >= 2019 &
               carries >= 50) %>% pull(team_color),
    shape = 21,
    size = 3
  ) +
  geom_text_repel(
    aes(label = player_name),
    # segment.color = p_data %>%
    #   arrange(-pbwr) %>%
    #   pull(team_color),
    min.segment.length = .5,
    family = 'Montserrat',
    color = color_cw[5],
    size = 2.5,
    nudge_y = -.007
  ) +
  theme_cw_dark

# Air Yards Market Share plot ---------------------------------------------
# https://fantasyevaluator.com/nfl-tools/market-share/
player_stats %>%
  filter(season == 2020 &
           targets >= 50) %>%
  ggplot(aes(x = target_share, y = air_yards_share)) +
  geom_point(
    color = player_stats %>%
      filter(season == 2020 &
               targets >= 50) %>% pull(team_color2),
    fill = player_stats %>%
      filter(season == 2020 &
               targets >= 50) %>% pull(team_color),
    shape = 21,
    size = 3
  ) +
  geom_text_repel(
    aes(label = player_name),
    # segment.color = p_data %>%
    #   arrange(-pbwr) %>%
    #   pull(team_color),
    min.segment.length = .5,
    family = 'Montserrat',
    color = color_cw[5],
    size = 2.5,
    nudge_y = -.007
  ) +
  theme_cw_dark

stats %>% 
  ggplot(
    
  )