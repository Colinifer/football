# Data --------------------------------------------------------------------

current_season <- year

con <- fx.db_con(x.host = 'localhost')
pbp <- tbl(con, 'nflfastR_pbp') %>% 
  filter(season >= 2006) %>% 
  collect()
dbDisconnect(con)

player_stats <- map_df(pbp %>%
                         pull(season) %>%
                         unique(),
                       function(x) {
                         pbp %>%
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

top_rushers <- ff_players %>%
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
    receiving_yards,
    on_roster
  ) %>%
  arrange(-hvt)


# RB HVT ------------------------------------------------------------------
# https://fantasyevaluator.com/nfl-tools/rb-hvt/
fx.ff_free_agents(league_name = 'Beep Boop') %>% 
  filter(hvt > 0) %>%
  ggplot(aes(x = hvt_percentage, y = hvt_per_game)) +
  geom_point(
    aes(
      color = team_color2,
      fill = team_color
      ),
    shape = 21,
    size = 3
  ) + 
  scale_color_identity() + 
  scale_fill_identity() + 
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
fx.ff_free_agents(league_name = 'Family') %>%
  filter(air_yards_share > .08 & 
           target_share > .08) %>% 
  # filter(on_roster == FALSE) %>% 
  ggplot(aes(x = target_share, y = air_yards_share)) +
  geom_point(
    aes(color = team_color2,
    fill = team_color),
    shape = 21,
    size = 3
  ) +
  scale_color_identity() + 
  scale_fill_identity() + 
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
  # scale_color_manual(values =  team_color2,
  #                    name = "Team") +
  theme_cw_dark

sample_players <- player_stats %>%
  filter(targets >= 50 | attempts >= 150 | carries >= 40) %>% 
  pull(player_id)

player_stats_weekly %>% 
  left_join(
    roster_df %>% 
      select(
        gsis_id,
        position
      ),
    by = c('player_id' = 'gsis_id')) %>% 
  filter(position == 'WR' & 
           player_name %in% c('J.Jefferson', 'D.Samuel', 'A.Cooper', 'R.Anderson')) %>% 
  ggplot(aes(x = week, y = wopr)) +
  geom_line(aes(group = player_id))

# Targets Share & ADoT plot -----------------------------------------------
# https://fantasyevaluator.com/nfl-tools/market-share/
fx.ff_free_agents(league_name = 'Family') %>% 
  mutate(adot = receiving_air_yards / targets) %>% 
  filter(air_yards_share > .08 & 
           target_share > .08) %>% 
  # filter(on_roster == FALSE) %>% 
  ggplot(aes(x = target_share, y = adot)) +
  geom_point(
    aes(color = team_color2,
        fill = team_color),
    shape = 21,
    size = 3
  ) +
  scale_color_identity() + 
  scale_fill_identity() + 
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
  # scale_color_manual(values =  team_color2,
  #                    name = "Team") +
  theme_cw_dark
