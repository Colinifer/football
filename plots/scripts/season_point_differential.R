year

# all_pbp_df <-
#   do.call(rbind, lapply(dir(glue('data/pbp'), 
#                      full = T) %>% 
#                        .[which(grepl('.rds', .) &
#                                  !grepl('xyac', .))], 
#                      function(x){
#                        print(x)
#                        readRDS(x)})
#           )

con <- fx.db_con(x.host = 'localhost')
clean_all_pbp_df <-
  tbl(con, 'nflfastR_pbp') %>%
  filter(season_type == 'REG') %>%
  select(season,
         game_id,
         week,
         home_team,
         away_team,
         home_score,
         away_score,
         posteam,
         defteam,
         -xyac_median_yardage) %>% 
  collect() %>% 
  mutate(defteam = ifelse(defteam == "LA", "LAR", defteam),
         posteam = ifelse(posteam == "LA", "LAR", posteam),
         posteam = ifelse(season < 2016 & posteam == 'LAR', 'STL', posteam),
         defteam = ifelse(season < 2016 & defteam == 'LAR', 'STL', defteam),
         posteam = ifelse(season < 2017 & posteam == 'LAC', 'SD', posteam),
         defteam = ifelse(season < 2017 & defteam == 'LAC', 'SD', defteam),
         posteam = ifelse(season < 2020 & posteam == 'LV', 'OAK', posteam),
         defteam = ifelse(season < 2020 & defteam == 'LV', 'OAK', defteam),
         home_team = ifelse(home_team == "LA", "LAR", home_team),
         away_team = ifelse(away_team == "LA", "LAR", away_team),
         home_team = ifelse(season < 2016 & home_team == 'LAR', 'STL', home_team),
         away_team = ifelse(season < 2016 & away_team == 'LAR', 'STL', away_team),
         home_team = ifelse(season < 2017 & home_team == 'LAC', 'SD', home_team),
         away_team = ifelse(season < 2017 & away_team == 'LAC', 'SD', away_team),
         home_team = ifelse(season < 2020 & home_team == 'LV', 'OAK', home_team),
         away_team = ifelse(season < 2020 & away_team == 'LV', 'OAK', away_team)) %>% 
  mutate(home_result = home_score - away_score,
         away_result = away_score - home_score)
dbDisconnect(con)

all_point_diff <- clean_all_pbp_df %>% 
  group_by(season, game_id, week, home_team, home_result) %>% 
  summarize(result = last(home_result)) %>% 
  select(-home_result) %>% 
  rename(team = home_team) %>% 
  rbind(clean_all_pbp_df %>% 
          group_by(season, game_id, week, away_team, away_result) %>% 
          summarize(result = last(away_result)) %>% 
          select(-away_result) %>% 
          rename(team = away_team)) %>%
  arrange(season, team, week) %>% 
  mutate(season_team = glue('{season} {team}')) %>% 
  group_by(season_team) %>% 
  # mutate(result = lag(result) + result)
  mutate(season_point_diff = cumsum(result)) %>% 
  arrange(season_team, week)
  # left_join(teams_colors_logos %>% 
  #             select(team_abbr, team_color, team_color2),
  #           by = c('team' = 'team_abbr'))

all_point_diff$nfl_pri_dark <- NFL_pri_dark[match(all_point_diff$team, names(NFL_pri_dark))]

# Loop through recent seasons
# lapply(1999:2020, function(year){

n_week <- fx.n_week(pbp_df %>% filter(season == year))

# Get best/worst teams in current and all seasons
best_worst_teams <- c(
  # Get best team since 1999
  all_point_diff %>% 
    filter(season_point_diff == all_point_diff$season_point_diff %>% 
             max()) %>% 
    pull(season_team),
  # Get worst team since 1999
  all_point_diff %>% 
    filter(season_point_diff == all_point_diff$season_point_diff %>% 
             min()) %>% 
    pull(season_team),
  # Get best team from current season
  all_point_diff %>% 
    filter(season == year &
             week == n_week) %>% 
    arrange(season_point_diff %>% 
              desc()
            ) %>% 
    head(1) %>% 
    pull(season_team),
  # Get worst team from current season
  all_point_diff %>% 
    filter(season == year &
             week == n_week) %>% 
    arrange(season_point_diff) %>% 
    head(1) %>% 
    pull(season_team)
)

p <- all_point_diff %>% 
  ggplot() + 
  geom_line(
    aes(
      x = week, 
      y = season_point_diff, 
      group = factor(season_team)),
    color = color_cw[4],
    alpha = .25
    ) + 
  geom_line(
    data = all_point_diff %>% 
      filter(season_team %in% best_worst_teams),
    aes(
      x = week, 
      y = season_point_diff, 
      group = factor(season_team)),
    color = all_point_diff %>% 
      filter(season_team %in% best_worst_teams) %>% 
      pull(nfl_pri_dark),
    size = 1,
    alpha = 1
  ) +
  geom_label_repel(
    data = all_point_diff %>% 
      filter(season_team %in% best_worst_teams) %>% 
      group_by(season_team) %>% 
      filter(row_number()==n()),
    aes(x = week,
        y = season_point_diff,
        label = glue('{season_team}: {ifelse(season_point_diff>0, glue("+{season_point_diff}"),season_point_diff)}')),
    size = 2,
    nudge_y = ifelse(all_point_diff %>% 
                       filter(season_team %in% best_worst_teams) %>% 
                       group_by(season_team) %>% 
                       filter(row_number()==n()) %>% 
                       pull(season_point_diff) > 0, -50, 50),
    label.size = 0,
    segment.size = .8,
    fill = color_cw[4],
    color = color_cw[5],
    segment.colour = color_cw[4],
    family = 'Montserrat'
  ) + 
  geom_point(
    data = all_point_diff %>% 
      filter(season_team %in% best_worst_teams) %>% 
      group_by(season_team) %>% 
      filter(row_number()==n()),
    aes(
      x = week, 
      y = season_point_diff, 
      group = factor(season_team)),
    color = all_point_diff %>% 
      filter(season_team %in% best_worst_teams) %>% 
      group_by(season_team) %>% 
      filter(row_number()==n()) %>% 
      pull(nfl_pri_dark),
    size = 1.1,
    alpha = 1
  ) + 
  coord_cartesian(xlim = c(1.74,16.3)) + 
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10, 12, 14, 16)) + 
  labs(
    x = 'Week',
    y = 'Point Differential',
    title = glue('{year} Best/Worst Team Point Differential'),
    subtitle = 'All Regular Season Games from 1999-2020'
  )

p_dark <- p + 
  theme_cw_dark +
  theme(
    legend.position = 'none',
    # panel.grid.minor = element_line(color=color_cw[1], size = 0.3),
    panel.grid.major = element_line(color=color_cw[1], size = 0.4)
  )

p_light <- p  +
  theme_cw_light +
  theme(
    legend.position = 'none',
    # panel.grid.minor = element_line(color=color_cw[1], size = 0.3),
    panel.grid.major = element_line(color=color_cw[1], size = 0.4)
  )

brand_plot(p_dark,  asp = 16/10, save_name = glue('plots/desktop/team_tiers/season_point_diff_{year}_dark.png'), data_home = 'Data: @nflfastR', fade_borders = '')

brand_plot(p_light,  asp = 16/10, save_name = glue('plots/desktop/team_tiers/season_point_diff_{year}_light.png'), data_home = 'Data: @nflfastR', fade_borders = '')


# rm(clean_all_pbp_df, all_point_diff, best_worst_teams, p)
# })
