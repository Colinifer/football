library(tidyverse)
library(nflfastR)
load(url('https://github.com/guga31bb/metrics/blob/master/dakota_model.rda?raw=true'))

# choose seasons for which the plot shall be generated
# CPOE starts in 2006
season <- year

# Load pbp for the chosen season from nflfastR data repo
# can be multiple seasons
# lapply(2006:2020, function(season){
pbp_df <-
  purrr::map_df(season, function(x) {
    readRDS(url(glue::glue("https://github.com/guga31bb/nflfastR-data/blob/master/data/play_by_play_{x}.rds?raw=true")))
  }) %>% decode_player_ids(fast = TRUE) %>% 
  mutate(defteam = ifelse(defteam == "LA", "LAR", defteam),
         posteam = ifelse(posteam == "LA", "LAR", posteam),
         posteam = ifelse(season < 2016 & posteam == 'LAR', 'STL', posteam),
         defteam = ifelse(season < 2016 & defteam == 'LAR', 'STL', defteam),
         posteam = ifelse(season < 2017 & posteam == 'LAC', 'SD', posteam),
         defteam = ifelse(season < 2017 & defteam == 'LAC', 'SD', defteam),
         posteam = ifelse(season < 2020 & posteam == 'LV', 'OAK', posteam),
         defteam = ifelse(season < 2020 & defteam == 'LV', 'OAK', defteam))

# compute cpoe grouped by air_yards
epa_cpoe <-
  pbp_df %>%
  filter(!is.na(cpoe) & !is.na(epa)) %>%
  group_by(game_id, defteam) %>%
  summarise(cpoe = mean(cpoe), epa = mean(epa)) %>% 
  left_join(pbp_df %>%
              filter(!is.na(cpoe) &
                       !is.na(epa)) %>%
              group_by(defteam) %>%
              summarise(cpoe = mean(cpoe), epa_per_play = mean(epa)) %>%
              mutate(season_dakota = mgcv::predict.gam(dakota_model, .)) %>%
              select(-cpoe, -epa_per_play),
            by = c('defteam')
  )

# summarise cpoe using player ID (note that player ids are 'NA' for 'no_play' plays. 
# Since we would filter those plays anyways we can use the id here)
# The correct name is being joined using the roster data
# first arranged by number of plays to filter the 30 QBs with most pass attempts
# The filter is set to 30 because we want to have 6 columns and 5 rows in the facet
summary_df <-
  pbp_df %>%
  filter(!is.na(cpoe) & !is.na(epa)
  ) %>% 
  group_by(game_id, week, defteam) %>%
  summarise(pa = n(),
            total_cpoe = mean(cpoe),
            total_epa = mean(epa)
  ) %>%
  left_join(pbp_df %>% 
              select(defteam, 
                     team = posteam
              ) %>% 
              unique(),
            by = c('defteam')
  ) %>% 
  group_by(defteam) %>% 
  mutate(season_pa = sum(pa, na.rm = T)) %>% 
  ungroup %>% 
  arrange(season_pa %>% 
            desc()
  ) %>% 
  left_join(epa_cpoe, by = c('defteam', 'game_id')) %>% 
  arrange(season_dakota) %>% 
  mutate(lab_dakota = glue('DAKOTA: {season_dakota %>% round(3)}')) %>% 
  left_join(
    teams_colors_logos %>% select(team_abbr, team_color, team_logo_espn),
    by = c('defteam' = 'team_abbr')
  ) %>% mutate(season = game_id %>% substr(1, 4)) %>% 
  mutate_at(vars(season_dakota), funs(factor(., levels=unique(.))))

# create data frame used to add the logos
# arranged by name because name is used for the facet
colors_raw <-
  summary_df %>%
  group_by(defteam) %>%
  summarise(defteam = first(defteam)) %>%
  left_join(
    teams_colors_logos %>% select(team_abbr, team_color),
    by = c("defteam" = "team_abbr")
  ) %>%
  arrange(defteam)

# the below used smooth algorithm uses the parameter n as the number
# of points at which to evaluate the smoother. When using color as aesthetics
# we need exactly the same number of colors (-> n times the same color per player)
n_eval <- 80
colors <-
  as.data.frame(lapply(colors_raw, rep, n_eval)) %>%
  arrange(defteam)

# mean data frame for the smoothed line of the whole league
mean <-
  summary_df %>%
  summarise(league_cpoe = mean(total_cpoe), 
            league_epa = mean(total_epa)
            )

summary_images_df <- 
  summary_df %>% 
  select(defteam, season_dakota, lab_dakota, team_logo_espn, season_pa) %>% 
  unique() %>% 
  arrange(season_dakota %>% desc())

panel_label <- summary_images_df$defteam
names(panel_label) <- summary_images_df$season_dakota

# create the plot. Set asp to make sure the images appear in the correct aspect ratio
asp <- 16/16
p <-
  summary_df %>%
  ggplot(aes(x = cpoe, y = epa)) +
  geom_hline(yintercept = mean$league_epa, 
             color = "red", 
             linetype = "dashed") +
  geom_vline(xintercept =  mean$league_cpoe, 
             color = "red", 
             linetype = "dashed") +
  geom_point(aes(color = defteam), 
             size = 1.2, 
             shape = 16,
             alpha = 0.75 ) +
  scale_color_manual(values =  NFL_pri_dark,
                     name = "Team") +
  # geom_point(aes(fill = team), shape = 21, size = 1 , alpha = 0.1) +
  # geom_point(alpha = 0.2, aes(color = team), size = .5) +
  # scale_color_manual(values =  NFL_pri_dark,
  #                    name = "Team") +
  geom_point(data = summary_df %>% 
               group_by(defteam) %>% 
               filter(row_number()==n()), 
             aes(fill = defteam),
             color = color_cw[5],
             shape = 21, 
             size = 1.2) +
  scale_fill_manual(values =  NFL_pri_dark,
                     name = "Team") +
  geom_shadowtext(data = summary_images_df,
                  aes(label = lab_dakota, 
                      x = 37.5, 
                      y = -1.23),
                  color = color_cw[5],
                  bg.color = color_cw[2],
                  bg.r = .3,
                  hjust = 1,
                  family = "Montserrat",
                  size = 1.4) +
  ggimage::geom_image(data = summary_images_df, aes(x = 26.5, y = -.8, image = team_logo_espn),
                      size = .25, by = "width", asp = asp
  ) +
  coord_cartesian(xlim = c(-35, 35), ylim = c(-1.25, 1.25))# 'zoom in'

p_desktop <- p +
  labs(
    x = 'Completion Percentage Over Expectation (CPOE in percentage points)',
    y = 'EPA per Pass Attempt',
    title = glue::glue('Defensive Passing Efficiency Allowed by Game {season}'),
    subtitle = "Defenses allowing the lowest QB DAKOTA to opposing teams, ordered by @benbbaldwin's DAKOTA rating\nWhite Dot = Most Recent Game. Red Line = League Average."
  ) +
  facet_wrap(~season_dakota, labeller = labeller(season_dakota = panel_label), ncol = 8) +
  theme_cw +
  theme(
    axis.title = element_text(size = 8),
    axis.text = element_text(size = 5),
    axis.ticks = element_line(color = color_cw[5], size = 0.3),
    axis.ticks.length = unit(2, 'pt'),
    axis.title.y = element_text(angle = 90),
    plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 6),
    # plot.margin = margin(1, 1, 1, 1, unit = "cm"),
    panel.background = element_rect(fill = color_cw[2]),
    panel.spacing.x = unit(1.25, "lines"),
    panel.spacing.y = unit(1, "lines"),
    legend.position = "none",
    legend.title = element_blank(),
    legend.text = element_blank(),
    strip.text = element_text(size = 4, hjust = 0.5, face = "bold")
  )

p_mobile <- p +
  labs(
    x = 'Completion Percentage Over Expectation (CPOE in percentage points)',
    y = 'EPA per Pass Attempt',
    title = glue::glue('Defensive Passing Efficiency by Game {season}'),
    subtitle = "Defenses allowing the lowest QB DAKOTA to opposing teams,\nordered by @benbbaldwin's DAKOTA rating\nWhite Dot = Most Recent Game. Red Line = League Average."
  ) +
  facet_wrap(~season_dakota, labeller = labeller(season_dakota = panel_label), ncol = 4) +
  theme_cw +
  theme(
    axis.title = element_text(size = 8),
    axis.text = element_text(size = 5),
    axis.ticks = element_line(color = color_cw[5], size = 0.3),
    axis.ticks.length = unit(2, 'pt'),
    axis.title.y = element_text(angle = 90),
    plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 6),
    plot.margin = margin(.25, 1, .25, .25, unit = "cm"),
    panel.background = element_rect(fill = color_cw[2]),
    panel.spacing.x = unit(1.25, "lines"),
    panel.spacing.y = unit(.7, "lines"),
    legend.position = "none",
    legend.title = element_blank(),
    legend.text = element_blank(),
    strip.text = element_text(size = 4, hjust = 0.5, face = "bold")
  )

# Desktop
# save the plot
brand_plot(p_desktop, asp = 16/10, save_name = glue('plots/desktop/defense/defense_epa_vs_cpoe/defense_epa_vs_cpoe_{season}.png'), data_home = 'Data: @nflfastR', fade_borders = '')

# Mobile
# save the plot
brand_plot(p_mobile, asp = 9/16, save_name = glue('plots/mobile/defense/defense_epa_vs_cpoe/defense_epa_vs_cpoe_{season}.png'), data_home = 'Data: @nflfastR', fade_borders = '')

rm(season, epa_cpoe, summary_df, colors_raw, n_eval, colors, mean, summary_images_df, panel_label, asp, p, p_desktop, p_mobile)
# })
