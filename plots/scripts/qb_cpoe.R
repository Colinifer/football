library(tidyverse)
library(nflfastR)

# choose seasons for which the plot shall be generated
# CPOE starts in 2006
season <- 2020

# load pbp for the choosen seasosn from nflfastR data repo
# can be multiple seasons as well
lapply(2009:2019, function(season){

# Download play-by-play data, decode player IDs, and 
pbp_df <-
  purrr::map_df(season, function(x) {
    readRDS(url(glue::glue("https://github.com/guga31bb/nflfastR-data/blob/master/data/play_by_play_{x}.rds?raw=true")))
  }) %>% decode_player_ids(fast = TRUE) %>% 
  mutate(defteam = ifelse(defteam == "LA", "LAR", defteam),
         posteam = ifelse(posteam == "LA", "LAR", posteam))

# load roster data from nflfastR data repo
roster_df <-
  readRDS(url("https://github.com/guga31bb/nflfastR-data/blob/master/roster-data/roster.rds?raw=true")) %>% 
  decode_player_ids(fast = TRUE) %>% 
  mutate(team.abbr = ifelse(team.abbr == "LA", "LAR", team.abbr))

# compute cpoe grouped by air_yards
cpoe <-
  pbp_df %>%
  filter(!is.na(cpoe)) %>%
  group_by(passer_player_id, air_yards) %>%
  summarise(count = n(), cpoe = mean(cpoe))

# summarise cpoe using player ID (note that player ids are 'NA' for 'no_play' plays. 
# Since we would filter those plays anyways we can use the id here)
# The correct name is being joined using the roster data
# first arranged by number of plays to filter the 30 QBs with most pass attempts
# The filter is set to 30 because we want to have 6 columns and 5 rows in the facet
summary_df <-
  pbp_df %>%
  filter(!is.na(cpoe)
         ) %>%
  group_by(passer_player_id) %>%
  summarise(plays = n(),
            total_cpoe = mean(cpoe)
            ) %>%
  arrange(plays %>% desc()
          ) %>%
  left_join(pbp_df %>% 
              filter(!is.na(passer_player_id)
                     ) %>% 
              select(passer_player_id, 
                     team = posteam
                     ) %>% 
              unique(),
            by = c('passer_player_id')
            ) %>% 
  head(32) %>%
  arrange(total_cpoe %>% 
            desc()
          ) %>%
  left_join(
    sleeper_players_df %>%
      select(position, full_name, sportradar_id, gsis_id, espn_id, headshot_url),
    by = c('passer_player_id' = 'gsis_id')
  ) %>%
  # left_join(
  #   as_tibble(roster_df) %>%
  #     select(team = team.abbr,
  #            first_name = teamPlayers.firstName,
  #            last_name = teamPlayers.lastName,
  #            gsis = teamPlayers.gsisId,
  #            headshot_url = teamPlayers.headshot_url
  #            ) %>%
  #     mutate(full_name = glue('{first_name} {last_name}')) %>%
  #     select(-first_name, -last_name) %>% unique(),
  #   by = c('passer_player_id' = 'gsis', 'team')
  # ) %>%
  mutate(# some headshot urls are broken. They are checked here and set to a default 
    headshot_url = dplyr::if_else(
      RCurl::url.exists(as.character(headshot_url)),
      as.character(headshot_url),
      'http://static.nfl.com/static/content/public/image/fantasy/transparent/200x200/default.png',
    )
  ) %>%
  left_join(cpoe, by = 'passer_player_id') %>%
  left_join(
    teams_colors_logos %>% select(team_abbr, team_color, team_logo_espn),
    by = c('team' = 'team_abbr')
  ) %>% 
  mutate(facet_label_wrap = glue('{full_name}: {total_cpoe}'),
         total_cpoe = total_cpoe %>% round(3)) %>% 
  mutate_at(vars(total_cpoe), funs(factor(., levels=unique(.))))

# create data frame used to add the logos
# arranged by name because name is used for the facet
colors_raw <-
  summary_df %>%
  group_by(passer_player_id) %>%
  summarise(team = first(team), name = first(full_name)) %>%
  left_join(
    teams_colors_logos %>% select(team_abbr, team_color),
    by = c("team" = "team_abbr")
  ) %>%
  arrange(name)

# the below used smooth algorithm uses the parameter n as the number
# of points at which to evaluate the smoother. When using color as aesthetics
# we need exactly the same number of colors (-> n times the same color per player)
n_eval <- 80
colors <-
  as.data.frame(lapply(colors_raw, rep, n_eval)) %>%
  arrange(name)

# mean data frame for the smoothed line of the whole league
mean <-
  summary_df %>%
  group_by(air_yards) %>%
  summarise(league = mean(cpoe), league_count = n())

summary_images_df <- 
  summary_df %>% 
  select(full_name, passer_player_id, total_cpoe, headshot_url, team_logo_espn) %>% 
  mutate(status = color_cw[5],
         lab_cpoe = glue('Total CPOE: {total_cpoe}')) %>% 
  unique()

# Create a named character vector to replace value with name in the facet titles
panel_label <- summary_df$full_name
names(panel_label) <- summary_df$total_cpoe

# create the plot. Set asp to make sure the images appear in the correct aspect ratio
asp <- 16/16

# Desktop
p <-
  summary_df %>% 
  # arrange(total_cpoe %>% desc()) %>% 
  ggplot(aes(x = air_yards, y = cpoe)) +
  geom_smooth(data = mean, aes(x = air_yards, 
                     y = league, 
                     weight = league_count), 
              n = n_eval, 
              color = "red", 
              alpha = 0.7, 
              se = FALSE, 
              size = 0.5, 
              linetype = "dashed"
  ) +
  geom_smooth(se = FALSE, alpha = 0.9, 
              aes(color = team, weight = count), 
              size = 0.65, n = n_eval
  ) +
  scale_color_manual(values =  NFL_pri_dark,
                     name = "Team") +
  geom_point(aes(color = team), 
             size = summary_df$count / 15, 
             alpha = 0.4) + 
  # scale_fill_manual(values =  NFL_pri,
  #                   name = "Team") +
  ggimage::geom_image(data = summary_images_df, aes(x = 27.5, y = -16.5, image = team_logo_espn),
    size = .2, by = "width", asp = asp
  ) +
  ggimage::geom_image(data = summary_images_df, aes(x = 0, y = -19, image = headshot_url),
    size = .4, by = "width", asp = asp
  ) +
  geom_shadowtext(data = summary_images_df,
                  aes(label = lab_cpoe, 
                      x = 31, 
                      y = -24.5),
                  color = color_cw[5],
                  bg.color = color_cw[3],
                  hjust = 1,
                  family = "Montserrat",
                  size = 1.3) +
  xlim(-10, 40) + # makes sure the smoothing algorithm is evaluated between -10 and 40
  coord_cartesian(xlim = c(-5, 30), ylim = c(-25, 25)) + # 'zoom in'
  labs(
    x = "Target Depth In Yards Thrown Beyond The Line Of Scrimmage (DOT)",
    y = "Completion Percentage Over Expectation (CPOE)",
    title = glue::glue("Passing Efficiency {season}"),
    subtitle = "CPOE as a function of target depth. Dotsize equivalent to number of targets. Smoothed for -10 ≤ DOT ≤ 30 Yards.\nRed Line = League Average."
  ) +
  # Use the named character vector to replace CPOE rank with Player name
  facet_wrap(~total_cpoe, labeller = labeller(total_cpoe = panel_label), ncol = 8) +
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

# p

# save the plot
brand_plot(p, asp = 16/10, save_name = glue('plots/desktop/qb_cpoe_vs_dot_{season}.png'), data_home = 'Data: @nflfastR', fade_borders = '')

# Mobile
p <-
  summary_df %>% 
  # arrange(total_cpoe %>% desc()) %>% 
  ggplot(aes(x = air_yards, y = cpoe)) +
  geom_smooth(data = mean, aes(x = air_yards, 
                               y = league, 
                               weight = league_count), 
              n = n_eval, 
              color = "red", 
              alpha = 0.7, 
              se = FALSE, 
              size = 0.5, 
              linetype = "dashed"
  ) +
  geom_smooth(se = FALSE, alpha = 0.9, 
              aes(color = team, weight = count), 
              size = 0.65, n = n_eval
  ) +
  scale_color_manual(values =  NFL_pri_dark,
                     name = "Team") +
  geom_point(aes(color = team), 
             size = summary_df$count / 15, 
             alpha = 0.4) + 
  # scale_fill_manual(values =  NFL_pri,
  #                   name = "Team") +
  ggimage::geom_image(data = summary_images_df, aes(x = 27.5, y = -16.5, image = team_logo_espn),
                      size = .2, by = "width", asp = asp
  ) +
  ggimage::geom_image(data = summary_images_df, aes(x = 0, y = -19, image = headshot_url),
                      size = .4, by = "width", asp = asp
  ) +
  geom_shadowtext(data = summary_images_df,
                  aes(label = lab_cpoe, 
                      x = 31, 
                      y = -24.5),
                  color = color_cw[5],
                  bg.color = color_cw[3],
                  hjust = 1,
                  family = "Montserrat",
                  size = 1.3) +
  xlim(-10, 40) + # makes sure the smoothing algorithm is evaluated between -10 and 40
  coord_cartesian(xlim = c(-5, 30), ylim = c(-25, 25)) + # 'zoom in'
  labs(
    x = "Target Depth In Yards Thrown Beyond The Line Of Scrimmage (DOT)",
    y = "Completion Percentage Over Expectation (CPOE)",
    title = glue::glue("Passing Efficiency {season}"),
    subtitle = "CPOE as a function of target depth. Dotsize equivalent to number of targets.\nSmoothed for -10 ≤ DOT ≤ 30 Yards.\nRed Line = League Average."
  ) +
  # Use the named character vector to replace CPOE rank with Player name
  facet_wrap(~total_cpoe, labeller = labeller(total_cpoe = panel_label), ncol = 4) +
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
    panel.spacing.y = unit(.75, "lines"),
    legend.position = "none",
    legend.title = element_blank(),
    legend.text = element_blank(),
    strip.text = element_text(size = 4, hjust = 0.5, face = "bold")
  )

# p

# save the plot
brand_plot(p, asp = 9/16, save_name = glue('plots/mobile/qb_cpoe_vs_dot_{season}.png'), data_home = 'Data: @nflfastR', fade_borders = '')
})
