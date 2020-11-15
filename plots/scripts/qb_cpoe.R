library(tidyverse)
library(nflfastR)

# choose seasons for which the plot shall be generated
# CPOE starts in 2006
season <- 2020

# load pbp for the choosen seasosn from nflfastR data repo
# can be multiple seasons as well
pbp <-
  purrr::map_df(season, function(x) {
    readRDS(url(glue::glue("https://github.com/guga31bb/nflfastR-data/blob/master/data/play_by_play_{x}.rds?raw=true")))
  })

pbp <- 
  pbp %>% 
  decode_player_ids

# load roster data from nflfastR data repo
roster <-
  readRDS(url("https://github.com/guga31bb/nflfastR-data/blob/master/roster-data/roster.rds?raw=true"))

roster <- 
  roster %>% 
  decode_player_ids()

# compute cpoe grouped by air_yards
cpoe <-
  pbp %>%
  filter(!is.na(cpoe)) %>%
  group_by(passer_player_id, air_yards) %>%
  summarise(count = n(), cpoe = mean(cpoe))

# summarise cpoe using player ID (note that player ids are 'NA' for 'no_play' plays. 
# Since we would filter those plays anyways we can use the id here)
# The correct name is being joined using the roster data
# first arranged by number of plays to filter the 30 QBs with most pass attempts
# The filter is set to 30 because we want to have 6 columns and 5 rows in the facet
summary_df <-
  pbp %>%
  filter(!is.na(cpoe)) %>%
  group_by(passer_player_id) %>%
  summarise(plays = n(),
            total_cpoe = mean(cpoe)) %>%
  arrange(plays %>% desc()) %>%
  head(32) %>%
  arrange(total_cpoe %>% desc()) %>% 
  left_join(
    sleeper_players_df %>% select(team, full_name, gsis, headshot_url),
    by = c("passer_player_id" = "gsis")
  ) %>%
  mutate(# some headshot urls are broken. They are checked here and set to a default 
    headshot_url = dplyr::if_else(
      RCurl::url.exists(as.character(headshot_url)),
      as.character(headshot_url),
      "http://static.nfl.com/static/content/public/image/fantasy/transparent/200x200/default.png",
    )
  ) %>%
  left_join(cpoe, by = "passer_player_id") %>%
  left_join(
    teams_colors_logos %>% select(team_abbr, team_color, team_logo_espn),
    by = c("team" = "team_abbr")
  )

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
  select(full_name, passer_player_id, headshot_url, team_logo_espn) %>% 
  unique()

# create the plot. Set asp to make sure the images appear in the correct aspect ratio
p <-
  summary_df %>%
  ggplot(aes(x = air_yards, y = cpoe)) +
  geom_smooth(
    data = mean, aes(x = air_yards, y = league, weight = league_count), n = n_eval,
    color = "red", alpha = 0.7, se = FALSE, size = 0.5, linetype = "dashed"
  ) +
  geom_smooth(
    se = FALSE, alpha = 0.7, aes(color = team, weight = count), size = 0.65, n = n_eval
  ) +
  scale_color_manual(values =  NFL_pri,
                     name = "Team") +
  geom_point(aes(color = team), size = summary_df$count / 15, alpha = 0.3) +
  scale_fill_manual(values =  NFL_pri,
                    name = "Team") +
  ggimage::geom_image(data = summary_images_df, aes(x = 27.5, y = -20, image = team_logo_espn),
    size = .2, by = "width", asp = asp
  ) +
  ggimage::geom_image(data = summary_images_df, aes(x = -2.5, y = -20, image = headshot_url),
    size = .2, by = "width", asp = asp
  ) +
  xlim(-10, 40) + # makes sure the smoothing algorithm is evaluated between -10 and 40
  coord_cartesian(xlim = c(-5, 30), ylim = c(-25, 25)) + # 'zoom in'
  labs(
    x = "Target Depth In Yards Thrown Beyond The Line Of Scrimmage (DOT)",
    y = "Completion Percentage Over Expectation\n(CPOE in percentage points)",
    title = glue::glue("Passing Efficiency {season}"),
    subtitle = "CPOE as a function of target depth. Dotsize equivalent to number of targets. Smoothed for -10 ≤ DOT ≤ 40 Yards.\nRed Line = League Average."
  ) +
  facet_wrap(vars(full_name), ncol = 8, scales = "free") +
  theme_cw +
  theme(
    axis.title = element_text(size = 8),
    axis.text = element_text(size = 6),
    axis.title.y = element_text(angle = 90),
    plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 6),
    # panel.margin.y = ,
    legend.position = "none",
    legend.title = element_blank(),
    legend.text = element_blank(),
    strip.text = element_text(size = 4, hjust = 0.5, face = "bold")
  )

# save the plot
brand_plot(p, asp = 16/10, save_name = glue('plots/desktop/cpoe_vs_dot_{season}.png'), data_home = 'Data: @nflfastR', fade_borders = '')
