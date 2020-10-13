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
summary <-
  pbp %>%
  filter(!is.na(cpoe)) %>%
  group_by(passer_player_id) %>%
  summarise(plays = n()) %>%
  arrange(desc(plays)) %>%
  head(30) %>%
  left_join(
    roster %>% filter(season == season) %>% select(name = teamPlayers.displayName, teamPlayers.gsisId, team.abbr, teamPlayers.headshot_url),
    by = c("passer_player_id" = "teamPlayers.gsisId")
  ) %>%
  mutate(# some headshot urls are broken. They are checked here and set to a default 
    teamPlayers.headshot_url = dplyr::if_else(
      RCurl::url.exists(as.character(teamPlayers.headshot_url)),
      as.character(teamPlayers.headshot_url),
      "http://static.nfl.com/static/content/public/image/fantasy/transparent/200x200/default.png",
    )
  ) %>%
  left_join(cpoe, by = "passer_player_id") %>%
  left_join(
    teams_colors_logos %>% select(team_abbr, team_color, team_logo_espn),
    by = c("team.abbr" = "team_abbr")
  )

# create data frame used to add the logos
# arranged by name because name is used for the facet
colors_raw <-
  summary %>%
  group_by(passer_player_id) %>%
  summarise(team = first(team.abbr), name = first(name)) %>%
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
  summary %>%
  group_by(air_yards) %>%
  summarise(league = mean(cpoe), league_count = n())

# create the plot. Set asp to make sure the images appear in the correct aspect ratio
asp <- 1.2
plot <-
  summary %>%
  ggplot(aes(x = air_yards, y = cpoe)) +
  geom_smooth(
    data = mean, aes(x = air_yards, y = league, weight = league_count), n = n_eval,
    color = "red", alpha = 0.7, se = FALSE, size = 0.5, linetype = "dashed"
  ) +
  geom_smooth(
    se = FALSE, alpha = 0.7, aes(color = team_color, weight = count), size = 0.65, n = n_eval
  ) +
  geom_point(aes(color = team_color), size = summary$count / 15, alpha = 0.4) +
  ggimage::geom_image(aes(x = 27.5, y = -20, image = team_logo_espn),
    size = .15, by = "width", asp = asp
  ) +
  ggimage::geom_image(aes(x = -2.5, y = -20, image = teamPlayers.headshot_url),
    size = .15, by = "width", asp = asp
  ) +
  xlim(-10, 40) + # makes sure the smoothing algorithm is evaluated between -10 and 40
  coord_cartesian(xlim = c(-5, 30), ylim = c(-25, 25)) + # 'zoom in'
  labs(
    x = "Target Depth In Yards Thrown Beyond The Line Of Scrimmage (DOT)",
    y = "Completion Percentage Over Expectation (CPOE in percentage points)",
    caption = "Data: @nflfastR",
    title = glue::glue("Passing Efficiency {season}"),
    subtitle = "CPOE as a function of target depth. Dotsize equivalent to number of targets. Smoothed for -10 ≤ DOT ≤ 40 Yards. Red Line = League Average."
  ) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 6),
    plot.title = element_text(size = 12, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    plot.caption = element_text(size = 8),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 6),
    strip.text = element_text(size = 6, hjust = 0.5, face = "bold"),
    aspect.ratio = 1 / asp
  ) +
  facet_wrap(vars(name), ncol = 6, scales = "free")

# save the plot
ggsave(glue::glue("plots/desktop/cpoe_vs_dot_{season}.png"), dpi = 600, width = 24, height = 21, units = "cm")

# season <- season + 1