library(tidyverse)
library(espnscrapeR)
library(nflfastR)
library(teamcolors)
library(gt)
library(webshot)


# Data --------------------------------------------------------------------

current_season <- year

con <- fx.db_con(x.host = 'localhost')
pbp_df <- 
#   purrr::map_df(current_season, function(x) {
#   readRDS(url(
#     glue::glue("https://github.com/guga31bb/nflfastR-data/blob/master/data/play_by_play_{x}.rds?raw=true")
#   ))
#   # }) %>% filter(week < 9)
# }) %>% 
  tbl(con, 'nflfastR_pbp') %>% 
  filter(season == current_season & 
           season_type == 'REG' & 
           !is.na(posteam) & 
           (rush == 1 | pass == 1)) %>% 
  select(-xyac_median_yardage) %>%
  collect() %>% 
  decode_player_ids
print(current_season)

n_week <- fx.n_week(pbp_df)

# Calculate avg epa
epa_data <- pbp_df %>%
  filter(posteam != '') %>% 
  # dplyr::filter(!is.na(epa), !is.na(ep), !is.na(posteam), play_type == "pass" | play_type == "run") %>%
  dplyr::filter(!is.na(posteam)) %>%
  dplyr::group_by(game_id, season, week, posteam, home_team) %>%
  dplyr::summarise(
    off_epa = mean(epa, na.rm = T),
  ) %>%
  dplyr::left_join(pbp_df %>%
                     filter(play_type == "pass" | play_type == "run") %>%
                     dplyr::group_by(game_id, season, week, defteam, away_team) %>%
                     dplyr::summarise(def_epa = mean(epa, na.rm = T)),
                   by = c("game_id", "posteam" = "defteam", "season", "week"),
                   all.x = T
  ) %>%
  dplyr::mutate(opponent = ifelse(posteam == home_team, away_team, home_team)) %>%
  dplyr::select(game_id, season, week, home_team, away_team, posteam, opponent, off_epa, def_epa)

offense <- pbp_df %>%
  filter(!is.na(epa) & !is.na(posteam)) %>% 
  group_by(posteam, season) %>%
  summarize(
    n_pass=sum(pass, na.rm = T),
    n_rush=sum(rush, na.rm = T),
    epa_per_pass=sum(epa*pass, na.rm = T)/n_pass,
    epa_per_rush=sum(epa*rush, na.rm = T)/n_rush,
    success_per_pass=sum(pass*epa>0, na.rm = T)/n_pass,
    success_per_rush=sum(rush*epa>0, na.rm = T)/n_rush,
    off_epa=mean(epa, na.rm = T),
    off_success=mean(success, na.rm = T)
  )

# Get ESPN Win Rates
all_win_rate <- scrape_espn_win_rate(season = current_season)

wide_win_rate <- all_win_rate %>%
  pivot_wider(names_from = stat,
              values_from = win_pct,
              id_col = team) %>%
  purrr::set_names(nm = c('team', 'prwr', 'rswr', 'pbwr', 'rbwr')) %>%
  mutate(prwr_rk = min_rank(desc(prwr)), .before = prwr) %>%
  mutate(rswr_rk = min_rank(desc(rswr)), .before = rswr) %>%
  mutate(pbwr_rk = min_rank(desc(pbwr)), .before = pbwr) %>%
  mutate(rbwr_rk = min_rank(desc(rbwr)), .before = rbwr) %>%
  mutate(def_wr_comb_rk = (prwr_rk + rswr_rk) / 2) %>%
  mutate(off_wr_comb_rk = (rbwr_rk + pbwr_rk) / 2) %>%
  mutate(total_wr_comb_rk = (prwr_rk + rswr_rk + rbwr_rk + pbwr_rk) / 4) %>% 
  left_join(teams_colors_logos %>% filter(team_abbr != 'LAR') %>%
              select(team_abbr, team_name),
            by = c('team' = 'team_name')) %>% 
  select(team, team_abbr, prwr_rk, prwr, rswr_rk, rswr, def_wr_comb_rk, pbwr_rk, pbwr, rbwr_rk, rbwr, off_wr_comb_rk, total_wr_comb_rk)

grob_img_adj <- function(img_url, alpha = 1, whitewash = 0) {
  return(lapply(img_url, function(x) {
    if (is.na(x)) {
      return(NULL)
    } else {
      img <- magick::image_read(x)[[1]]
      img[1, , ] <- as.raw(255 - (255 - as.integer(img[1, , ])) * (1 - whitewash))
      img[2, , ] <- as.raw(255 - (255 - as.integer(img[2, , ])) * (1 - whitewash))
      img[3, , ] <- as.raw(255 - (255 - as.integer(img[3, , ])) * (1 - whitewash))
      img[4, , ] <- as.raw(as.integer(img[4, , ]) * alpha)
      return(grid::rasterGrob(image = magick::image_read(img)))
    }
  }))
}

# Team Rush EPA compared to Run Block Win Rate ----------------------------

p <- wide_win_rate %>% 
  select(team_abbr, rbwr) %>% 
  left_join(offense %>% 
              select(posteam, epa_per_rush), 
            by = c('team_abbr' = 'posteam')) %>% 
  mutate(team_logo_espn = glue('https://a.espncdn.com/i/teamlogos/nfl/500/{team_abbr}.png')) %>% 
  arrange(-rbwr) %>% 
  ggplot(aes(x = rbwr, y = epa_per_rush)) +
  geom_hline(yintercept = mean(offense$epa_per_rush), color = "red", linetype = "dashed") +
  geom_vline(xintercept =  mean(wide_win_rate$rbwr), color = "red", linetype = "dashed") +
  geom_smooth(method = 'lm', se = FALSE) + 
  geom_grob(aes(
    x = rbwr,
    y = epa_per_rush,
    label = grob_img_adj(team_logo_espn, alpha = 0.8),
    vp.height = 0.075
  )) +
  # geom_image(aes(image = team_logo_espn), size = 0.05, asp = 4/3) +
  labs(x = "Run Block Win Rate %",
       y = "EPA per Rush",
       # caption = "Data: @nflscrapR",
       title = glue("{year} NFL Team Rushing Efficiency"),
       subtitle = glue("How successful are teams at rushing  as compared to their O-Line's success")) +
  theme_cw_dark + 
  theme(
    axis.title.y = element_text(angle = 90),
    legend.position = c(0.99, 0.99),
    legend.justification = c(1, 1) ,
    plot.title = element_text(size = 16),
    #panel.grid.minor = element_blank()
  )

brand_plot(p, asp = 16/10, save_name = glue('plots/desktop/team_rushing/team_rush_epa_rbwr_{current_season}.png'), data_home = 'Data: @nflfastR & ESPN', fade_borders = '')


# RB EPA to RBWR ----------------------------------------------------------

# compute cpoe grouped by air_yards
rb_epa <-
  pbp_df %>%
  filter(!is.na(epa) & !is.na(rusher_player_id)) %>%
  group_by(rusher_player_id) %>%
  summarise(total_yards = sum(yards_gained), epa = mean(epa))

top_32 <- 
  pbp_df %>%
  filter(!is.na(epa) & !is.na(rusher_player_id)
  ) %>%
  group_by(rusher_player_id) %>%
  summarise(posteam = posteam %>% first(),
            rush_attempts = n(),
            total_epa = mean(epa)
  ) %>%
  left_join(
    sleeper_players_df %>% 
      select(gsis_id, position),
    by = c('rusher_player_id' = 'gsis_id')
    ) %>%
  filter(position == 'RB') %>% 
  arrange(rush_attempts %>% desc()
  ) %>% 
  group_by(posteam) %>% 
  filter(row_number() == 1) %>% 
  # head(32) %>% 
  pull(rusher_player_id)

# summarise cpoe using player ID (note that player ids are 'NA' for 'no_play' plays. 
# Since we would filter those plays anyways we can use the id here)
# The correct name is being joined using the roster data
# first arranged by number of plays to filter the 30 QBs with most pass attempts
# The filter is set to 30 because we want to have 6 columns and 5 rows in the facet
summary_df <-
  pbp_df %>%
  filter(!is.na(epa) & !is.na(rusher_player_id)
  ) %>% 
  group_by(rusher_player_id) %>%
  summarise(rush_attempts = n(),
            total_epa = mean(epa)
  ) %>%
  left_join(pbp_df %>% 
              filter(!is.na(rusher_player_id)
              ) %>% 
              select(rusher_player_id, 
                     team = posteam
              ) %>% 
              unique(),
            by = c('rusher_player_id')
  ) %>% 
  group_by(rusher_player_id) %>% 
  mutate(season_rush_attempts = sum(rush_attempts, na.rm = T)) %>% 
  ungroup %>% 
  arrange(season_rush_attempts %>% 
            desc()
  ) %>% 
  filter(rusher_player_id %in% top_32) %>%
  left_join(
    sleeper_players_df %>%
      select(position, full_name, sportradar_id, gsis_id, espn_id, headshot_url),
    by = c('rusher_player_id' = 'gsis_id')
  ) %>%
  # left_join(
  #   as_tibble(roster_df),
  #   by = c('passer_player_id' = 'gsis' , 'team')
  # ) %>%
  mutate(# some headshot urls are broken. They are checked here and set to a default 
    headshot_url = dplyr::if_else(RCurl::url.exists(as.character(headshot_url)), as.character(headshot_url), 'http://static.nfl.com/static/content/public/image/fantasy/transparent/200x200/default.png')
  ) %>% 
  left_join(rb_epa, by = c('rusher_player_id')) %>% 
  arrange(total_epa %>% desc()) %>% 
  left_join(
    teams_colors_logos %>% select(team_abbr, team_color, team_color2, team_logo_espn),
    by = c('team' = 'team_abbr')
  )

# create data frame used to add the logos
# arranged by name because name is used for the facet
colors_raw <-
  summary_df %>%
  group_by(rusher_player_id) %>%
  summarise(team = first(team), name = first(full_name)) %>%
  left_join(
    teams_colors_logos %>% select(team_abbr, team_color, team_color2),
    by = c("team" = "team_abbr")
  ) %>%
  arrange(name)

# the below used smooth algorithm uses the parameter n as the number
# of points at which to evaluate the smoother. When using color as aesthetics
# we need exactly the same number of colors (-> n times the same color per player)

# mean data frame for the smoothed line of the whole league
mean <-
  summary_df %>%
  summarise(league_epa = mean(total_epa)
  )

summary_images_df <- 
  summary_df %>% 
  select(full_name, rusher_player_id, total_epa, season_rush_attempts, headshot_url, team_logo_espn) %>% 
  unique() %>% 
  arrange(total_epa %>% desc()) %>% 
  head(32)

# RB rushing + RBWR
p_data <- wide_win_rate %>% 
  select(team_abbr, rbwr) %>% 
  left_join(summary_df %>% 
              select(full_name, team, total_epa) %>% unique(), 
            by = c('team_abbr' = 'team')) %>% 
  left_join(colors_raw %>% select(team, team_color, team_color2),
            by = c('team_abbr' = 'team')) %>% 
  mutate(team_logo_espn = glue('https://a.espncdn.com/i/teamlogos/nfl/500/{team_abbr}.png')) %>% 
  arrange(-rbwr)

p <- p_data %>% 
  ggplot(aes(x = rbwr, y = total_epa)) +
  # geom_hline(yintercept = mean(summary_df$season_dakota), color = "red", linetype = "dashed") +
  # geom_vline(xintercept =  mean(wide_win_rate$pbwr), color = "red", linetype = "dashed") +
  geom_smooth(method='lm', se=FALSE) + 
  geom_text_repel(
    aes(label = full_name),
    # segment.color = p_data %>%
    #   arrange(-pbwr) %>%
    #   pull(team_color),
    min.segment.length = .5,
    family = 'Montserrat',
    color = color_cw[5],
    size = 2.5,
    nudge_y = -.007
  ) +
  geom_point(
    color = p_data %>%
      arrange(-rbwr) %>%
      pull(team_color2),
    fill = p_data %>% 
      arrange(-rbwr) %>% 
      pull(team_color),
    shape = 21,
    size = 3
  ) +
  labs(x = "Run Block Win Rate %",
       y = "Total EPA",
       # caption = "Data: @nflscrapR",
       title = glue("{year} NFL Team Rushing Efficiency"),
       subtitle = glue("How successful are RBs at rushing as compared to their O-Line's success")) +
  theme_cw_dark + 
  theme(
    axis.title.y = element_text(angle = 90),
    plot.title = element_text(size = 16)
    #panel.grid.minor = element_blank()
  )

brand_plot(p, save_name = glue('plots/desktop/team_rushing/rb_rush_epa_rbwr_{current_season}.png'), asp = 16/10, data_home = 'Data: @nflfastR & ESPN', fade_borders = '')

# RB rush attempts + Total EPA
p_data <- wide_win_rate %>% 
  select(team_abbr, rbwr) %>% 
  left_join(summary_df %>% 
              select(full_name, team, total_epa, season_rush_attempts), 
            by = c('team_abbr' = 'team')) %>% 
  left_join(colors_raw %>% select(team, team_color, team_color2),
            by = c('team_abbr' = 'team')) %>% 
  mutate(team_logo_espn = glue('https://a.espncdn.com/i/teamlogos/nfl/500/{team_abbr}.png')) %>% 
  arrange(-rbwr)

p <- p_data %>% 
  ggplot(aes(x = season_rush_attempts, y = total_epa)) +
  # geom_hline(yintercept = mean(summary_df$season_dakota), color = "red", linetype = "dashed") +
  # geom_vline(xintercept =  mean(wide_win_rate$pbwr), color = "red", linetype = "dashed") +
  geom_smooth(method='lm', se=FALSE) + 
  geom_text_repel(
    aes(label = full_name),
    # segment.color = p_data %>%
    #   arrange(-pbwr) %>%
    #   pull(team_color),
    min.segment.length = .5,
    family = 'Montserrat',
    color = color_cw[5],
    size = 2.5,
    nudge_y = -.007
  ) +
  geom_point(
    color = p_data %>%
      arrange(-rbwr) %>%
      pull(team_color2),
    fill = p_data %>% 
      arrange(-rbwr) %>% 
      pull(team_color),
    shape = 21,
    size = 3
  ) +
  labs(x = "Total Rush Attempts",
       y = "Total EPA",
       # caption = "Data: @nflscrapR",
       title = glue("{year} NFL Team Rushing Efficiency"),
       subtitle = glue("How successful are RBs at rushing as compared to their O-Line's success")) +
  theme_cw_dark + 
  theme(
    axis.title.y = element_text(angle = 90),
    plot.title = element_text(size = 16)
    #panel.grid.minor = element_blank()
  )

brand_plot(p, save_name = glue('plots/desktop/team_rushing/rb_rush_attempts_epa_{current_season}.png'), asp = 16/10, data_home = 'Data: @nflfastR & ESPN', fade_borders = '')


# Team Pass EPA to Pass Block Win Rate ------------------------------------

p <- wide_win_rate %>% 
  select(team_abbr, pbwr) %>% 
  left_join(offense %>% 
              select(posteam, epa_per_pass), 
            by = c('team_abbr' = 'posteam')) %>% 
  mutate(team_logo_espn = glue('https://a.espncdn.com/i/teamlogos/nfl/500/{team_abbr}.png')) %>% 
  arrange(-pbwr) %>% 
  ggplot(aes(x = pbwr, y = epa_per_pass)) +
  geom_hline(yintercept = mean(offense$epa_per_pass), color = "red", linetype = "dashed") +
  geom_vline(xintercept =  mean(wide_win_rate$pbwr), color = "red", linetype = "dashed") +
  geom_smooth(method='lm', se=FALSE) + 
  geom_grob(aes(
    x = pbwr,
    y = epa_per_pass,
    label = grob_img_adj(team_logo_espn, alpha = 0.8),
    vp.height = 0.075
  )) +
  # geom_image(aes(image = team_logo_espn), size = 0.05, asp = 4/3) +
  labs(x = "Pass Block Win Rate %",
       y = "EPA per Pass",
       # caption = "Data: @nflscrapR",
       title = glue("{year} NFL Team Passing Efficiency"),
       subtitle = glue("How successful are teams at passing as compared to their O-Line's success")) +
  theme_cw_dark + 
  theme(
    axis.title.y = element_text(angle = 90),
    legend.position = c(0.99, 0.99),
    legend.justification = c(1, 1) ,
    plot.title = element_text(size = 16),
    #panel.grid.minor = element_blank()
  )

brand_plot(p, save_name = glue('plots/desktop/team_passing/team_pass_epa_rbwr_{current_season}.png'), asp = 16/10, data_home = 'Data: @nflfastR & ESPN', fade_borders = '')

# Passer to Pass Block Win Rate -------------------------------------------

load(url('https://github.com/guga31bb/metrics/blob/master/dakota_model.rda?raw=true'))

# compute cpoe grouped by air_yards
epa_cpoe <-
  pbp_df %>%
  filter(!is.na(cpoe) & !is.na(epa) & !is.na(passer_player_id)) %>%
  group_by(passer_player_id) %>%
  summarise(cpoe = mean(cpoe), epa = mean(epa)) %>% 
  left_join(pbp_df %>%
              filter(!is.na(cpoe) &
                       !is.na(epa) & !is.na(passer_player_id)) %>%
              group_by(passer_player_id) %>%
              summarise(cpoe = mean(cpoe), epa_per_play = mean(epa)) %>%
              mutate(season_dakota = mgcv::predict.gam(dakota_model, .)) %>%
              select(-cpoe, -epa_per_play),
            by = c('passer_player_id')
  )

top_32 <- 
  pbp_df %>%
  filter(!is.na(cpoe) & !is.na(epa) & !is.na(passer_player_id)
  ) %>%
  group_by(passer_player_id) %>%
  summarise(posteam = posteam %>% first(),
            pa = n(),
            total_cpoe = mean(cpoe),
            total_epa = mean(epa)
  ) %>%
  left_join(
    sleeper_players_df %>% 
      select(gsis_id, position),
    by = c('passer_player_id' = 'gsis_id')
  ) %>% 
  arrange(pa %>% desc()
  ) %>% 
  group_by(posteam) %>% 
  filter(row_number() == 1) %>% # Filter passer with the most PA on each team.
  # head(32) %>% 
  pull(passer_player_id)

# summarize cpoe using player ID (note that player ids are 'NA' for 'no_play' plays. 
# Since we would filter those plays anyways we can use the id here)
# The correct name is being joined using the roster data
# first arranged by number of plays to filter the 30 QBs with most pass attempts
# The filter is set to 30 because we want to have 6 columns and 5 rows in the facet
summary_df <-
  pbp_df %>%
  filter(!is.na(cpoe) & !is.na(epa) & !is.na(passer_player_id)
  ) %>% 
  group_by(passer_player_id) %>%
  summarise(pa = n(),
            total_cpoe = mean(cpoe, na.rm = T),
            total_epa = sum(epa, na.rm = T)
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
  group_by(passer_player_id) %>% 
  mutate(season_pa = sum(pa, na.rm = T)) %>% 
  ungroup %>% 
  arrange(season_pa %>% 
            desc()
  ) %>% 
  filter(passer_player_id %in% top_32) %>%
  left_join(
    sleeper_players_df %>%
      select(position, full_name, sportradar_id, gsis_id, espn_id, headshot_url),
    by = c('passer_player_id' = 'gsis_id')
  ) %>%
  # left_join(
  #   as_tibble(roster_df),
  #   by = c('passer_player_id' = 'gsis' , 'team')
  # ) %>%
  mutate(# some headshot urls are broken. They are checked here and set to a default 
    headshot_url = dplyr::if_else(RCurl::url.exists(as.character(headshot_url)), as.character(headshot_url), 'http://static.nfl.com/static/content/public/image/fantasy/transparent/200x200/default.png')
  ) %>% 
  left_join(epa_cpoe, by = c('passer_player_id')) %>% 
  arrange(season_dakota %>% desc()) %>% 
  mutate(lab_dakota = glue('DAKOTA: {season_dakota %>% round(3)}')) %>% 
  left_join(
    teams_colors_logos %>% select(team_abbr, team_color, team_logo_espn),
    by = c('team' = 'team_abbr')
  )

# create data frame used to add the logos
# arranged by name because name is used for the facet
colors_raw <-
  summary_df %>%
  group_by(passer_player_id) %>%
  summarise(team = first(team), name = first(full_name)) %>%
  left_join(
    teams_colors_logos %>% select(team_abbr, team_color, team_color2),
    by = c("team" = "team_abbr")
  ) %>%
  arrange(name)

# the below used smooth algorithm uses the parameter n as the number
# of points at which to evaluate the smoother. When using color as aesthetics
# we need exactly the same number of colors (-> n times the same color per player)
n_eval <- 80
colors <-
  as.data.frame(
    lapply(colors_raw, rep, n_eval)
    ) %>%
  arrange(name)

# mean data frame for the smoothed line of the whole league
mean <-
  summary_df %>%
  summarise(
    league_cpoe = mean(total_cpoe), 
    league_epa = mean(total_epa)
  )

summary_images_df <- 
  summary_df %>% 
  select(full_name, passer_player_id, season_dakota, lab_dakota, headshot_url, team_logo_espn, season_pa) %>% 
  unique() %>% 
  arrange(season_dakota %>% desc()) %>% 
  head(32)

# DAKOTA + PBWR -----------------------------------------------------------
p_data <- wide_win_rate %>%
  select(team_abbr, pbwr) %>%
  left_join(
    summary_df %>%
      select(full_name, team, season_dakota) %>% unique(),
    by = c('team_abbr' = 'team')
  ) %>%
  left_join(colors_raw %>% select(team, team_color, team_color2),
            by = c('team_abbr' = 'team')) %>%
  mutate(team_logo_espn = glue(
    'https://a.espncdn.com/i/teamlogos/nfl/500/{team_abbr}.png'
  )) %>%
  arrange(-pbwr)

# p_data$pri_dark <- NFL_pri_dark[match(p_data$team_abbr, names(NFL_pri_dark))]

p <- p_data %>%
  ggplot(aes(x = pbwr, y = season_dakota)) +
  # geom_hline(yintercept = mean(summary_df$season_dakota), color = "red", linetype = "dashed") +
  # geom_vline(xintercept =  mean(wide_win_rate$pbwr), color = "red", linetype = "dashed") +
  geom_smooth(method = 'lm', se = FALSE) +
  geom_text_repel(
    aes(label = full_name),
    # segment.color = p_data %>%
    #   arrange(-pbwr) %>%
    #   pull(team_color),
    min.segment.length = .5,
    family = 'Montserrat',
    color = color_cw[5],
    size = 2.5,
    nudge_y = -.007
  ) +
  geom_point(
    color = p_data %>%
      arrange(-pbwr) %>%
      pull(team_color2),
    fill = p_data %>% 
      arrange(-pbwr) %>% 
      pull(team_color),
    shape = 21,
    size = 3
  ) +
  labs(
    x = "Pass Block Win Rate %",
    y = "DAKOTA",
    # caption = "Data: @nflscrapR",
    title = glue("{year} NFL Team Passing Efficiency"),
    subtitle = glue(
      "How successful are QBs at passing as compared to their O-Line's success?\n
                       DAKOTA compared to OL Pass Block Win Rate"
    )
  ) +
  theme_cw_dark +
  theme(axis.title.y = element_text(angle = 90),
        plot.title = element_text(size = 16)
        # panel.background = element_rect(fill = color_cw[3])
        )

brand_plot(p, save_name = glue('plots/desktop/qb_passing/pb_pass_dakota_pbwr_{current_season}.png'), asp = 16/10, data_home = 'Data: @nflfastR & ESPN', fade_borders = '')

# CPOE + PBWR -------------------------------------------------------------
p_data <- wide_win_rate %>%
  select(team_abbr, pbwr) %>%
  left_join(
    summary_df %>%
      select(full_name, team, total_cpoe) %>% unique(),
    by = c('team_abbr' = 'team')
  ) %>%
  left_join(colors_raw %>% select(team, team_color, team_color2),
            by = c('team_abbr' = 'team')) %>%
  mutate(team_logo_espn = glue(
    'https://a.espncdn.com/i/teamlogos/nfl/500/{team_abbr}.png'
  )) %>%
  arrange(-pbwr)

p <- p_data %>%
  ggplot(aes(x = pbwr, y = total_cpoe)) +
  # geom_hline(yintercept = mean(summary_df$season_dakota), color = "red", linetype = "dashed") +
  # geom_vline(xintercept =  mean(wide_win_rate$pbwr), color = "red", linetype = "dashed") +
  geom_smooth(method = 'lm', se = FALSE) +
  geom_text_repel(
    aes(label = full_name),
    # segment.color = p_data %>%
    #   arrange(-pbwr) %>%
    #   pull(team_color),
    min.segment.length = .5,
    family = 'Montserrat',
    color = color_cw[5],
    size = 2.5,
    nudge_y = -.007
  ) +
  geom_point(
    color = p_data %>%
      arrange(-pbwr) %>%
      pull(team_color2),
    fill = p_data %>% 
      arrange(-pbwr) %>% 
      pull(team_color),
    shape = 21,
    size = 3
  ) +
  labs(
    x = "Pass Block Win Rate %",
    y = "Total CPOE",
    # caption = "Data: @nflscrapR",
    title = glue("{year} NFL Team Passing Efficiency"),
    subtitle = glue(
      "How successful are QBs at passing as compared to their O-Line's success?\n
                       CPOE compared to OL Pass Block Win Rate"
    )
  ) +
  theme_cw_dark +
  theme(axis.title.y = element_text(angle = 90),
        plot.title = element_text(size = 16))

brand_plot(p, save_name = glue('plots/desktop/qb_passing/pb_pass_cpoe_pbwr_{current_season}.png'), asp = 16/10, data_home = 'Data: @nflfastR & ESPN', fade_borders = '')

# CPOE + PBWR -------------------------------------------------------------
p_data <- wide_win_rate %>%
  select(team_abbr, pbwr) %>%
  left_join(
    summary_df %>%
      select(full_name, team, total_epa) %>% unique(),
    by = c('team_abbr' = 'team')
  ) %>%
  left_join(colors_raw %>% select(team, team_color, team_color2),
            by = c('team_abbr' = 'team')) %>%
  mutate(team_logo_espn = glue(
    'https://a.espncdn.com/i/teamlogos/nfl/500/{team_abbr}.png'
  )) %>%
  arrange(-pbwr) 

p <- p_data %>%
  ggplot(aes(x = pbwr,
             y = total_epa)) +
  # geom_hline(yintercept = mean(summary_df$season_dakota), color = "red", linetype = "dashed") +
  # geom_vline(xintercept =  mean(wide_win_rate$pbwr), color = "red", linetype = "dashed") +
  geom_smooth(method = 'lm', se = FALSE) +
  geom_text_repel(
    aes(label = full_name),
    # segment.color = p_data %>%
    #   arrange(-pbwr) %>%
    #   pull(team_color),
    min.segment.length = .5,
    family = 'Montserrat',
    color = color_cw[5],
    size = 2.5,
    nudge_y = -.007
  ) +
  geom_point(
    color = p_data %>%
      arrange(-pbwr) %>%
      pull(team_color2),
    fill = p_data %>% 
      arrange(-pbwr) %>% 
      pull(team_color),
    shape = 21,
    size = 3
  ) +
  labs(
    x = "Pass Block Win Rate %",
    y = "Total EPA",
    # caption = "Data: @nflscrapR",
    title = glue("{year} NFL Team Passing Efficiency"),
    subtitle = glue(
      "How successful are QBs at passing as compared to their O-Line's success?\n
                       Total EPA compared to OL Pass Block Win Rate"
    )
  ) +
  theme_cw_dark +
  theme(axis.title.y = element_text(angle = 90),
        plot.title = element_text(size = 16))

brand_plot(p, save_name = glue('plots/desktop/qb_passing/pb_pass_total_epa_pbwr_{current_season}.png'), asp = 16/10, data_home = 'Data: @nflfastR & ESPN', fade_borders = '')
