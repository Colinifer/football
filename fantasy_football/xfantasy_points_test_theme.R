library(tidyverse)
library(viridis)

source('https://github.com/mrcaseb/nflfastR/blob/master/R/utils.R?raw=true')
source('https://github.com/mrcaseb/nflfastR/raw/master/R/helper_add_xyac.R')
source('https://github.com/mrcaseb/nflfastR/raw/master/R/helper_add_nflscrapr_mutations.R')
source('fantasy_football/xyac/add_xyac_old.R')


# YAC Distribution Function -----------------------------------------------

# duplicate the add_xyac() function that we sourced above
add_xyac_dist <- add_xyac


# separate each block of code in the add_xyac_dist() function into blocks
add_xyac_blocks <- body(add_xyac_dist) %>% as.list

# we want to remove lines 51 to 62 from the 5th item in the list
add_xyac_blocks[[2]] <- add_xyac_blocks[[2]] %>% 
  format %>% 
  .[-(61:72)] %>% 
  paste(collapse = '\n') %>% 
  str2lang

# replace the body of add_xyac_dist() with our new edited function
body(add_xyac_dist) <- add_xyac_blocks %>% as.call


# Data --------------------------------------------------------------------

# pbp_df <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2020.rds'))
pbp_df <- readRDS(url('https://github.com/guga31bb/nflfastR-data/blob/master/data/play_by_play_2020.rds?raw=true'))


# Plot --------------------------------------------------------------------

source('plots/assets/plot_theme.R')

my_week <- 5


quick_rost <- readRDS(url('https://github.com/guga31bb/nflfastR-raw/blob/master/roster/roster.rds?raw=true')) 


fant_pt_dist_df <- pbp_df %>% 
  filter(pass_attempt==1 & season_type=='REG' & two_point_attempt==0 & !is.na(receiver_id) & !is.na(cp) & ((receiver_jersey_number < 90 & receiver_jersey_number > 79) | (receiver_jersey_number < 20 & receiver_jersey_number > 9))) %>% 
  add_xyac_dist %>% 
  select(season = season.x, game_id, play_id, posteam = posteam.x, receiver, receiver_id, yardline_100 = yardline_100.x, air_yards = air_yards.x, actual_yards_gained = yards_gained, complete_pass, cp, yac_prob = prob, gain) %>% 
  mutate(
    gain = ifelse(yardline_100==air_yards, yardline_100, gain),
    yac_prob = ifelse(yardline_100==air_yards, 1, yac_prob),
    PPR_points = 1 + gain/10 + ifelse(gain == yardline_100, 6, 0),
    half_PPR_points = .5 + gain/10 + ifelse(gain == yardline_100, 6, 0),
    catch_run_prob = cp * yac_prob,
    exp_PPR_points = PPR_points * catch_run_prob,
    exp_half_PPR_points = half_PPR_points * catch_run_prob,
    actual_outcome = ifelse(actual_yards_gained==gain & complete_pass==1, 1, 0),
    actual_PPR_points = ifelse(actual_outcome==1, PPR_points, 0),
    actual_half_PPR_points = ifelse(actual_outcome==1, half_PPR_points, 0),
    target = 0,
    game_played = 0
  )


fant_pt_dist_df <- nflfastR::decode_player_ids(fant_pt_dist_df)


incomplete_df <- fant_pt_dist_df %>% 
  mutate(
    gain = 0,
    PPR_points = 0,
    half_PPR_points = 0,
    yac_prob = 0,
    exp_PPR_points = 0,
    exp_half_PPR_points = 0,
    complete_pass = 0,
    catch_run_prob = 1 - cp,
    actual_outcome = NA,
    actual_PPR_points = NA,
    actual_half_PPR_points = NA,
    target = 1
  ) %>% 
  distinct %>% 
  group_by(game_id, receiver) %>% 
  mutate(game_played = ifelse(row_number()==1,1,0)) %>% 
  ungroup

if (exists("sleep.players") == FALSE) {
  sleeper_api_players_url <-
    'https://api.sleeper.app/v1/players/nfl'
  sleeper_api_players <-
    jsonlite::fromJSON(url('https://api.sleeper.app/v1/players/nfl'), flatten = T)
  na_map <-
    readRDS(
      url(
        "https://github.com/mrcaseb/nflfastR-roster/blob/master/R/na_map.rds?raw=true"
      )
    )
  
  # source("https://github.com/mrcaseb/nflfastR-roster/blob/master/R/update_roster.R")
  sleep.players <-
    purrr::map_dfr(sleeper_api_players, function(x)
      purrr::map(x, function(y)
        ifelse(is.null(y), NA, y))) %>%
    dplyr::na_if("") %>%
    dplyr::mutate_if(is.character, stringr::str_trim) %>%
    dplyr::filter(
      !(is.na(team) &
          is.na(gsis_id)),
      !player_id %in% nflfastR::teams_colors_logos$team_abbr,
      first_name != "Duplicate"
    ) %>%
    dplyr::left_join(na_map, by = c("sportradar_id" = "id")) %>%
    dplyr::mutate(
      gsis_id = dplyr::if_else(is.na(gsis_id), gsis, gsis_id),
      update_dt = lubridate::now("America/New_York"),
      season = dplyr::if_else(
        lubridate::month(update_dt) < 3,
        lubridate::year(update_dt) - 1,
        lubridate::year(update_dt)
      ),
      index = 1:dplyr::n(),
      headshot_url = dplyr::if_else(is.na(espn_id), NA_character_, as.character(
        glue::glue(
          "https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/{espn_id}.png"
        )
      ))
    )
}
if (exists("espn.players") == FALSE) {
  espn.league_id <- fantasy_key$league_id[1]
  files_list <- list.files("fantasy_football/data/free_agents/")
  
  espn.players <- readRDS(paste0(
    "fantasy_football/data/free_agents/",
    list.files("fantasy_football/data/free_agents/")[grepl(paste0(espn.league_id, "players_", Sys.Date(), ".rds"),
                                                           files_list)]
  ))
}

WR_rank_df <- rbind(incomplete_df, fant_pt_dist_df) %>%
  group_by(posteam, receiver) %>%
  summarize(
    receiver_id = unique(receiver_id),
    tot_PPR = sum(actual_PPR_points, na.rm = T),
    tot_half_PPR = sum(actual_half_PPR_points, na.rm = T),
    tot_targ = sum(target),
    tot_gp = sum(game_played),
    PPR_pg = tot_PPR / tot_gp,
    half_PPR_pg = tot_half_PPR / tot_gp
  ) %>%
  # ungroup %>%
  arrange(-tot_targ) %>%
  #slice(1:25)
  left_join(
    sleep.players %>%
      select(position, sportradar_id, gsis_id, espn_id, headshot_url),
    by = c("receiver_id" = "gsis_id")
  ) %>%
  left_join(espn.players %>%
              select(id, status, onTeamId),
            by = c("espn_id" = "id")) %>%
  # filter(status != "ONTEAM") %>% #Use to search through FA's
  mutate(tm_rnk = row_number()) %>%
  filter(tm_rnk <= 4)

# make a data frame to loop around
sampling_df <- rbind(incomplete_df, fant_pt_dist_df) %>% 
  right_join(WR_rank_df %>% select(posteam, receiver)) %>% 
  select(season, game_id, play_id, posteam, receiver, catch_run_prob, half_PPR_points) %>% 
  group_by(game_id, play_id)


if (file.exists("fantasy_football/data/sample_sim_df.rds") == FALSE) {
  # do sim
  sim_df <- do.call(rbind, lapply(1:5000, function(x) {
    sampling_df %>% 
      mutate(sim_res = sample(half_PPR_points, 1, prob = catch_run_prob)) %>% 
      select(season, game_id, play_id, posteam, receiver, sim_res) %>% 
      distinct %>% 
      group_by(posteam, receiver) %>% 
      summarize(sim_tot = sum(sim_res, na.rm = T), .groups = 'drop') %>% 
      return
  }))
  saveRDS(sim_df, file = 'fantasy_football/data/sample_sim_df.rds')
}
sim_df <- readRDS('fantasy_football/data/sample_sim_df.rds')


sim_df <- sim_df %>% mutate(sim = 1)


# calculate how many points were actually scored
actual_df <- fant_pt_dist_df %>%
  group_by(posteam, receiver) %>% 
  summarize(sim_tot = sum(actual_half_PPR_points, na.rm = T), .groups = 'drop') %>% 
  mutate(sim = 0)


# figure out what percentile the actual values fall in
percentile_df <- rbind(sim_df, actual_df) %>% 
  group_by(posteam, receiver) %>% 
  mutate(perc = percent_rank(sim_tot)) %>% 
  filter(sim == 0) %>% 
  mutate(sim_tot = NULL, sim = NULL)

library(ggridges)


source('plots/assets/plot_theme.R')


plot_data <- sim_df %>%
  left_join(percentile_df) %>%
  left_join(WR_rank_df) %>%
  mutate(
    sim_pg = sim_tot / tot_gp,
    pl_lab = paste0(receiver, '\n', number(perc * 100, accuracy = 0.1), ' perc.'),
    posteam = factor(posteam, .tm_div_order_alt)
  ) %>%
  group_by(posteam, receiver) %>%
  mutate(
    obs_num = row_number(),
    status_color = if_else(status != "ONTEAM", color_cw[6], color_cw[5])
  ) 

p <- plot_data %>%
  ggplot(aes(
    x = sim_pg,
    y = tm_rnk,
    group = receiver,
    label = pl_lab
  )) +
  facet_wrap(. ~ posteam, nrow = 4, scales = 'free') +
  geom_point(
    aes(x = ifelse(obs_num == 1, half_PPR_pg, NA), y = tm_rnk + 0.08),
    color = color_cw[2],
    fill = color_cw[8],
    shape = 24,
    size = 0.6,
    stroke = 0.2,
    na.rm = T
  ) +
  stat_density_ridges(
    aes(fill = factor(stat(quantile))),
    geom = 'density_ridges_gradient',
    quantiles = c(.1, .25, .75, .9),
    rel_min_height = 0.001,
    bandwidth = 1,
    calc_ecdf = T,
    scale = 0.9,
    color = color_cw[6],
    size = 0.2,
    show.legend = F
  ) +
  #geom_boxplot(size = 0.4, color = 'darkblue', width = 0.6, outlier.alpha = 0, notchwidth = 1) +
  scale_color_manual(values = c(
    color_cw[6],
    color_cw[5],
    color_cw[7]
  )) +
  geom_shadowtext(
    aes(
      x = ifelse(obs_num == 1, 48, NA),
      y = tm_rnk - 0.5,
      color = factor(status)
    ),
    hjust = 1,
    bg.color = color_cw[2],
    #[family = font_family,]
    size = 1.2,
    na.rm = T,
    bg.r = 0.2
  ) +
  scale_x_continuous(
    breaks = seq(0, 80, 10),
    expand = expansion(mult = 0),
    limits = c(0, 50)
  ) +
  scale_y_reverse(expand = expansion(mult = c(0, 0.04), add = c(0.2, 0)), limits = c(4.1, 0)) +
  scale_fill_manual(values = c(
    alpha(color_cw[7], 0.5),
    alpha(color_cw[6], 0.5),
    alpha(color_cw[5], 0.5),
    alpha(color_cw[6], 0.5),
    alpha(color_cw[7], 0.5)
  )) +
  labs(
    title = paste0('2020 Expected 1/2 PPR Fantasy Points per Game as of Week ', my_week),
    subtitle = 'Grey represents middle 50% of outcomes, Orange tails are each 10% of outcomes  |  Caret shows actual avg  |  Based on 10,000 Simulations',
    x = NULL
  ) +
  theme_cw +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text = element_text(size = 4),
    axis.line = element_line(color = '#e0e0e0', size = 0.5),
    panel.border = element_rect(color = '#16191C', size = 0.1),
    axis.ticks.length = unit(0.15, 'lines'),
    axis.ticks = element_line(color = '#e0e0e0', size = 0.5),
    panel.grid.minor.x = element_line('#16191C', size = 0.2),
    panel.spacing.x = unit(0.5, 'lines')
  ) +
  annotation_custom(
    make_gradient(deg = 270),
    ymin = Inf,
    ymax = -0.1,
    xmin = -Inf,
    xmax = Inf
  ) +
  annotation_custom(
    make_gradient(deg = 0),
    ymin = -Inf,
    ymax = Inf,
    xmin = 48,
    xmax = Inf
  )

# ggplot_gtable(ggplot_build(p))

brand_plot(p, asp = 16/9, save_name = 'xfp_half_PPR_box_2020_test.png', fade_borders = '', data_home = 'Data: @nflfastR', tm_wordmarks = T)
     