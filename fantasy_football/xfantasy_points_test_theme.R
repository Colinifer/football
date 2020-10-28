
# Notes -------------------------------------------------------------------

# WOPR = 1.5 × Target Market Share + 0.7 × Air Yards Market Share
# Need to calcualte team market shares


# Start -------------------------------------------------------------------

library(tidyverse)
library(parallel)
library(viridis)

source('init.R')

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
if (exists("pbp_df") == FALSE) {
  pbp_df <- readRDS(url(glue('https://github.com/guga31bb/nflfastR-data/blob/master/data/play_by_play_{year}.rds?raw=true')))
}

# Plot Data ---------------------------------------------------------------

source('plots/assets/plot_theme.R')

my_week <- pbp_df %>% select(week) %>% max()


quick_rost <- readRDS(url('https://github.com/guga31bb/nflfastR-raw/blob/master/roster/roster.rds?raw=true')) %>% 
  decode_player_ids(fast = TRUE)


fant_pt_dist_df <- pbp_df %>% 
  # filter(pass_attempt==1 & season_type=='REG' & two_point_attempt==0 & !is.na(receiver_id) & !is.na(cp) & ((receiver_jersey_number < 90 & receiver_jersey_number > 79) | (receiver_jersey_number < 20 & receiver_jersey_number > 9))) %>% 
  filter(pass_attempt==1 & season_type=='REG' & two_point_attempt==0 & !is.na(receiver_id) & !is.na(cp)) %>% 
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


fant_pt_dist_df <- decode_player_ids(fant_pt_dist_df, fast = TRUE)


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


receiver_rank_df <- rbind(incomplete_df, fant_pt_dist_df) %>%
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
    sleeper_players_df %>%
      select(position, sportradar_id, gsis_id, espn_id, headshot_url),
    by = c("receiver_id" = "gsis_id")
  ) %>%
  left_join(espn_players_df %>%
              select(id, status, onTeamId),
            by = c("espn_id" = "id")) %>%
  # filter(status != "ONTEAM") %>% #Use to search through FA's
  mutate(tm_rnk = row_number()) %>%
  ungroup() %>% 
  group_by(position) %>% 
  mutate(pos_rnk = row_number()) %>% 
  ungroup() %>% 
  arrange(-tot_targ) %>% 
  mutate(lg_rnk = row_number()) %>% 
  filter(tm_rnk <= 5)

# make a data frame to loop around
sampling_df <- rbind(incomplete_df, fant_pt_dist_df) %>% 
  right_join(receiver_rank_df %>% select(posteam, receiver)) %>% 
  select(season, game_id, play_id, posteam, receiver, catch_run_prob, half_PPR_points) %>% 
  group_by(game_id, play_id)


# --- Simulation ---

# Parallel sampling
fx.sample_sim <- function(nsims = 10000, ncores = .66) {
  # do sim
  sample.fx <- function(x) {
    sampling_df %>% 
      mutate(sim_res = sample(half_PPR_points, 1, prob = catch_run_prob)) %>% 
      select(season, game_id, play_id, posteam, receiver, sim_res) %>% 
      distinct %>% 
      group_by(posteam, receiver) %>% 
      summarize(sim_tot = sum(sim_res, na.rm = T), .groups = 'drop') %>% 
      return
  }
    no_cores <- detectCores() * ncores
  cl <- makeCluster(no_cores, type="FORK")
  clusterEvalQ(cl, {
    library(tidyverse)
  })
    sim_df <- do.call(rbind, parLapply(cl, 1:10000, sample.fx))
  stopCluster(cl)
  saveRDS(sim_df, file = 'fantasy_football/data/sample_sim_df.rds')
  assign("sim_df", sim_df, envir = globalenv())
}

# Run simulation function
fx.sample_sim(10000, .58)

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


cols <- c("ONTEAM" = color_cw[5], "FREEAGENT" = color_cw[6], "WAIVERS" = color_cw[7])

plot_data <- sim_df %>%
  # filter(tm_rnk <= 4) %>% 
  left_join(percentile_df) %>%
  left_join(receiver_rank_df) %>%
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

receiver_rank_df <- receiver_rank_df %>% left_join(plot_data %>% select(receiver_id, pl_lab) %>% unique())


# Plot --------------------------------------------------------------------

# source('plots/assets/plot_theme.R')

p <- plot_data %>%
  select(!tm_rnk, !pos_rnk) %>% 
  filter(tm_rnk <= 4) %>% 
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
  scale_color_manual(
    values = cols,
    name = "Status"
    ) +
  # geom_shadowtext(
  #   aes(
  #     x = ifelse(obs_num == 1, 47, NA),
  #     y = tm_rnk - 0.5,
  #     color = factor(status)
  #   ),
  #   family = "Montserrat",
  #   hjust = 1,
  #   bg.color = color_cw[2],
  #   size = 1.2,
  #   na.rm = T,
  #   bg.r = 0.2
  # ) +
  geom_text(
    data = receiver_rank_df %>% filter(tm_rnk <= 4),
    aes(
      # x = ifelse(obs_num == 1, 28, NA),
      x = 42,
      y = tm_rnk - 0.5,
      color = factor(status)
    ),
    family = "Montserrat",
    hjust = 1,
    #[family = font_family,]
    size = 1.0,
    na.rm = T,
    show.legend = FALSE
  ) +
  geom_image(
    data = receiver_rank_df %>% filter(tm_rnk <= 4),
    aes(
      image = headshot_url, 
      x = 47, 
      y = tm_rnk - 0.5,
    ),
    size = 0.3,
    na.rm = T
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
  # scale_fill_viridis(option = "C", direction = -1, discrete = TRUE) +
  labs(
    title = paste0('2020 Expected 1/2 PPR Fantasy Points per Game as of Week ', my_week),
    subtitle = 'Grey represents middle 50% of outcomes, Orange tails are each 10% of outcomes  |  Caret shows actual avg  |  Based on 10,000 Simulations',
    x = NULL
  ) +
  theme_cw +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 8),
    legend.key.height = unit(.6, 'pt'),
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

brand_plot(p, asp = 16/9, save_name = 'fantasy_football/plots/xfp_half_PPR_box_2020_test.png', fade_borders = '', data_home = 'Data: @nflfastR', tm_wordmarks = T)



# TE ----------------------------------------------------------------------

# source('plots/assets/plot_theme.R')

p_te <- plot_data %>% 
  filter(position == "TE", pos_rnk <= 20) %>% 
  select(-c("tm_rnk", "lg_rnk")) %>% 
  arrange(-half_PPR_pg) %>% 
  # select_if(function(x) any(is.na(x))) %>% 
  # summarize_each(funs(sum(is.na(.))))
  ggplot(aes(
    x = sim_pg,
    y = pos_rnk,
    group = receiver,
    label = pl_lab,
    fill = stat(x)
  )) +
  geom_point(
    aes(x = ifelse(obs_num == 1, half_PPR_pg, NA), y = pos_rnk + 0.08),
    color = color_cw[2],
    fill = color_cw[8],
    shape = 24,
    size = 1,
    stroke = 0.2,
    na.rm = T
  ) +
  stat_density_ridges(
    # aes(fill = factor(stat(quantile))),
    geom = 'density_ridges_gradient',
    quantiles = c(.1, .25, .75, .9),
    rel_min_height = 0.001,
    bandwidth = 1,
    calc_ecdf = T,
    scale = 1.2,
    # color = color_cw[6],
    size = 0.2,
    show.legend = F,
  ) +
  # geom_density_ridges_gradient(
  #   # aes(
  #   #   fill = factor(sim_pg)
  #   # ),
  #   rel_min_height = 0.001,
  #   bandwidth = 1,
  #   calc_ecdf = T,
  #   scale = 0.9,
  #   # color = color_cw[6],
  #   size = 0.2,
  #   show.legend = F
  # ) +
  #geom_boxplot(size = 0.4, color = 'darkblue', width = 0.6, outlier.alpha = 0, notchwidth = 1) +
  scale_color_manual(
    values = cols,
    name = "Status"
  ) +
  # geom_shadowtext(
  #   aes(
  #     x = ifelse(obs_num == 1, 38, NA),
  #     y = pos_rnk - 0.5,
  #     color = factor(status)
  #   ),
  #   family = "Montserrat",
  #   hjust = 1,
  #   bg.color = color_cw[2],
  #   #[family = font_family,]
  #   size = 4.2,
  #   na.rm = T,
  #   bg.r = 0.2
  # ) +
  geom_text(
    data = receiver_rank_df %>% filter(position == "TE" & pos_rnk <= 20),
    aes(
      # x = ifelse(obs_num == 1, 28, NA),
      x = 27,
      y = pos_rnk - 0.5,
      color = factor(status)
    ),
    family = "Montserrat",
    hjust = 1,
    #[family = font_family,]
    size = 2.4,
    na.rm = T,
    show.legend = FALSE
  ) +
  geom_image(
    data = receiver_rank_df %>% filter(position == "TE" & pos_rnk <= 20),
    aes(
      image = headshot_url, 
      x = 28, 
      y = pos_rnk - 0.5,
    ),
    size = 0.05,
    na.rm = T
  ) +
  scale_x_continuous(
    breaks = seq(0, 80, 10),
    expand = expansion(mult = 0),
    limits = c(0, 30)
  ) +
  # scale_y_reverse(expand = expansion(mult = c(0, 0.04), add = c(0.2, 0)), limits = c(4.1, 0)) +
  scale_y_reverse(
  ) +
  # scale_fill_manual(values = c(
  #   alpha(color_cw[7], 0.5),
  #   alpha(color_cw[6], 0.5),
  #   alpha(color_cw[5], 0.5),
  #   alpha(color_cw[6], 0.5),
  #   alpha(color_cw[7], 0.5)
  # )) +
  scale_fill_viridis(
    name = "1/2 PPG", 
    option = "A",
    direction = -1
  ) +
  labs(
    title = paste0('2020 TE Expected 1/2 PPR Fantasy Points per Game\nThrough Week ', my_week),
    subtitle = 'Grey represents middle 50% of outcomes, Orange tails are each 10% of outcomes  |  Caret shows actual avg  |  Based on 10,000 Simulations',
    x = "1/2 PPR Points/Game",
    y = "Position Rank"
  ) +
  theme_cw +
  theme(
    plot.title = element_text(family = "Chivo", face = "bold", size = 12),
    plot.subtitle = element_text(size = 6),
    legend.key.height = unit(.6, 'pt'),
    axis.title.y = element_text(angle = 90),
    # axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text = element_text(size = 5),
    axis.line = element_line(color = color_cw[5], size = 0.5),
    panel.border = element_rect(color = color_cw[4], size = 0.1),
    axis.ticks.length = unit(0.15, 'lines'),
    axis.ticks = element_line(color = color_cw[5], size = 0.5),
    panel.grid.minor.x = element_line(color_cw[4], size = 0.2),
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

# p_te

# ggplot_gtable(ggplot_build(p))

brand_plot(p_te, asp = 6/9, save_name = 'fantasy_football/plots/xfp_half_PPR_box_TE_2020_test.png', fade_borders = '', data_home = 'Data: @nflfastR', tm_wordmarks = F)

# RB ----------------------------------------------------------------------


# source('plots/assets/plot_theme.R')

p_rb <- plot_data %>% 
  filter(position == "RB", pos_rnk <= 20) %>% 
  select(-c("tm_rnk", "lg_rnk")) %>% 
  arrange(-half_PPR_pg) %>% 
  # select_if(function(x) any(is.na(x))) %>% 
  # summarize_each(funs(sum(is.na(.))))
  ggplot(aes(
    x = sim_pg,
    y = pos_rnk,
    group = receiver,
    label = pl_lab,
    fill = stat(x)
  )) +
  geom_point(
    aes(x = ifelse(obs_num == 1, half_PPR_pg, NA), y = pos_rnk + 0.08),
    color = color_cw[2],
    fill = color_cw[8],
    shape = 24,
    size = 1,
    stroke = 0.2,
    na.rm = T
  ) +
  stat_density_ridges(
    # aes(fill = factor(stat(quantile))),
    geom = 'density_ridges_gradient',
    quantiles = c(.1, .25, .75, .9),
    rel_min_height = 0.001,
    bandwidth = 1,
    calc_ecdf = T,
    scale = 1.2,
    # color = color_cw[6],
    size = 0.2,
    show.legend = F,
  ) +
  # geom_density_ridges_gradient(
  #   # aes(
  #   #   fill = factor(sim_pg)
  #   # ),
  #   rel_min_height = 0.001,
  #   bandwidth = 1,
  #   calc_ecdf = T,
  #   scale = 0.9,
  #   # color = color_cw[6],
  #   size = 0.2,
  #   show.legend = F
  # ) +
  #geom_boxplot(size = 0.4, color = 'darkblue', width = 0.6, outlier.alpha = 0, notchwidth = 1) +
  scale_color_manual(
    values = cols,
    name = "Status"
  ) +
  # geom_shadowtext(
  #   aes(
  #     x = ifelse(obs_num == 1, 38, NA),
  #     y = pos_rnk - 0.5,
  #     color = factor(status)
  #   ),
  #   family = "Montserrat",
  #   hjust = 1,
  #   bg.color = color_cw[2],
  #   #[family = font_family,]
  #   size = 4.2,
  #   na.rm = T,
  #   bg.r = 0.2
  # ) +
  geom_text(
    data = receiver_rank_df %>% filter(position == "RB" & pos_rnk <= 20),
    aes(
      # x = ifelse(obs_num == 1, 28, NA),
      x = 27,
      y = pos_rnk - 0.5,
      color = factor(status)
    ),
    family = "Montserrat",
    hjust = 1,
    #[family = font_family,]
    size = 2.4,
    na.rm = T,
    show.legend = FALSE
  ) +
  geom_image(
    data = receiver_rank_df %>% filter(position == "RB" & pos_rnk <= 20),
    aes(
      image = headshot_url, 
      x = 28, 
      y = pos_rnk - 0.5,
    ),
    size = 0.05,
    na.rm = T
  ) +
  scale_x_continuous(
    breaks = seq(0, 80, 10),
    expand = expansion(mult = 0),
    limits = c(0, 30)
  ) +
  # scale_y_reverse(expand = expansion(mult = c(0, 0.04), add = c(0.2, 0)), limits = c(4.1, 0)) +
  scale_y_reverse(
  ) +
  # scale_fill_manual(values = c(
  #   alpha(color_cw[7], 0.5),
  #   alpha(color_cw[6], 0.5),
  #   alpha(color_cw[5], 0.5),
  #   alpha(color_cw[6], 0.5),
  #   alpha(color_cw[7], 0.5)
  # )) +
  scale_fill_viridis(
    name = "1/2 PPG", 
    option = "A",
    direction = -1
  ) +
  labs(
    title = paste0('2020 RB Expected 1/2 PPR Fantasy Points per Game\nThrough Week ', my_week),
    subtitle = 'Grey represents middle 50% of outcomes, Orange tails are each 10% of outcomes  |  Caret shows actual avg  |  Based on 10,000 Simulations',
    x = NULL,
    y = "Position Rank"
  ) +
  theme_cw +
  theme(
    plot.title = element_text(family = "Chivo", face = "bold", size = 12),
    plot.subtitle = element_text(size = 6),
    legend.key.height = unit(.6, 'pt'),
    axis.title.y = element_text(angle = 90),
    # axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text = element_text(size = 5),
    axis.line = element_line(color = color_cw[5], size = 0.5),
    panel.border = element_rect(color = color_cw[4], size = 0.1),
    axis.ticks.length = unit(0.15, 'lines'),
    axis.ticks = element_line(color = color_cw[5], size = 0.5),
    panel.grid.minor.x = element_line(color_cw[4], size = 0.2),
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

# p_rb

# ggplot_gtable(ggplot_build(p))

brand_plot(p_rb, asp = 6/9, save_name = 'fantasy_football/plots/xfp_half_PPR_box_RB_2020_test.png', fade_borders = '', data_home = 'Data: @nflfastR', tm_wordmarks = F)


# WR ----------------------------------------------------------------------

# source('plots/assets/plot_theme.R')

p_wr <- plot_data %>% 
  filter(position == "WR", pos_rnk <= 40) %>% 
  select(-c("tm_rnk", "lg_rnk")) %>% 
  arrange(-half_PPR_pg) %>% 
  # select_if(function(x) any(is.na(x))) %>% 
  # summarize_each(funs(sum(is.na(.))))
  ggplot(aes(
    x = sim_pg,
    y = pos_rnk,
    group = receiver,
    label = pl_lab,
    fill = stat(x)
  )) +
  geom_point(
    aes(x = ifelse(obs_num == 1, half_PPR_pg, NA), y = pos_rnk + 0.08),
    color = color_cw[2],
    fill = color_cw[8],
    shape = 24,
    size = 1,
    stroke = 0.2,
    na.rm = T
  ) +
  stat_density_ridges(
    # aes(fill = factor(stat(quantile))),
    geom = 'density_ridges_gradient',
    quantiles = c(.1, .25, .75, .9),
    rel_min_height = 0.001,
    bandwidth = 1,
    calc_ecdf = T,
    scale = 1.2,
    # color = color_cw[6],
    size = 0.2,
    show.legend = FALSE,
  ) +
  # geom_density_ridges_gradient(
  #   # aes(
  #   #   fill = factor(sim_pg)
  #   # ),
  #   rel_min_height = 0.001,
  #   bandwidth = 1,
  #   calc_ecdf = T,
  #   scale = 0.9,
  #   # color = color_cw[6],
  #   size = 0.2,
  #   show.legend = F
  # ) +
  #geom_boxplot(size = 0.4, color = 'darkblue', width = 0.6, outlier.alpha = 0, notchwidth = 1) +
  scale_color_manual(
    values = cols,
    name = "Status"
  ) +
  # geom_shadowtext(
  #   aes(
  #     x = ifelse(obs_num == 1, 38, NA),
  #     y = pos_rnk - 0.5,
  #     color = factor(status)
  #   ),
  #   family = "Montserrat",
  #   hjust = 1,
  #   bg.color = color_cw[2],
  #   #[family = font_family,]
  #   size = 4.2,
  #   na.rm = T,
  #   bg.r = 0.2
  # ) +
  geom_text(
    data = receiver_rank_df %>% filter(position == "WR" & pos_rnk <= 40),
    aes(
      # x = ifelse(obs_num == 1, 28, NA),
      x = 27,
      y = pos_rnk - 0.5,
      color = factor(status)
    ),
    family = "Montserrat",
    hjust = 1,
    #[family = font_family,]
    size = 1.3,
    na.rm = T,
    show.legend = F
  ) +
  geom_image(
    data = receiver_rank_df %>% filter(position == "WR" & pos_rnk <= 40),
    aes(
      image = headshot_url, 
      x = 28, 
      y = pos_rnk - 0.5,
    ),
    size = 0.03,
    na.rm = T
  ) +
  scale_x_continuous(
    breaks = seq(0, 80, 10),
    expand = expansion(mult = 0),
    limits = c(0, 30)
  ) +
  # scale_y_reverse(expand = expansion(mult = c(0, 0.04), add = c(0.2, 0)), limits = c(4.1, 0)) +
  scale_y_reverse(
  ) +
  # scale_fill_manual(values = c(
  #   alpha(color_cw[7], 0.5),
  #   alpha(color_cw[6], 0.5),
  #   alpha(color_cw[5], 0.5),
  #   alpha(color_cw[6], 0.5),
  #   alpha(color_cw[7], 0.5)
  # )) +
  scale_fill_viridis(
    name = "1/2 PPG", 
    option = "A",
    direction = -1
  ) +
  labs(
    title = paste0('2020 WR Expected 1/2 PPR Fantasy Points per Game\nThrough Week ', my_week),
    subtitle = 'Grey represents middle 50% of outcomes | Orange tails are each 10% of outcomes | Caret shows actual avg\nBased on 10,000 Simulations',
    x = "1/2 PPR Points/Game",
    y = "Position Rank"
  ) +
  theme_cw +
  theme(
    plot.title = element_text(family = "Chivo", face = "bold", size = 12),
    plot.subtitle = element_text(size = 6),
    legend.key.height = unit(.6, 'pt'),
    axis.title.y = element_text(angle = 90),
    # axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text = element_text(size = 5),
    axis.line = element_line(color = color_cw[5], size = 0.5),
    panel.border = element_rect(color = color_cw[4], size = 0.1),
    axis.ticks.length = unit(0.15, 'lines'),
    axis.ticks = element_line(color = color_cw[5], size = 0.5),
    panel.grid.minor.x = element_line(color_cw[4], size = 0.2),
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

# p_wr

brand_plot(p_wr, asp = 6/9, save_name = 'fantasy_football/plots/xfp_half_PPR_box_WR_2020_test.png', fade_borders = '', data_home = 'Data: @nflfastR', tm_wordmarks = F)


# ADoT --------------------------------------------------------------------

roster_df <- fast_scraper_roster(2020)

player_df <- pbp_df %>% 
  filter(!is.na(air_yards) & !is.na(receiver_id) & air_yards <= 70 & air_yards >= -15) %>% 
  group_by(season, posteam, receiver_id) %>% 
  summarise(n = n(), mean_air_yards = mean(air_yards)) %>% 
  arrange(-n) %>% 
  mutate(team_targ_rank = row_number()) %>% 
  filter(team_targ_rank <= 4) %>%
  arrange(-mean_air_yards) %>% 
  mutate(team_air_rank = row_number()) %>% 
  left_join(roster_df, by = c('season', 'receiver_id' = 'gsis_id')) %>% 
  mutate(jersey_number = NULL) %>% 
  ungroup

tm_grob_df <- data.frame(posteam = .tm_div_order, team_air_rank = 2, full_name = NA, headshot_url = ESPN_logo_url(.tm_div_order), air_yards = 20, vp.height = 1, alpha = 0.4)

grob_df <- player_df %>% 
  select(posteam, team_air_rank, full_name, headshot_url) %>% 
  mutate(air_yards = 40, vp.height = 0.4, alpha = 0, team_air_rank = team_air_rank) %>% 
  mutate(posteam = factor(posteam, .tm_div_order))

grob_df$grob <- sapply(1:nrow(grob_df), function(x) grob_img_adj(grob_df$headshot_url[x], alpha = grob_df$alpha[x]))

my_week <- pbp_df %>% select(week) %>% max()

p <- pbp_df %>% 
  filter(!is.na(air_yards) & !is.na(receiver_id) & air_yards <= 70 & air_yards >= -15) %>% 
  right_join(player_df) %>%
  mutate(posteam = factor(posteam, .tm_div_order)) %>% 
  ggplot(aes(group = team_air_rank, x = air_yards, y = team_air_rank)) +
  facet_wrap(.~posteam, nrow = 8, scales = 'free_x') +
  geom_image(data = tm_grob_df, aes(image = headshot_url), size = 0.7, asp = 1.375) +
  geom_image(data = tm_grob_df, aes(image = headshot_url), size = 0.7, color = 'white', alpha = 0.8, asp = 1.375) +
  geom_vline(xintercept = seq(0,40,10), color = 'grey85', size = 0.3) +
  geom_grob(data = grob_df, aes(x = air_yards, y = team_air_rank - 0.4, label = grob, vp.height = vp.height)) +
  geom_hline(data = grob_df, aes(yintercept = ifelse(is.na(full_name), NA, team_air_rank), color = posteam), size = 0.6, show.legend = F) +
  geom_density_ridges(aes(color = posteam, fill = posteam), scale = 1.2, bandwidth = 3, panel_scaling = F, show.legend = F, size = 0.4) +
  geom_shadowtext(data = grob_df, aes(label = full_name, x = air_yards - 3, y = team_air_rank - 0.6), color = color_cw[5], bg.color = color_cw[2], bg.r = 0.2, hjust = 1, size = 1.5) +
  scale_y_reverse(expand = expansion(add = c(0.2,0.2))) +
  scale_x_continuous(limits = c(-5,45), expand = expansion(mult = 0)) +
  scale_color_manual(values = NFL_sec) +
  scale_fill_manual(values = NFL_pri) +
  labs(title = '2020 Targeted Air Yards Distribution',
       subtitle = paste0('Four most targeted players on each team as of Week ', my_week),
       x = NULL,
       y = NULL) +
  theme_cw +
  theme(
    strip.text = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major = element_blank(),
    axis.text = element_text(size = 4),
    axis.line.x = element_line(color = 'darkblue', size = 0.5),
    axis.line.y = element_blank(),
    panel.background = element_rect(fill = 'transparent'),
    panel.border = element_rect(color = 'grey95', size = 0.1),
    axis.ticks.length = unit(0.1, 'lines'),
    axis.ticks = element_line(color = 'darkblue', size = 0.5)
  )

brand_plot(p, asp = 16/9, save_name = 'adot_box_2020_test_no_legend.png', fade_borders = '', data_home = 'Data: @nflfastR', tm_wordmarks = T)

rm(list = ls())