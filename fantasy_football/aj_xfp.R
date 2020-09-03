# NOTES
# 
# Adjust color scale automatically based on points and columns

library(tidyverse)

source('https://github.com/mrcaseb/nflfastR/raw/master/R/helper_add_xyac.R')
source('https://github.com/mrcaseb/nflfastR/raw/master/R/helper_add_nflscrapr_mutations.R')


# YAC Distribution Function -----------------------------------------------

# duplicate the add_xyac() function that we sourced above
add_xyac_dist <- add_xyac

# separate each block of code in the add_xyac_dist() function into blocks
add_xyac_blocks <- body(add_xyac_dist) %>% as.list

# we want to remove lines 51 to 62 from the 5th item in the list
add_xyac_blocks[[5]] <- add_xyac_blocks[[5]] %>% 
  format %>% 
  .[-(51:62)] %>% 
  paste(collapse = '\n') %>% 
  str2lang

# replace the body of add_xyac_dist() with our new edited function
body(add_xyac_dist) <- add_xyac_blocks %>% as.call


# Data --------------------------------------------------------------------

pbp_df <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2019.rds'))

avg_exp_fp_df <- pbp_df %>% 
  filter(pass_attempt==1 & season_type=='REG' & two_point_attempt==0 & !is.na(receiver_id)) %>% 
  add_xyac_dist %>% 
  select(season = season.x, game_id, play_id, posteam = posteam.x, receiver, yardline_100 = yardline_100.x, air_yards = air_yards.x, actual_yards_gained = yards_gained, complete_pass, cp, yac_prob = prob, gain) %>% 
  mutate(
    gain = ifelse(yardline_100==air_yards, yardline_100, gain),
    yac_prob = ifelse(yardline_100==air_yards, 1, yac_prob),
    PPR_points = 1 + gain/10 + ifelse(gain == yardline_100, 6, 0),
    half_PPR_points = .5 + gain/10 + ifelse(gain == yardline_100, 6, 0),
    catch_run_prob = cp * yac_prob,
    exp_PPR_points = PPR_points * catch_run_prob,
    exp_half_PPR_points = half_PPR_points * catch_run_prob,
    exp_yards = gain * catch_run_prob,
    actual_outcome = ifelse(actual_yards_gained==gain & complete_pass==1, 1, 0),
    actual_PPR_points = ifelse(actual_outcome==1, PPR_points, 0),
    actual_half_PPR_points = ifelse(actual_outcome==1, half_PPR_points, 0),
    target = 0,
    game_played = 0,
  )  %>% 
  group_by(game_id, receiver) %>% 
  mutate(game_played = ifelse(row_number()==1,1,0)) %>% 
  ungroup %>% 
  group_by(game_id, play_id, receiver) %>% 
  mutate(target = ifelse(row_number()==1,1,0)) %>% 
  ungroup %>% 
  group_by(posteam, receiver) %>% 
  summarize(
    games = sum(game_played, na.rm = T),
    targets = sum(target, na.rm = T),
    catches = sum(actual_outcome, na.rm = T),
    yards = sum(ifelse(actual_outcome==1, gain, 0), na.rm = T),
    td = sum(ifelse(gain==yardline_100, actual_outcome, 0), na.rm = T),
    PPR_pts = sum(actual_PPR_points, na.rm = T),
    half_PPR_pts = sum(actual_half_PPR_points, na.rm = T),
    exp_catches = sum(ifelse(target==1, cp, NA), na.rm = T),
    exp_yards = sum(exp_yards, na.rm = T),
    exp_td = sum(ifelse(gain==yardline_100, catch_run_prob, 0), na.rm = T),
    exp_PPR_pts = sum(exp_PPR_points, na.rm = T),
    exp_half_PPR_pts = sum(exp_half_PPR_points, na.rm = T)
  ) %>% 
  mutate(
    half_ppr_pts_diff = half_PPR_pts - exp_half_PPR_pts,
    ppr_pts_diff = PPR_pts - exp_PPR_pts
  ) %>% 
  ungroup



library(gt)
# make the table
# 1/2 PPR xFP table
avg_exp_fp_df %>%
  select(
    games,
    receiver,
    posteam,
    targets,
    catches,
    yards,
    td,
    half_PPR_pts,
    exp_catches,
    exp_yards,
    exp_td,
    exp_half_PPR_pts,
    half_ppr_pts_diff
  ) %>%
  arrange(-exp_half_PPR_pts) %>% 
  dplyr::slice(1:50) %>% 
  mutate(Rank = paste0('#',row_number())) %>%
  gt() %>%
  tab_header(title = 'Expected Receiving 1/2 PPR Fantasy Points, 2019') %>% 
  cols_move_to_start(columns = vars(Rank)) %>% 
  cols_label(
    games = 'GP',
    receiver = '',
    posteam = '',
    targets = 'Targ',
    catches = 'Rec',
    yards = 'Yds',
    td = 'TD',
    half_PPR_pts = 'FP',
    exp_catches = 'Rec',
    exp_yards = 'Yds',
    exp_td = 'TD',
    exp_half_PPR_pts = 'FP',
    half_ppr_pts_diff = "Pts Diff."
  ) %>% 
  fmt_number(columns = vars(exp_td, half_PPR_pts, exp_half_PPR_pts, half_ppr_pts_diff), decimals = 1) %>% 
  fmt_number(columns = vars(yards, exp_yards, exp_catches), decimals = 0, sep_mark = ',') %>% 
  tab_style(style = cell_text(size = 'x-large'), locations = cells_title(groups = 'title')) %>% 
  tab_style(style = cell_text(align = 'center', size = 'medium'), locations = cells_body()) %>% 
  tab_style(style = cell_text(align = 'left'), locations = cells_body(vars(receiver))) %>% 
  tab_spanner(label = 'Actual', columns = vars(catches, yards, td, half_PPR_pts)) %>% 
  tab_spanner(label = 'Expected', columns = vars(exp_catches, exp_yards, exp_td, exp_half_PPR_pts)) %>% 
  tab_source_note(source_note = '') %>% 
  data_color(
    columns = vars(half_PPR_pts, exp_half_PPR_pts),
    colors = scales::col_numeric(palette = c('grey97', 'darkorange1'), domain = c(100, 380)), # anyway to automate?
    autocolor_text = FALSE
  ) %>%
  data_color(
    columns = vars(half_ppr_pts_diff),
    colors = scales::col_numeric(palette = c('grey97', 'darkorange1'), domain = c(max(avg_exp_fp_df$half_ppr_pts_diff), min(avg_exp_fp_df$half_ppr_pts_diff))),
    autocolor_text = FALSE
  ) %>% 
  text_transform(
    locations = cells_body(vars(posteam)),
    fn = function(x) web_image(url = paste0('https://a.espncdn.com/i/teamlogos/nfl/500/',x,'.png'))
  ) %>% 
  cols_width(vars(posteam) ~ px(45)) %>% 
  tab_options(
    table.font.color = 'darkblue',
    data_row.padding = '2px',
    row_group.padding = '3px',
    column_labels.border.bottom.color = 'darkblue',
    column_labels.border.bottom.width = 1.4,
    table_body.border.top.color = 'darkblue',
    row_group.border.top.width = 1.5,
    row_group.border.top.color = '#999999',
    table_body.border.bottom.width = 0.7,
    table_body.border.bottom.color = '#999999',
    row_group.border.bottom.width = 1,
    row_group.border.bottom.color = 'darkblue',
    table.border.top.color = 'transparent',
    table.background.color = '#F2F2F2',
    table.border.bottom.color = 'transparent',
    row.striping.background_color = '#FFFFFF',
    row.striping.include_table_body = TRUE
  ) %>% 
  gtsave(filename = paste0("xFP_share_half_ppr_", pbp_df$season[1], ".png"), path = "fantasy_football/plots")


# Full PPR xFP table
avg_exp_fp_df %>%
  select(
    games,
    receiver,
    posteam,
    targets,
    catches,
    yards,
    td,
    PPR_pts,
    exp_catches,
    exp_yards,
    exp_td,
    exp_PPR_pts,
    ppr_pts_diff
  ) %>%
  arrange(-exp_PPR_pts) %>% 
  dplyr::slice(1:50) %>% 
  mutate(Rank = paste0('#',row_number())) %>%
  gt() %>%
  tab_header(title = 'Expected Receiving PPR Fantasy Points, 2019') %>% 
  cols_move_to_start(columns = vars(Rank)) %>% 
  cols_label(
    games = 'GP',
    receiver = '',
    posteam = '',
    targets = 'Targ',
    catches = 'Rec',
    yards = 'Yds',
    td = 'TD',
    PPR_pts = 'FP',
    exp_catches = 'Rec',
    exp_yards = 'Yds',
    exp_td = 'TD',
    exp_PPR_pts = 'FP',
    ppr_pts_diff = "Pts Diff."
  ) %>% 
  fmt_number(columns = vars(exp_td, PPR_pts, exp_PPR_pts, ppr_pts_diff), decimals = 1) %>% 
  fmt_number(columns = vars(yards, exp_yards, exp_catches), decimals = 0, sep_mark = ',') %>% 
  tab_style(style = cell_text(size = 'x-large'), locations = cells_title(groups = 'title')) %>% 
  tab_style(style = cell_text(align = 'center', size = 'medium'), locations = cells_body()) %>% 
  tab_style(style = cell_text(align = 'left'), locations = cells_body(vars(receiver))) %>% 
  tab_spanner(label = 'Actual', columns = vars(catches, yards, td, PPR_pts)) %>% 
  tab_spanner(label = 'Expected', columns = vars(exp_catches, exp_yards, exp_td, exp_PPR_pts)) %>% 
  tab_source_note(source_note = '') %>% 
  data_color(
    columns = vars(PPR_pts, exp_PPR_pts),
    colors = scales::col_numeric(palette = c('grey97', 'darkorange1'), domain = c(100, 380)), # need to adjust for full PPR point scale
    autocolor_text = FALSE
  ) %>%
  data_color(
    columns = vars(ppr_pts_diff),
    colors = scales::col_numeric(palette = c('grey97', 'darkorange1'), domain = c(max(avg_exp_fp_df$ppr_pts_diff), min(avg_exp_fp_df$ppr_pts_diff))),
    autocolor_text = FALSE
  ) %>% 
  text_transform(
    locations = cells_body(vars(posteam)),
    fn = function(x) web_image(url = paste0('https://a.espncdn.com/i/teamlogos/nfl/500/',x,'.png'))
  ) %>% 
  cols_width(vars(posteam) ~ px(45)) %>% 
  tab_options(
    table.font.color = 'darkblue',
    data_row.padding = '2px',
    row_group.padding = '3px',
    column_labels.border.bottom.color = 'darkblue',
    column_labels.border.bottom.width = 1.4,
    table_body.border.top.color = 'darkblue',
    row_group.border.top.width = 1.5,
    row_group.border.top.color = '#999999',
    table_body.border.bottom.width = 0.7,
    table_body.border.bottom.color = '#999999',
    row_group.border.bottom.width = 1,
    row_group.border.bottom.color = 'darkblue',
    table.border.top.color = 'transparent',
    table.background.color = '#F2F2F2',
    table.border.bottom.color = 'transparent',
    row.striping.background_color = '#FFFFFF',
    row.striping.include_table_body = TRUE
  ) %>% 
  gtsave(filename = paste0("xFP_share_ppr_", pbp_df$season[1], ".png"), path = "fantasy_football/plots")

# requires:
# install.packages("webshot")
# library(webshot)
# webshot::install_phantomjs()


# Distribution ------------------------------------------------------------


fant_pt_dist_df <- pbp_df %>% 
  filter(pass_attempt==1 & season_type=='REG' & two_point_attempt==0 & !is.na(receiver_id) & receiver == 'S.Watkins' & week <= 2) %>% 
  add_xyac_dist %>% 
  select(season = season.x, game_id, play_id, posteam = posteam.x, receiver, yardline_100 = yardline_100.x, air_yards = air_yards.x, actual_yards_gained = yards_gained, complete_pass, cp, yac_prob = prob, gain) %>% 
  mutate(
    gain = ifelse(yardline_100==air_yards, yardline_100, gain),
    yac_prob = ifelse(yardline_100==air_yards, 1, yac_prob),
    PPR_points = 1 + gain/10 + ifelse(gain == yardline_100, 6, 0),
    catch_run_prob = cp * yac_prob,
    exp_PPR_points = PPR_points * catch_run_prob,
    actual_outcome = ifelse(actual_yards_gained==gain & complete_pass==1, 1, 0),
    actual_PPR_points = ifelse(actual_outcome==1, PPR_points, 0),
    target = 0,
    game_played = 0
  )

incomplete_df <- fant_pt_dist_df %>% 
  mutate(
    gain = 0,
    PPR_points = 0,
    yac_prob = 0,
    exp_PPR_points = 0,
    complete_pass = 0,
    catch_run_prob = 1 - cp,
    actual_outcome = NA,
    actual_PPR_points = NA,
    target = 1
  ) %>% 
  distinct %>% 
  group_by(game_id, receiver) %>% 
  mutate(game_played = ifelse(row_number()==1,1,0)) %>% 
  ungroup



# make a data frame to loop around
sampling_df <- rbind(incomplete_df, fant_pt_dist_df) %>% 
  select(season, game_id, play_id, posteam, receiver, catch_run_prob, PPR_points) %>% 
  group_by(game_id, play_id)

# do sim
sim_df <- do.call(rbind, lapply(1:10000, function(x) {
  sampling_df %>% 
    mutate(sim_res = sample(PPR_points, 1, prob = catch_run_prob)) %>% 
    select(season, game_id, play_id, posteam, receiver, sim_res) %>% 
    distinct %>% 
    group_by(game_id, posteam, receiver) %>% 
    summarize(sim_tot = sum(sim_res, na.rm = T), .groups = 'drop') %>% 
    return
}))

sim_df <- sim_df %>% mutate(sim = 1)

# calculate how many points were actually scored
actual_df <- fant_pt_dist_df %>%
  group_by(game_id, posteam, receiver) %>% 
  summarize(sim_tot = sum(actual_PPR_points, na.rm = T), .groups = 'drop') %>% 
  mutate(sim = 0)

# figure out what percentile the actual values fall in
percentile_df <- rbind(sim_df, actual_df) %>% 
  group_by(game_id, posteam, receiver) %>% 
  mutate(perc = percent_rank(sim_tot)) %>% 
  filter(sim == 0)




library(scales)


ggplot(data = sim_df, aes(x = sim_tot, group = game_id, color = game_id, fill = game_id)) +
  geom_density(alpha = 0.1, size = 1) +
  geom_spoke(data = percentile_df, aes(angle = pi/2, radius = 0.01, y = 0), size = 1, show.legend = F)  + 
  geom_label(data = percentile_df, aes(y = 0.01, label = paste0('Actual\n',game_id,'\n',number(round(perc*100,2),accuracy = 0.1), ' perc.')), size = 2, fill = 'grey98', show.legend = F)  + 
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(labels = percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.05))) +
  scale_color_manual(values = c('#ff7f00','#9932cc')) +
  scale_fill_manual(values = c('#ff7f00','#9932cc')) +
  labs(title = 'Sammy Watkins Expected PPR Fantasy Point Distribution',
       subtitle = 'Based on 10,000 Simulations',
       y = 'Density',
       x = 'Expected PPR Fantasy Points',
       color = NULL,
       fill = NULL) +
  theme(
    line = element_line(lineend = 'round', color='darkblue'),
    text = element_text(color='darkblue'),
    plot.background = element_rect(fill = 'grey95', color = 'transparent'),
    panel.border = element_rect(color = 'darkblue', fill = NA),
    panel.background = element_rect(fill = 'white', color = 'transparent'),
    axis.ticks = element_line(color = 'darkblue', size = 0.5),
    axis.ticks.length = unit(2.75, 'pt'),
    axis.title = element_text(size = 8),
    axis.text = element_text(size = 7, color = 'darkblue'),
    plot.title = element_text(size = 14),
    plot.subtitle = element_text(size = 8),
    plot.caption = element_text(size = 5),
    legend.background = element_rect(fill = 'grey90', color = 'darkblue'),
    legend.key = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color='grey85', size = 0.3),
    axis.title.y = element_text(angle = 0, vjust = 0.5),
    legend.position = 'bottom'
  )