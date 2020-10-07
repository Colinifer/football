# NOTES
# 
# Adjust color scale automatically based on points and columns

library(tidyverse)

source('https://raw.githubusercontent.com/mrcaseb/nflfastR/master/R/utils.R')
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
pbp_df <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2020.rds'))

# Receivers ---------------------------------------------------------------

# Average Expected Fantasy Points - Receivers
avg_exp_fp_df <- pbp_df %>% 
  filter(pass_attempt==1 & season_type=='REG' & two_point_attempt==0 & !is.na(receiver_id)) %>% 
  add_xyac_dist %>% 
  select(season = season.x, game_id, play_id, posteam = posteam.x, receiver, receiver_player_id, receiver_id, yardline_100 = yardline_100.x, air_yards = air_yards.x, actual_yards_gained = yards_gained, complete_pass, cp, yac_prob = prob, gain) %>% 
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
    game_played = 0
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
    columns = vars(half_PPR_pts),
    colors = scales::col_numeric(palette = c('grey97', 'darkorange1'), domain = c(max(avg_exp_fp_df$half_PPR_pts), min(avg_exp_fp_df$half_PPR_pts))), # anyway to automate?
    autocolor_text = FALSE
  ) %>%
  data_color(
    columns = vars(exp_half_PPR_pts),
    colors = scales::col_numeric(palette = c('grey97', 'darkorange1'), domain = c(max(avg_exp_fp_df$exp_half_PPR_pts), min(avg_exp_fp_df$exp_half_PPR_pts))), # need to adjust for full PPR point scale
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
    columns = vars(PPR_pts),
    colors = scales::col_numeric(palette = c('grey97', 'darkorange1'), domain = c(max(avg_exp_fp_df$PPR_pts), min(avg_exp_fp_df$PPR_pts))), # need to adjust for full PPR point scale
    autocolor_text = FALSE
  ) %>%
  data_color(
    columns = vars(exp_PPR_pts),
    colors = scales::col_numeric(palette = c('grey97', 'darkorange1'), domain = c(max(avg_exp_fp_df$exp_PPR_pts), min(avg_exp_fp_df$exp_PPR_pts))), # need to adjust for full PPR point scale
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


# Quarterbacks ------------------------------------------------------------

# Completed Air Yards Over Expected
cayoe_xyac <- pbp_df %>%
  filter(pass_attempt == 1 &
           season_type == 'REG' &
           two_point_attempt == 0 & !is.na(receiver_id) &
           # wp > .2 &
           # wp < .8 &
           air_yards > 0) %>%
  add_xyac_dist

cayoe <- cayoe_xyac %>%
  select(
    season = season.x,
    game_id,
    play_id,
    posteam = posteam.x,
    passer,
    passer_player_id,
    passer_id,
    receiver,
    receiver_player_id,
    receiver_id,
    yardline_100 = yardline_100.x,
    air_yards = air_yards.x,
    actual_yards_gained = yards_gained,
    complete_pass,
    cp,
    cpoe,
    yac_prob = prob,
    gain,
    pass_attempt
  ) %>%
  mutate(
    gain = ifelse(yardline_100 == air_yards, yardline_100, gain),
    yac_prob = ifelse(yardline_100 == air_yards, 1, yac_prob),
    PPR_points = 1 + gain / 10 + ifelse(gain == yardline_100, 6, 0),
    half_PPR_points = .5 + gain / 10 + ifelse(gain == yardline_100, 6, 0),
    catch_run_prob = cp * yac_prob,
    exp_PPR_points = PPR_points * catch_run_prob,
    exp_half_PPR_points = half_PPR_points * catch_run_prob,
    exp_yards = gain * catch_run_prob,
    actual_outcome = ifelse(actual_yards_gained == gain &
                              complete_pass == 1, 1, 0),
    actual_PPR_points = ifelse(actual_outcome == 1, PPR_points, 0),
    actual_half_PPR_points = ifelse(actual_outcome == 1, half_PPR_points, 0),
    # completion = 0,
    attempt = 0,
    game_played = 0,
    cayoe = cpoe * air_yards,
    sum_cayoe = 0
  ) %>%
  group_by(game_id, passer_player_id) %>%
  mutate(game_played = ifelse(row_number() == 1, 1, 0)) %>%
  ungroup %>%
  # group_by(game_id, play_id, passer) %>%
  # mutate(completion = ifelse(row_number() == 1, 1, 0)) %>%
  # ungroup %>%
  group_by(game_id, play_id, receiver) %>% 
  mutate(attempt = ifelse(row_number()==1,1,0)) %>% 
  ungroup %>% 
  group_by(posteam, passer) %>%
  # filter()
  summarize(
    games = sum(game_played, na.rm = T),
    pass_attempts = sum(attempt, na.rm = T),
    completions = sum(actual_outcome, na.rm = T),
    yards = sum(ifelse(actual_outcome == 1, gain, 0), na.rm = T),
    td = sum(ifelse(gain == yardline_100, actual_outcome, 0), na.rm = T),
    PPR_pts = sum(actual_PPR_points, na.rm = T),
    half_PPR_pts = sum(actual_half_PPR_points, na.rm = T),
    exp_completions = sum(ifelse(attempt == 1, cp, NA), na.rm = T),
    exp_yards = sum(exp_yards, na.rm = T),
    exp_td = sum(ifelse(gain == yardline_100, catch_run_prob, 0), na.rm = T),
    exp_PPR_pts = sum(exp_PPR_points, na.rm = T),
    exp_half_PPR_pts = sum(exp_half_PPR_points, na.rm = T),
    sum_cayoe = sum(cayoe, na.rm = T),
  ) %>%
  mutate(
    half_ppr_pts_diff = half_PPR_pts - exp_half_PPR_pts,
    ppr_pts_diff = PPR_pts - exp_PPR_pts,
    cayoe_a = sum_cayoe / pass_attempts
  ) %>%
  ungroup %>%
  filter(pass_attempts > 5)


# xFP QB table
cayoe %>%
  select(
    games,
    passer,
    posteam,
    pass_attempts,
    completions,
    yards,
    td,
    PPR_pts,
    exp_completions,
    exp_yards,
    exp_td,
    exp_PPR_pts,
    ppr_pts_diff,
    sum_cayoe,
    cayoe_a
  ) %>%
  arrange(-exp_PPR_pts) %>% 
  dplyr::slice(1:50) %>% 
  mutate(Rank = paste0('#',row_number())) %>%
  gt() %>%
  tab_header(title = 'Expected Receiving PPR Fantasy Points, 2019') %>% 
  cols_move_to_start(columns = vars(Rank)) %>% 
  cols_label(
    games = 'GP',
    passer = '',
    posteam = '',
    pass_attempts = 'PA',
    completions = 'Comp',
    yards = 'Yds',
    td = 'TD',
    PPR_pts = 'FP',
    exp_completions = 'xComp',
    exp_yards = 'xYds',
    exp_td = 'xTD',
    exp_PPR_pts = 'xFP',
    ppr_pts_diff = "Pts Diff."
  ) %>% 
  fmt_number(columns = vars(exp_td, PPR_pts, exp_PPR_pts, ppr_pts_diff), decimals = 1) %>% 
  fmt_number(columns = vars(yards, exp_yards, exp_completions), decimals = 0, sep_mark = ',') %>% 
  tab_style(style = cell_text(size = 'x-large'), locations = cells_title(groups = 'title')) %>% 
  tab_style(style = cell_text(align = 'center', size = 'medium'), locations = cells_body()) %>% 
  tab_style(style = cell_text(align = 'left'), locations = cells_body(vars(passer))) %>% 
  tab_spanner(label = 'Actual', columns = vars(completions, yards, td, PPR_pts)) %>% 
  tab_spanner(label = 'Expected', columns = vars(exp_completions, exp_yards, exp_td, exp_PPR_pts)) %>% 
  tab_source_note(source_note = '') %>% 
  data_color(
    columns = vars(PPR_pts),
    colors = scales::col_numeric(palette = c('grey97', 'darkorange1'), domain = c(max(cayoe$PPR_pts), min(cayoe$PPR_pts))), # need to adjust for full PPR point scale
    autocolor_text = FALSE
  ) %>%
  data_color(
    columns = vars(exp_PPR_pts),
    colors = scales::col_numeric(palette = c('grey97', 'darkorange1'), domain = c(max(cayoe$exp_PPR_pts), min(cayoe$exp_PPR_pts))), # need to adjust for full PPR point scale
    autocolor_text = FALSE
  ) %>%
  data_color(
    columns = vars(ppr_pts_diff),
    colors = scales::col_numeric(palette = c('grey97', 'darkorange1'), domain = c(max(cayoe$ppr_pts_diff), min(cayoe$ppr_pts_diff))),
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
  gtsave(filename = paste0("xFP_QB_", pbp_df$season[1], ".png"), path = "fantasy_football/plots")


# Distribution ------------------------------------------------------------

source('https://github.com/ajreinhard/data-viz/raw/master/ggplot/plot_SB.R')

my_week <- 4

quick_rost <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-raw/master/roster/roster.rds')) 

# quick_rost %>% 
#   filter(Pos == 'TE' & (No >= 90 | No <= 79) & No != 0 & Status == 'ACT') %>% 
#   view
# #pull(No) %>% table
# 
# quick_rost %>% 
#   filter(Pos == 'WR' & (No >= 90 | (No <= 79 & No >= 20) | No <= 9)  & No != 0 & Status == 'ACT') %>% 
#   view
# 
# quick_rost %>% 
#   filter(((No < 90 & No > 79) | (No > 20 & No < 9)) & Status == 'ACT' & (Pos != 'WR' & Pos != 'TE')) %>% 
#   #pull(Pos) %>% table
#   view

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

p <- sim_df %>% 
  left_join(percentile_df) %>% 
  left_join(WR_rank_df) %>% 
  mutate(
    sim_pg = sim_tot / tot_gp,
    pl_lab = paste0(receiver, '\n', number(perc * 100, accuracy = 0.1), ' perc.'),
    posteam = factor(posteam, .tm_div_order_alt)
  ) %>% 
  group_by(posteam, receiver) %>% 
  mutate(obs_num = row_number()) %>% 
  ggplot(aes(x = sim_pg, y = tm_rnk, group = receiver, label = pl_lab)) +
  facet_wrap(. ~ posteam, nrow = 4, scales = 'free') +
  geom_point(aes(x = ifelse(obs_num==1, half_PPR_pg, NA), y = tm_rnk + 0.08), color = 'darkblue', fill = color_SB[1], shape = 24, size = 0.6, stroke = 0.2, na.rm = T) +
  stat_density_ridges(
    aes(fill = factor(stat(quantile))),
    geom = 'density_ridges_gradient',
    quantiles = c(.1,.25,.75,.9),
    rel_min_height = 0.001,
    bandwidth = 1,
    calc_ecdf = T,
    scale = 0.9,
    color = 'grey50',
    size = 0.2,
    show.legend = F
  ) +
  #geom_boxplot(size = 0.4, color = 'darkblue', width = 0.6, outlier.alpha = 0, notchwidth = 1) +
  geom_shadowtext(aes(x = ifelse(obs_num==1, 48, NA), y = tm_rnk - 0.5), hjust = 1, color = 'darkblue', bg.color = 'white', family = "Helvetica", size = 1.2, na.rm = T, bg.r = 0.2) +
  scale_x_continuous(breaks = seq(0,80,10), expand = expansion(mult = 0), limits = c(0,50)) +
  scale_y_reverse(expand = expansion(mult = c(0, 0.04), add = c(0.2, 0)), limits = c(4.1,0)) +
  scale_fill_manual(values = c(alpha(color_SB[1], 0.4),alpha(color_SB[4], 0.4),alpha('grey60', 0.4),alpha(color_SB[4], 0.4),alpha(color_SB[1], 0.4))) +
  labs(title = paste0('2020 Expected 1/2 PPR Fantasy Points per Game as of Week ', my_week),
       subtitle = 'Grey represents middle 50% of outcomes, Orange tails are each 10% of outcomes  |  Caret shows actual avg  |  Based on 10,000 Simulations',
       x = NULL) +
  theme_SB +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text = element_text(size = 4),
    axis.line = element_line(color = 'darkblue', size = 0.5),
    panel.border = element_rect(color = 'grey95', size = 0.1),
    axis.ticks.length = unit(0.15, 'lines'),
    axis.ticks = element_line(color = 'darkblue', size = 0.5),
    panel.grid.minor.x = element_line('grey95', size = 0.2),
    panel.spacing.x = unit(0.5, 'lines')
  ) +
  annotation_custom(make_gradient(deg = 270), ymin=Inf, ymax=-0.1, xmin=-Inf, xmax=Inf) +
  annotation_custom(make_gradient(deg = 0), ymin=-Inf, ymax=Inf, xmin=48, xmax=Inf)

brand_plot(p, asp = 16/9, save_name = 'xfp_half_PPR_box_2020.png', data_home = 'Data: @nflfastR', tm_wordmarks = T)


# PPR Distribution

# make a data frame to loop around
sampling_df <- rbind(incomplete_df, fant_pt_dist_df) %>% 
  right_join(WR_rank_df %>% select(posteam, receiver)) %>% 
  select(season, game_id, play_id, posteam, receiver, receiver_id, catch_run_prob, PPR_points) %>% 
  group_by(game_id, play_id)

# do sim
sim_df <- do.call(rbind, lapply(1:5000, function(x) {
  sampling_df %>% 
    mutate(sim_res = sample(PPR_points, 1, prob = catch_run_prob)) %>% 
    select(season, game_id, play_id, posteam, receiver, receiver_id, sim_res) %>% 
    distinct %>% 
    group_by(posteam, receiver) %>% 
    summarize(sim_tot = sum(sim_res, na.rm = T), .groups = 'drop') %>% 
    return
}))

sim_df <- sim_df %>% mutate(sim = 1)

# calculate how many points were actually scored
actual_df <- fant_pt_dist_df %>%
  group_by(posteam, receiver) %>% 
  summarize(sim_tot = sum(actual_PPR_points, na.rm = T), .groups = 'drop') %>% 
  mutate(sim = 0)

# figure out what percentile the actual values fall in
percentile_df <- rbind(sim_df, actual_df) %>% 
  group_by(posteam, receiver) %>% 
  mutate(perc = percent_rank(sim_tot)) %>% 
  filter(sim == 0) %>% 
  mutate(sim_tot = NULL, sim = NULL)


library(ggridges)

p <- sim_df %>% 
  left_join(percentile_df) %>% 
  left_join(WR_rank_df) %>% 
  mutate(
    sim_pg = sim_tot / tot_gp,
    pl_lab = paste0(receiver, '\n', number(perc * 100, accuracy = 0.1), ' perc.'),
    posteam = factor(posteam, .tm_div_order_alt)
  ) %>% 
  group_by(posteam, receiver) %>% 
  mutate(obs_num = row_number()) %>% 
  ggplot(aes(x = sim_pg, y = tm_rnk, group = receiver, label = pl_lab)) +
  facet_wrap(. ~ posteam, nrow = 4, scales = 'free') +
  geom_point(aes(x = ifelse(obs_num==1, PPR_pg, NA), y = tm_rnk + 0.08), color = 'darkblue', fill = color_SB[1], shape = 24, size = 0.6, stroke = 0.2, na.rm = T) +
  stat_density_ridges(
    aes(fill = factor(stat(quantile))),
    geom = 'density_ridges_gradient',
    quantiles = c(.1,.25,.75,.9),
    rel_min_height = 0.001,
    bandwidth = 1,
    calc_ecdf = T,
    scale = 0.9,
    color = 'grey50',
    size = 0.2,
    show.legend = F
  ) +
  #geom_boxplot(size = 0.4, color = 'darkblue', width = 0.6, outlier.alpha = 0, notchwidth = 1) +
  geom_shadowtext(aes(x = ifelse(obs_num==1, 48, NA), y = tm_rnk - 0.5), hjust = 1, color = 'darkblue', bg.color = 'white', family = "Helvetica", size = 1.2, na.rm = T, bg.r = 0.2) +
  scale_x_continuous(breaks = seq(0,80,10), expand = expansion(mult = 0), limits = c(0,50)) +
  scale_y_reverse(expand = expansion(mult = c(0, 0.04), add = c(0.2, 0)), limits = c(4.1,0)) +
  scale_fill_manual(values = c(alpha(color_SB[1], 0.4),alpha(color_SB[4], 0.4),alpha('grey60', 0.4),alpha(color_SB[4], 0.4),alpha(color_SB[1], 0.4))) +
  labs(title = paste0('2020 Expected PPR Fantasy Points per Game as of Week ', my_week),
       subtitle = 'Grey represents middle 50% of outcomes, Orange tails are each 10% of outcomes  |  Caret shows actual avg  |  Based on 10,000 Simulations',
       x = NULL) +
  theme_SB +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text = element_text(size = 4),
    axis.line = element_line(color = 'darkblue', size = 0.5),
    panel.border = element_rect(color = 'grey95', size = 0.1),
    axis.ticks.length = unit(0.15, 'lines'),
    axis.ticks = element_line(color = 'darkblue', size = 0.5),
    panel.grid.minor.x = element_line('grey95', size = 0.2),
    panel.spacing.x = unit(0.5, 'lines')
  ) +
  annotation_custom(make_gradient(deg = 270), ymin=Inf, ymax=-0.1, xmin=-Inf, xmax=Inf) +
  annotation_custom(make_gradient(deg = 0), ymin=-Inf, ymax=Inf, xmin=48, xmax=Inf)

brand_plot(p, asp = 16/9, save_name = 'xfp_PPR_box_2020.png', data_home = 'Data: @nflfastR', tm_wordmarks = T)