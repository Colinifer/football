# NOTES
# 
# Adjust color scale automatically based on points and columns

library(tidyverse)

# source('init.R')

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
if (exists("pbp_df") == FALSE) {
  pbp_df <- readRDS(url(glue('https://github.com/guga31bb/nflfastR-data/blob/master/data/play_by_play_{year}.rds?raw=true')))
}

my_week <- pbp_df %>% select(week) %>% max()

pbp_df %>% 
  filter(week >= my_week - 4 & posteam == 'BUF' & pass_attempt==1 & season_type=='REG' & two_point_attempt==0 & !is.na(receiver_id)) %>% 
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
    receiver_id = unique(receiver_id),
    games = sum(game_played, na.rm = T),
    targets = sum(target, na.rm = T),
    targets_pg = targets / games,
    catches = sum(actual_outcome, na.rm = T),
    catches_pg = catches / games,
    yards = sum(ifelse(actual_outcome==1, gain, 0), na.rm = T),
    yards_pg = yards / games,
    air_yards = sum(ifelse(actual_outcome==1, air_yards, 0), na.rm = T),
    air_yards_pg = air_yards / games,
    td = sum(ifelse(gain==yardline_100, actual_outcome, 0), na.rm = T),
    td_pg = td / games,
    PPR_pts = sum(actual_PPR_points, na.rm = T),
    PPR_pts_pg = PPR_pts / games,
    half_PPR_pts = sum(actual_half_PPR_points, na.rm = T),
    half_PPR_pts_pg = half_PPR_pts / games,
    exp_catches = sum(ifelse(target==1, cp, NA), na.rm = T),
    exp_catches_pg = exp_catches / games,
    exp_yards = sum(exp_yards, na.rm = T),
    exp_yards_pg = exp_yards / games,
    exp_td = sum(ifelse(gain==yardline_100, catch_run_prob, 0), na.rm = T),
    exp_td_pg = exp_td / games,
    exp_td_pg = exp_td / games,
    exp_PPR_pts = sum(exp_PPR_points, na.rm = T),
    exp_PPR_pts_pg = exp_PPR_pts / games,
    exp_half_PPR_pts = sum(exp_half_PPR_points, na.rm = T),
    exp_half_PPR_pts_pg = exp_half_PPR_pts / games,
  ) %>% 
  mutate(
    half_PPR_pts_diff = half_PPR_pts - exp_half_PPR_pts,
    half_PPR_pts_pg_diff = half_PPR_pts_pg - exp_half_PPR_pts_pg,
    PPR_pts_diff = PPR_pts - exp_PPR_pts,
    PPR_pts_pg_diff = PPR_pts_pg - exp_PPR_pts_pg
  ) %>% 
  ungroup %>% 
  left_join(
    sleeper_players_df %>%
      select(position, sportradar_id, gsis_id, espn_id, headshot_url, injury_status),
    by = c("receiver_id" = "gsis_id")
  ) %>%
  left_join(espn_players_df %>%
              select(id, status, team_id = onTeamId, injured),
            by = c("espn_id" = "id")) %>% 
  # filter(injured != TRUE & (status != 'ONTEAM' | team_id == 8)) %>%
  filter(injured != TRUE) %>%
  select(
    games,
    receiver,
    posteam,
    targets,
    catches,
    yards,
    td,
    half_PPR_pts_pg,
    exp_catches,
    exp_yards,
    exp_td,
    exp_half_PPR_pts_pg,
    half_PPR_pts_pg_diff,
    status,
    team_id
  ) %>%
  arrange(-half_PPR_pts_pg) %>% 
  dplyr::slice(1:50) %>% 
  mutate(Rank = paste0('#',row_number())) %>%
  select(-status, team_id) %>%
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
    half_PPR_pts_pg = 'FP',
    exp_catches = 'Rec',
    exp_yards = 'Yds',
    exp_td = 'TD',
    exp_half_PPR_pts_pg = 'FP',
    half_PPR_pts_pg_diff = "Pts Diff."
  ) %>% 
  fmt_number(columns = vars(exp_td, half_PPR_pts_pg, exp_half_PPR_pts_pg, half_PPR_pts_pg_diff), decimals = 1) %>% 
  fmt_number(columns = vars(yards, exp_yards, exp_catches), decimals = 0, sep_mark = ',') %>% 
  tab_style(style = cell_text(size = 'x-large'), locations = cells_title(groups = 'title')) %>% 
  tab_style(style = cell_text(align = 'center', size = 'medium'), locations = cells_body()) %>% 
  tab_style(style = cell_text(align = 'left'), locations = cells_body(vars(receiver))) %>% 
  tab_spanner(label = 'Actual', columns = vars(catches, yards, td, half_PPR_pts_pg)) %>% 
  tab_spanner(label = 'Expected', columns = vars(exp_catches, exp_yards, exp_td, exp_half_PPR_pts_pg)) %>% 
  tab_source_note(source_note = '') %>% 
  data_color(
    columns = vars(half_PPR_pts_pg),
    colors = scales::col_numeric(
      palette = c(color_cw[6], color_cw[2]),
      domain = c(
        avg_exp_fp_df$half_PPR_pts_pg %>% max(),
        avg_exp_fp_df$half_PPR_pts_pg %>% min()
      ),
      reverse = TRUE
    ),
    # anyway to automate?
    autocolor_text = FALSE
  ) %>%
  data_color(
    columns = vars(exp_half_PPR_pts_pg),
    colors = scales::col_numeric(
      palette = c(color_cw[6], color_cw[2]),
      domain = c(
        avg_exp_fp_df$exp_half_PPR_pts_pg %>% max(),
        avg_exp_fp_df$exp_half_PPR_pts_pg %>% min()
      ),
      reverse = TRUE
    ),
    # need to adjust for full PPR point scale
    autocolor_text = FALSE
  ) %>%
  data_color(
    columns = vars(half_PPR_pts_pg_diff),
    colors = scales::col_numeric(
      palette = c(color_cw[6], color_cw[2]),
      domain = c(
        avg_exp_fp_df$half_PPR_pts_pg_diff %>% max(),
        avg_exp_fp_df$half_PPR_pts_pg_diff %>% min()
      ),
      reverse = TRUE
    ),
    autocolor_text = FALSE
  ) %>% 
  text_transform(
    locations = cells_body(vars(posteam)),
    fn = function(x) web_image(url = paste0('https://a.espncdn.com/i/teamlogos/nfl/500/',x,'.png'))
  ) %>% 
  cols_width(vars(posteam) ~ px(45)) %>% 
  tab_options(
    table.font.color = color_cw[5],
    data_row.padding = '2px',
    row_group.padding = '3px',
    column_labels.border.bottom.color = color_cw[5],
    column_labels.border.bottom.width = 1.4,
    table_body.border.top.color = color_cw[5],
    row_group.border.top.width = 1.5,
    row_group.border.top.color = 'transparent',
    table_body.border.bottom.width = 0.7,
    table_body.border.bottom.color = 'transparent',
    row_group.border.bottom.width = 1,
    row_group.border.bottom.color = color_cw[5],
    table.border.top.color = 'transparent',
    table.background.color = color_cw[1],
    table_body.hlines.color = color_cw[3],
    table.border.bottom.color = 'transparent',
    row.striping.background_color = color_cw[2],
    row.striping.include_table_body = TRUE
    # ) %>% 
    # gtsave(filename = paste0("xFP_share_half_ppr_", pbp_df$season[1], ".png"), path = "fantasy_football/plots")
  )
