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

if (exists("pbp_df") == F) {
  pbp_df <- readRDS(url(glue('https://github.com/guga31bb/nflfastR-data/blob/master/data/play_by_play_{year}.rds?raw=true')))
}

my_week <- pbp_df$week %>% max()


# Completed Air Yards Over Expected
cayoe_xyac <- pbp_df %>%
  filter(season == year & 
           pass_attempt == 1 &
           season_type == 'REG' &
           two_point_attempt == 0 & 
           !is.na(receiver_id) &
           !is.na(air_yards) &
           !is.na(complete_pass) &
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
    receiver,
    receiver_player_id,
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
    comp_air_yards = ifelse(complete_pass == 1, air_yards, 0),
    yac_prob = ifelse(yardline_100 == air_yards, 1, yac_prob),
    # PPR_points = 1 + gain / 10 + ifelse(gain == yardline_100, 6, 0),
    # half_PPR_points = .5 + gain / 10 + ifelse(gain == yardline_100, 6, 0),
    catch_run_prob = cp * yac_prob,
    # exp_PPR_points = PPR_points * catch_run_prob,
    # exp_half_PPR_points = half_PPR_points * catch_run_prob,
    exp_yards = gain * catch_run_prob,
    exp_air_yards = cp * air_yards,
    actual_outcome = ifelse(actual_yards_gained == gain &
                              complete_pass == 1, 1, 0),
    # actual_PPR_points = ifelse(actual_outcome == 1, PPR_points, 0),
    # actual_half_PPR_points = ifelse(actual_outcome == 1, half_PPR_points, 0),
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
    season = unique(season),
    games = sum(game_played, na.rm = T),
    pass_attempts = sum(attempt, na.rm = T),
    completions = sum(actual_outcome, na.rm = T),
    comp_air_yards = sum(ifelse(actual_outcome == 1, comp_air_yards, 0), na.rm = T),
    yards = sum(ifelse(actual_outcome == 1, gain, 0), na.rm = T),
    td = sum(ifelse(gain == yardline_100, actual_outcome, 0), na.rm = T),
    # PPR_pts = sum(actual_PPR_points, na.rm = T),
    # half_PPR_pts = sum(actual_half_PPR_points, na.rm = T),
    exp_completions = sum(ifelse(attempt == 1, cp, NA), na.rm = T),
    exp_air_yards = sum(ifelse(attempt == 1, exp_air_yards, NA), na.rm = T),
    exp_yards = sum(exp_yards, na.rm = T),
    exp_td = sum(ifelse(gain == yardline_100, catch_run_prob, 0), na.rm = T),
    # exp_PPR_pts = sum(exp_PPR_points, na.rm = T),
    # exp_half_PPR_pts = sum(exp_half_PPR_points, na.rm = T),
    sum_cayoe = sum(comp_air_yards-exp_air_yards, na.rm = T),
  ) %>%
  mutate(
    # half_ppr_pts_diff = half_PPR_pts - exp_half_PPR_pts,
    # ppr_pts_diff = PPR_pts - exp_PPR_pts,
    cayoe_a = sum_cayoe / pass_attempts
  ) %>%
  ungroup
  # filter(pass_attempts > mean(cayoe$pass_attempts)-(mean(cayoe$pass_attempts)*.6))

summary(cayoe$pass_attempts)

cayoe_filtered <- cayoe %>% 
  filter(pass_attempts >= ifelse(summary(pass_attempts)[4]>75, 75, summary(pass_attempts)[4]))

# xFP QB table
cayoe_filtered %>%
  select(
    games,
    passer,
    posteam,
    pass_attempts,
    completions,
    comp_air_yards,
    td,
    # PPR_pts,
    exp_completions,
    exp_air_yards,
    exp_td,
    # exp_PPR_pts,
    # ppr_pts_diff,
    sum_cayoe,
    cayoe_a
  ) %>%
  arrange(-cayoe_a) %>% 
  dplyr::slice(1:50) %>% 
  mutate(Rank = paste0('#',row_number())) %>%
  gt() %>%
  tab_header(title = paste('Completed Air Yards Over Expected (CAYOE),', cayoe_filtered$season[1]), 
             subtitle = paste('Through week', my_week, 'MNF', '|', 'Min.', ifelse(round(summary(cayoe_filtered$pass_attempts)[4])>75,75,round(summary(cayoe_filtered$pass_attempts)[4])),'pass attempts > 0 air yards')) %>% 
  cols_move_to_start(columns = vars(Rank)) %>% 
  cols_label(
    games = 'GP',
    passer = '',
    posteam = '',
    pass_attempts = 'PA',
    completions = 'Comp.',
    comp_air_yards = 'Air Yds',
    td = 'TD',
    # PPR_pts = 'FP',
    exp_completions = 'xComp.',
    exp_air_yards = 'xAir Yds',
    exp_td = 'xTD',
    # exp_PPR_pts = 'xFP',
    # ppr_pts_diff = "Pts Diff.",
    sum_cayoe = "Total CAYOE",
    cayoe_a = "Avg. CAYOE"
  ) %>% 
  fmt_number(columns = vars(sum_cayoe, cayoe_a), decimals = 2) %>% 
  fmt_number(columns = vars(exp_td), decimals = 1) %>% 
  fmt_number(columns = vars(comp_air_yards, exp_air_yards, exp_completions), decimals = 0, sep_mark = ',') %>% 
  tab_style(style = cell_text(font = "Chivo", size = 'x-large', weight = 'bold'), locations = cells_title(groups = 'title')) %>% 
  tab_style(style = cell_text(align = 'center', size = 'medium'), locations = cells_body()) %>% 
  tab_style(style = cell_text(align = 'left'), locations = cells_body(vars(passer))) %>% 
  tab_style(
    style = cell_borders(
      sides = "left",
      color = color_cw[5],
      weight = px(3)
    ),
    locations = list(
      cells_body(
        columns = c(4,8,11,12)
      )
    )
  ) %>% 
  tab_style(
    style = cell_text(font = "Chivo", weight = "bold"),
    locations = cells_body(
      columns = vars(Rank, passer)
    )
  ) %>% 
  tab_style(
    style = cell_text(font = "Montserrat"),
    locations = cells_body(
      columns = c(4:12)
    )
  ) %>% 
  tab_spanner(label = 'Actual', columns = vars(completions, comp_air_yards, td)) %>% 
  tab_spanner(label = 'Expected', columns = vars(exp_completions, exp_air_yards, exp_td)) %>% 
  tab_source_note(source_note = 'Chart: Colin Welsh | Data: @nflfastR') %>% 
  data_color(
    columns = vars(sum_cayoe),
    colors = scales::col_numeric(palette = c(color_cw[2], color_cw[6]), domain = c(max(cayoe_filtered$sum_cayoe), min(cayoe_filtered$sum_cayoe))),
    autocolor_text = FALSE
  ) %>% 
  data_color(
    columns = vars(cayoe_a),
    colors = scales::col_numeric(palette = c(color_cw[2], color_cw[6]), domain = c(max(cayoe_filtered$cayoe_a), min(cayoe_filtered$cayoe_a))),
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
    column_labels.font.weight = "bold",
    table_body.border.top.color = color_cw[5],
    row_group.border.top.width = 1.5,
    row_group.border.top.color = '#999999',
    table_body.border.bottom.width = 0.7,
    table_body.border.bottom.color = '#999999',
    row_group.border.bottom.width = 1,
    row_group.border.bottom.color = color_cw[5],
    table.border.top.color = 'transparent',
    table.background.color = color_cw[1],
    table.border.bottom.color = 'transparent',
    row.striping.background_color = color_cw[2],
    row.striping.include_table_body = TRUE
  ) %>% 
  opt_table_font(
    font = list(
      google_font("Chivo"),
      default_fonts()
    )
  ) %>% 
  gtsave(filename = paste0("qb_cayoe_", cayoe_filtered$season[1], ".png"), path = "plots/desktop")

# rm(list = ls())