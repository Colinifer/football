# Data --------------------------------------------------------------------

# pbp_df <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2020.rds'))
current_season <- year

pbp_df <- 
  # readRDS(url(glue('https://github.com/guga31bb/nflfastR-data/blob/master/data/play_by_play_{year}.rds?raw=true')))
  pbp_df %>% 
  add_xyac_mod() %>% 
  filter(
    season.x == current_season &
      pass_attempt == 1 &
      season_type == 'REG' &
      two_point_attempt == 0 &
      !is.na(receiver_id) &
      !is.na(air_yards.x) &
      !is.na(complete_pass) &
      # wp > .2 &
      # wp < .8 &
      air_yards.x > 0
  ) %>% 
  select(
    season = season.x,
    week = week.x,
    game_id,
    play_id,
    defteam = defteam,
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
  collect() %>% 
  mutate(defteam = ifelse(defteam == "LA", "LAR", defteam),
         posteam = ifelse(defteam == "LA", "LAR", defteam)) %>% 
  rename_at(.vars = vars(ends_with('.x')),
            .funs = funs(sub('[.]x$', '', .))) %>% 
  left_join(
    readRDS(glue('data/pbp/sportradar/sr_pbp_{current_season}.rds')) %>% 
      select(
        season = season.x,
        game_id,
        play_id,
        pass_defense_player_id = defPlayers.def_target
      ) %>%
      filter(
        !is.na(pass_defense_player_id)
      )
  )

cayoe <- pbp_df %>% 
  filter(!is.na(pass_defense_player_id)) %>% 
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
  group_by(game_id, pass_defense_player_id) %>%
  mutate(game_played = ifelse(row_number() == 1, 1, 0)) %>%
  ungroup %>%
  # group_by(game_id, play_id, passer) %>%
  # mutate(completion = ifelse(row_number() == 1, 1, 0)) %>%
  # ungroup %>%
  group_by(game_id, play_id, receiver) %>% 
  mutate(attempt = ifelse(row_number()==1,1,0)) %>% 
  ungroup %>% 
  group_by(defteam, pass_defense_player_id) %>%
  left_join(
    sleeper_players_df %>% 
      select(
        gsis_id,
        pass_defense_player_name = full_name),
    by = c('pass_defense_player_id' = 'gsis_id')
  ) %>% 
  # filter()
  summarize(
    pass_defense_player_name = first(pass_defense_player_name),
    season = unique(season),
    games = sum(game_played, na.rm = T),
    targets = sum(attempt, na.rm = T),
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
    cayoe_a = sum_cayoe / targets
  ) %>%
  ungroup
# filter(targets > mean(cayoe$targets)-(mean(cayoe$targets)*.6))

my_week <- fx.n_week(pbp_df)


summary(cayoe$targets)

cayoe_filtered <- cayoe %>% 
  filter(targets >= summary(targets)[4]) %>% 
  left_join(
    sleeper_players_df %>% 
      select(gsis_id,
             headshot_url,
             team,
             position
      ),
    by = c('pass_defense_player_id' = 'gsis_id',
           'defteam' = 'team')
  ) %>% 
  mutate(
    headshot_url = case_when(defteam == 'NYJ' & pass_defense_player_id == '00-0036152' ~ 'https://static.clubs.nfl.com/image/private/t_editorial_landscape_6_desktop/f_png/jets/senk0wm7brlw0wlcmcbj.png',
                             defteam == 'NYJ' & pass_defense_player_id == '00-0031331' ~ 'http://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/16948.png',
                             TRUE ~ headshot_url))

summary(cayoe_filtered$targets)

# xFP DB table
cayoe_filtered %>%
  select(
    games,
    headshot_url,
    pass_defense_player_name,
    position,
    defteam,
    targets,
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
  arrange(sum_cayoe) %>% 
  dplyr::slice(1:30) %>% 
  mutate(Rank = glue('# {row_number()}')) %>%
  gt() %>%
  tab_header(title = glue('Completed Air Yards Over Expected (CAYOE), {current_season}'), 
             subtitle = glue('Through week {my_week} | Min. {summary(cayoe$targets)[4] %>% round()} pass attempts > 0 air yards')) %>% 
  cols_move_to_start(columns = vars(Rank)) %>% 
  cols_label(
    games = 'GP',
    headshot_url = '',
    pass_defense_player_name = '',
    position = '',
    defteam = '',
    targets = 'Tgts',
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
    cayoe_a = html("CAYOE<br>(per Pass Attempt)")
  ) %>% 
  fmt_number(columns = vars(sum_cayoe, cayoe_a), decimals = 2) %>% 
  fmt_number(columns = vars(exp_td), decimals = 1) %>% 
  fmt_number(columns = vars(comp_air_yards, exp_air_yards, exp_completions), decimals = 0, sep_mark = ',') %>% 
  tab_style(style = cell_text(font = "Chivo", size = 'x-large', weight = 'bold'), locations = cells_title(groups = 'title')) %>% 
  tab_style(style = cell_text(align = 'center', size = 'medium'), locations = cells_body()) %>% 
  tab_style(style = cell_text(align = 'left'), locations = cells_body(vars(pass_defense_player_name))) %>% 
  tab_style(
    style = cell_borders(
      sides = "left",
      color = color_cw[5],
      weight = px(3)
    ),
    locations = list(
      cells_body(
        columns = c(6,10,13,14)
      )
    )
  ) %>% 
  tab_style(
    style = cell_text(font = "Chivo", weight = "bold"),
    locations = cells_body(
      columns = vars(Rank, pass_defense_player_name)
    )
  ) %>% 
  tab_style(
    style = cell_text(font = "Montserrat"),
    locations = cells_body(
      columns = c(6:14)
    )
  ) %>% 
  tab_spanner(label = 'Actual', columns = vars(completions, comp_air_yards, td)) %>% 
  tab_spanner(label = 'Expected', columns = vars(exp_completions, exp_air_yards, exp_td)) %>% 
  tab_source_note(source_note = 'Chart: Colin Welsh | Data: @nflfastR') %>% 
  data_color(
    columns = vars(sum_cayoe),
    colors = scales::col_quantile(palette = c(color_cw[6], color_cw[2], color_cw[8]), domain = c(max(cayoe_filtered$sum_cayoe), 0, min(cayoe_filtered$sum_cayoe))),
    autocolor_text = FALSE
  ) %>% 
  data_color(
    columns = vars(cayoe_a),
    colors = scales::col_quantile(palette = c(color_cw[6], color_cw[2], color_cw[8]), domain = c(max(cayoe_filtered$cayoe_a), 0, min(cayoe_filtered$cayoe_a))),
    autocolor_text = FALSE
  ) %>% 
  text_transform(
    locations = cells_body(vars(headshot_url)),
    fn = function(x) web_image(url = x)
  ) %>% 
  text_transform(
    locations = cells_body(vars(defteam)),
    fn = function(x) web_image(url = glue('https://a.espncdn.com/i/teamlogos/nfl/500/{x}.png'))
  ) %>% 
  cols_width(vars(defteam) ~ px(45)) %>% 
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
  gtsave(filename = glue("db_cayoe_top_{current_season}.png"), path = "plots/desktop")

cayoe_filtered %>%
  select(
    games,
    headshot_url,
    pass_defense_player_name,
    position,
    defteam,
    targets,
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
  arrange(-sum_cayoe) %>% 
  dplyr::slice(1:30) %>% 
  mutate(Rank = glue('# {row_number()}')) %>%
  gt() %>%
  tab_header(title = glue('Completed Air Yards Over Expected (CAYOE), {current_season}'), 
             subtitle = glue('Through week {my_week} | Min. {summary(cayoe$targets)[4] %>% round()} pass attempts > 0 air yards')) %>% 
  cols_move_to_start(columns = vars(Rank)) %>% 
  cols_label(
    games = 'GP',
    headshot_url = '',
    pass_defense_player_name = '',
    position = '',
    defteam = '',
    targets = 'Tgts',
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
    cayoe_a = html("CAYOE<br>(per Pass Attempt)")
  ) %>% 
  fmt_number(columns = vars(sum_cayoe, cayoe_a), decimals = 2) %>% 
  fmt_number(columns = vars(exp_td), decimals = 1) %>% 
  fmt_number(columns = vars(comp_air_yards, exp_air_yards, exp_completions), decimals = 0, sep_mark = ',') %>% 
  tab_style(style = cell_text(font = "Chivo", size = 'x-large', weight = 'bold'), locations = cells_title(groups = 'title')) %>% 
  tab_style(style = cell_text(align = 'center', size = 'medium'), locations = cells_body()) %>% 
  tab_style(style = cell_text(align = 'left'), locations = cells_body(vars(pass_defense_player_name))) %>% 
  tab_style(
    style = cell_borders(
      sides = "left",
      color = color_cw[5],
      weight = px(3)
    ),
    locations = list(
      cells_body(
        columns = c(6,10,13,14)
      )
    )
  ) %>% 
  tab_style(
    style = cell_text(font = "Chivo", weight = "bold"),
    locations = cells_body(
      columns = vars(Rank, pass_defense_player_name)
    )
  ) %>% 
  tab_style(
    style = cell_text(font = "Montserrat"),
    locations = cells_body(
      columns = c(6:14)
    )
  ) %>% 
  tab_spanner(label = 'Actual', columns = vars(completions, comp_air_yards, td)) %>% 
  tab_spanner(label = 'Expected', columns = vars(exp_completions, exp_air_yards, exp_td)) %>% 
  tab_source_note(source_note = 'Chart: Colin Welsh | Data: @nflfastR') %>% 
  data_color(
    columns = vars(sum_cayoe),
    colors = scales::col_quantile(palette = c(color_cw[6], color_cw[2], color_cw[8]), domain = c(max(cayoe_filtered$sum_cayoe), 0, min(cayoe_filtered$sum_cayoe))),
    autocolor_text = FALSE
  ) %>% 
  data_color(
    columns = vars(cayoe_a),
    colors = scales::col_quantile(palette = c(color_cw[6], color_cw[2], color_cw[8]), domain = c(max(cayoe_filtered$cayoe_a), 0, min(cayoe_filtered$cayoe_a))),
    autocolor_text = FALSE
  ) %>% 
  text_transform(
    locations = cells_body(vars(headshot_url)),
    fn = function(x) web_image(url = x)
  ) %>% 
  text_transform(
    locations = cells_body(vars(defteam)),
    fn = function(x) web_image(url = glue('https://a.espncdn.com/i/teamlogos/nfl/500/{x}.png'))
  ) %>% 
  cols_width(vars(defteam) ~ px(45)) %>% 
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
  gtsave(filename = glue("db_cayoe_bot_{current_season}.png"), path = "plots/desktop")

# rm(list = ls())