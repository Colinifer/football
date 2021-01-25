# Data --------------------------------------------------------------------

# xFP QB table
cayoe_filtered %>%
  select(
    games,
    headshot_url,
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
  tab_header(title = glue('Completed Air Yards Over Expected (CAYOE) {current_season}'), 
             subtitle = glue('Through week {my_week} | Min. {summary(cayoe$pass_attempts)[4] %>% round()} > 0 air yards')) %>% 
  cols_move_to_start(columns = vars(Rank)) %>% 
  cols_label(
    games = 'GP',
    headshot_url = '',
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
    cayoe_a = html("CAYOE<br>(per Pass Attempt)")
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
        columns = c(5,9,12,13)
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
      columns = c(5:13)
    )
  ) %>% 
  tab_spanner(label = 'Actual', columns = vars(completions, comp_air_yards, td)) %>% 
  tab_spanner(label = 'Expected', columns = vars(exp_completions, exp_air_yards, exp_td)) %>% 
  tab_source_note(source_note = 'Chart: Colin Welsh | Data: @nflfastR') %>% 
  data_color(
    columns = vars(sum_cayoe),
    colors = scales::col_quantile(palette = c(color_cw[8], color_cw[2], color_cw[6]), domain = c(max(cayoe_filtered$sum_cayoe), 0, min(cayoe_filtered$sum_cayoe))),
    autocolor_text = FALSE
  ) %>% 
  data_color(
    columns = vars(cayoe_a),
    colors = scales::col_quantile(palette = c(color_cw[8], color_cw[2], color_cw[6]), domain = c(max(cayoe_filtered$cayoe_a), 0, min(cayoe_filtered$cayoe_a))),
    autocolor_text = FALSE
  ) %>% 
  text_transform(
    locations = cells_body(vars(headshot_url)),
    fn = function(x) web_image(url = x)
  ) %>% 
  text_transform(
    locations = cells_body(vars(posteam)),
    fn = function(x) web_image(url = glue('https://a.espncdn.com/i/teamlogos/nfl/500/{x}.png'))
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
  gtsave(filename = glue("qb_cayoe_{current_season}.png"), path = "plots/desktop")

# rm(list = ls())