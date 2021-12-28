# Data --------------------------------------------------------------------
map(current_season,function(x){
  print(x)
  
  con <- fx.db_con(x.host = 'localhost')
  
  rushers <-
    # readRDS(url(glue('https://github.com/guga31bb/nflfastR-data/blob/master/data/play_by_play_{year}.rds?raw=true')))
    tbl(con, 'nflfastR_pbp') |> 
    filter(season == x) |> 
    collect() |> 
    calculate_player_stats_mod() |> 
    left_join(
      roster_df |> 
        select(gsis_id, position, headshot_url),
      by = c('player_id' = 'gsis_id')
    ) |> 
    filter(position == 'RB') |> 
    select(player_id, player_name, position, games, recent_team, carries, hvt, contains('rushing'), contains('receiving'), contains('fantasy'), headshot_url) |> 
    arrange(-rushing_yards)
  
  # filter(pass_attempts > mean(cayoe$pass_attempts)-(mean(cayoe$pass_attempts)*.6))
  
  my_week <- fx.n_week(pbp_df)
  
  
  summary(rushers$carries)
  
  rushers_filtered <- rushers |> 
    filter(carries >= summary(rushers$carries)[4])
  
  # xFP RB table
  rushers_filtered |>
    select(
      games,
      headshot_url,
      player_name,
      recent_team,
      carries,
      hvt,
      rushing_tds,
      rushing_yards,
      receiving_air_yards,
      receiving_tds,
      fantasy_points,
      fantasy_points_half_ppr,
      fantasy_points_ppr
    ) |>
    arrange(-hvt) |> 
    dplyr::slice(1:50) |> 
    mutate(Rank = glue('# {row_number()}')) |>
    gt() |>
    tab_header(title = glue('High Value Touches {x}'), 
               subtitle = glue('Through week {my_week} | Min. {summary(rushers$carries)[4] |> round()} carries')) |> 
    cols_move_to_start(columns = c(Rank)) |> 
    cols_label(
      games = 'GP',
      headshot_url = '',
      player_name = '',
      recent_team = '',
      carries = 'Carries',
      hvt = 'HVT',
      rushing_tds = 'TDs',
      rushing_yards = 'Yards',
      receiving_air_yards = 'Air Yards',
      receiving_tds = 'TDs',
      fantasy_points = 'Points',
      fantasy_points_half_ppr = '.5 PPR',
      fantasy_points_ppr = 'PPR'
    ) |> 
    tab_style(style = cell_text(font = 'Chivo', size = 'xx-large', weight = 'bold'), locations = cells_title(groups = 'title')) |> 
    tab_style(style = cell_text(font = 'Chivo', size = 'large', weight = 'normal'), locations = cells_title(groups = 'subtitle')) |> 
    tab_style(style = cell_text(align = 'center', size = 'medium'), locations = cells_body()) |> 
    tab_style(style = cell_text(align = 'left'), locations = cells_body(c(player_name))) |> 
    tab_style(
      style = cell_borders(
        sides = 'left',
        color = color_cw[5],
        weight = px(3)
      ),
      locations = list(
        cells_body(
          columns = c(5,9,11)
        )
      )
    ) |> 
    tab_style(
      style = cell_text(font = 'Chivo', weight = 'bold'),
      locations = cells_body(
        columns = c(Rank, player_name)
      )
    ) |> 
    tab_style(
      style = cell_text(font = 'Montserrat'),
      locations = cells_body(
        columns = c(5:13)
      )
    ) |> 
    tab_spanner(label = 'Rushing', columns = c(carries, hvt, rushing_tds, rushing_yards)) |> 
    tab_spanner(label = 'Receiving', columns = c(receiving_air_yards, receiving_tds)) |> 
    tab_spanner(label = 'Fantasy', columns = c(fantasy_points, fantasy_points_half_ppr, fantasy_points_ppr)) |> 
    tab_source_note(source_note = 'Chart: Colin Welsh | Data: @nflfastR') |> 
    data_color(
      columns = c(hvt),
      colors = scales::col_quantile(palette = c(color_cw[8], color_cw[2], color_cw[6]), domain = c(max(rushers$hvt), mean(rushers$hvt), min(rushers$hvt))),
      autocolor_text = FALSE
    ) |> 
    data_color(
      columns = c(rushing_yards),
      colors = scales::col_quantile(palette = c(color_cw[8], color_cw[2], color_cw[6]), domain = c(max(rushers_filtered$rushing_yards), mean(rushers_filtered$rushing_yards), min(rushers_filtered$rushing_yards))),
      autocolor_text = FALSE
    ) |> 
    data_color(
      columns = c(receiving_air_yards),
      colors = scales::col_quantile(palette = c(color_cw[8], color_cw[2], color_cw[6]), domain = c(max(rushers_filtered$receiving_air_yards), 0, min(rushers$receiving_air_yards))),
      autocolor_text = FALSE
    ) |> 
    data_color(
      columns = c(fantasy_points),
      colors = scales::col_quantile(palette = c(color_cw[8], color_cw[2], color_cw[6]), domain = c(max(rushers_filtered$fantasy_points), mean(rushers_filtered$fantasy_points), min(rushers_filtered$fantasy_points))),
      autocolor_text = FALSE
    ) |> 
    data_color(
      columns = c(fantasy_points_half_ppr),
      colors = scales::col_quantile(palette = c(color_cw[8], color_cw[2], color_cw[6]), domain = c(max(rushers_filtered$fantasy_points_half_ppr), mean(rushers_filtered$fantasy_points_half_ppr), min(rushers_filtered$fantasy_points_half_ppr))),
      autocolor_text = FALSE
    ) |> 
    data_color(
      columns = c(fantasy_points_ppr),
      colors = scales::col_quantile(palette = c(color_cw[8], color_cw[2], color_cw[6]), domain = c(max(rushers_filtered$fantasy_points_ppr), mean(rushers_filtered$fantasy_points_ppr), min(rushers_filtered$fantasy_points_ppr))),
      autocolor_text = FALSE
    ) |> 
    text_transform(
      locations = cells_body(c(headshot_url)),
      fn = function(x) web_image(url = x)
    ) |> 
    text_transform(
      locations = cells_body(c(recent_team)),
      fn = function(x) web_image(url = glue('https://a.espncdn.com/i/teamlogos/nfl/500/{x}.png'))
    ) |> 
    cols_width(c(recent_team) ~ px(45)) |> 
    tab_options(
      table.font.color = color_cw[5],
      data_row.padding = '2px',
      row_group.padding = '3px',
      column_labels.border.bottom.color = color_cw[5],
      column_labels.border.bottom.width = 1.4,
      column_labels.font.weight = 'bold',
      table_body.border.top.color = color_cw[5],
      row_group.border.top.width = 1.5,
      row_group.border.top.color = '#999999',
      table_body.border.bottom.width = 0.7,
      table_body.border.bottom.color = '#999999',
      row_group.border.bottom.width = 1,
      row_group.border.bottom.color = color_cw[5],
      # table.border.top.color = 'transparent',
      table.background.color = color_cw[1],
      # table.border.bottom.color = 'transparent',
      row.striping.background_color = color_cw[2],
      row.striping.include_table_body = TRUE
    ) |> 
    opt_table_font(
      font = list(
        google_font('Chivo'),
        default_fonts()
      )
    ) |> 
    gtsave(filename = glue('rb_hvt_{x}.png'), path = 'plots/desktop')
  
  dbDisconnect(con)
})
# rm(list = ls())