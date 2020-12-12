library(tidyverse)
library(espnscrapeR)
library(nflfastR)
library(teamcolors)
library(gt)
library(webshot)

# source('init.R')


# pbp_df <- readRDS(url(glue('https://github.com/guga31bb/nflfastR-data/blob/master/data/play_by_play_2020.rds?raw=true')))
# n_week <- pbp_df %>% select(week) %>% max()
# rm(pbp_df)

n_week <- fx.n_week(full_pbp_df)

all_win_rate <- scrape_espn_win_rate()

wide_win_rate <- all_win_rate %>%
  pivot_wider(names_from = stat,
              values_from = win_pct,
              id_col = team) %>%
  purrr::set_names(nm = c('team', 'prwr', 'rswr', 'pbwr', 'rbwr')) %>%
  mutate(prwr = glue('.{prwr}') %>% as.double(),
         rswr = glue('.{rswr}') %>% as.double(),
         pbwr = glue('.{pbwr}') %>% as.double(),
         rbwr = glue('.{rbwr}') %>% as.double(),
         ) %>% 
  mutate(prwr_rk = min_rank(desc(prwr)), .before = prwr) %>%
  mutate(rswr_rk = min_rank(desc(rswr)), .before = rswr) %>%
  mutate(pbwr_rk = min_rank(desc(pbwr)), .before = pbwr) %>%
  mutate(rbwr_rk = min_rank(desc(rbwr)), .before = rbwr) %>%
  mutate(def_wr_comb_rk = (prwr_rk + rswr_rk) / 2) %>%
  mutate(off_wr_comb_rk = (rbwr_rk + pbwr_rk) / 2) %>%
  mutate(total_wr_comb_rk = (prwr_rk + rswr_rk + rbwr_rk + pbwr_rk) / 4) %>% 
  left_join(teams_colors_logos %>% filter(team_abbr != 'LAR') %>%
              select(team_abbr, team_name),
            by = c('team' = 'team_name'))

wide_win_rate <- 
  wide_win_rate %>% 
  select(team, team_abbr, prwr_rk, prwr, rswr_rk, rswr, def_wr_comb_rk, pbwr_rk, pbwr, rbwr_rk, rbwr, off_wr_comb_rk, total_wr_comb_rk)

wide_win_rate %>% 
  select(!team_abbr) %>% 
  summarize(across(!contains('rk'), mean)) %>% 
  mutate(team = 'NFL')


matchup_winrate_df <- wide_win_rate %>% 
  left_join(
    matchup_df %>% 
      filter(week == n_week + 1) %>% 
      select(posteam, oppteam, weekday, gametime),
    by = c('team_abbr' = 'posteam')
    ) %>% 
  left_join(
    wide_win_rate %>%
      select(
        -team
      ) %>% 
      rename(
        'opp_prwr_rk' = 'prwr_rk',
        'opp_prwr' = 'prwr',
        'opp_rswr_rk' = 'rswr_rk',
        'opp_rswr' = 'rswr',
        'opp_def_wr_comb_rk' = 'def_wr_comb_rk',
        'opp_pbwr_rk' = 'pbwr_rk',
        'opp_pbwr' = 'pbwr',
        'opp_rbwr_rk' = 'rbwr_rk',
        'opp_rbwr' = 'rbwr',
        'opp_off_wr_comb_rk' = 'off_wr_comb_rk',
        'opp_total_wr_comb_rk' = 'total_wr_comb_rk'
      ),
    by = c('oppteam' = 'team_abbr')
  ) %>% 
  mutate(
    matchup_pr = prwr - opp_pbwr,
    matchup_rs = rswr - opp_rbwr,
    matchup_def = (matchup_pr + matchup_rs) / 2,
    matchup_pb = pbwr - opp_prwr,
    matchup_rb = rbwr - opp_rswr,
    matchup_off = (matchup_pb + matchup_rb) / 2,
  ) %>% 
  arrange(-matchup_def) %>% 
  filter(!is.na(oppteam))

matchup_df_adv <- wide_win_rate %>% 
  left_join(
    matchup_df %>% 
      filter(week == n_week + 1) %>% 
      select(posteam, oppteam, weekday, gametime),
    by = c('team_abbr' = 'posteam')
  ) %>% 
  left_join(
    wide_win_rate %>%
      select(
        -team
      ) %>% 
      rename(
        'opp_prwr_rk' = 'prwr_rk',
        'opp_prwr' = 'prwr',
        'opp_rswr_rk' = 'rswr_rk',
        'opp_rswr' = 'rswr',
        'opp_def_wr_comb_rk' = 'def_wr_comb_rk',
        'opp_pbwr_rk' = 'pbwr_rk',
        'opp_pbwr' = 'pbwr',
        'opp_rbwr_rk' = 'rbwr_rk',
        'opp_rbwr' = 'rbwr',
        'opp_off_wr_comb_rk' = 'off_wr_comb_rk',
        'opp_total_wr_comb_rk' = 'total_wr_comb_rk'
      ),
    by = c('oppteam' = 'team_abbr')
  ) %>% 
  mutate(
    matchup_pr = prwr - opp_pbwr,
    matchup_rs = rswr - opp_rbwr,
    matchup_def = (matchup_pr + matchup_rs) / 2,
    matchup_pb = pbwr - opp_prwr,
    matchup_rb = rbwr - opp_rswr,
    matchup_off = (matchup_pb + matchup_rb) / 2,
  ) %>% 
  arrange(-matchup_def) %>% 
  filter(!is.na(oppteam))

gtdf <- matchup_df_adv %>% 
  arrange(-matchup_def) %>% 
  select(
    -weekday,
    -gametime,
    -opp_prwr_rk,
    -opp_prwr,
    -opp_rswr_rk,
    -opp_rswr,
    -opp_def_wr_comb_rk,
    -opp_pbwr_rk,
    -opp_pbwr,
    -opp_rbwr_rk,
    -opp_rbwr,
    -opp_off_wr_comb_rk,
    -opp_total_wr_comb_rk
  )


# Total -------------------------------------------------------------------

# Notes: Removed Grand Summary info which contains all NFL average rows

# Sort by defense matchup
gt_def_tab <- gtdf %>% 
  select(-matchup_pr, -matchup_rs, -matchup_pb, -matchup_rb) %>% 
  # gt(rowname_col = 'team') %>%
  gt() %>%
  # grand_summary_rows(
  #   columns = c(4,6,9,11),
  #   fns = list(`NFL AVERAGE` = ~mean(.)),
  #   formatter = fmt_percent,
  #   decimals = 0
  # ) %>%
  data_color( # Normal rank columns, scale 1:32
    columns = c(3,5,8,10),
    colors = scales::col_quantile(
      # palette = c('#7fbf7b', '#f7f7f7', '#af8dc3')
      # palette = c('#ff7f00', '#f7f7f7'),
      palette = c(color_cw[6], color_cw[2], color_cw[8]),
      domain = c(1, 32)
    )
  ) %>%
  data_color( # Def Combined rank column, scale lowest to highest
    columns = c(7),
    colors = scales::col_quantile(
      # palette = c('#7fbf7b', '#f7f7f7', '#af8dc3')
      # palette = c('#ff7f00', '#f7f7f7'),
      palette = c(color_cw[6], color_cw[2], color_cw[8]),
      domain = c(gtdf$def_wr_comb_rk %>% min(), gtdf$def_wr_comb_rk %>% max())
    )
  ) %>%
  data_color( # Off Combined rank column, scale lowest to highest
    columns = c(12),
    colors = scales::col_quantile(
      # palette = c('#7fbf7b', '#f7f7f7', '#af8dc3')
      # palette = c('#ff7f00', '#f7f7f7'),
      palette = c(color_cw[6], color_cw[2], color_cw[8]),
      domain = c(gtdf$off_wr_comb_rk %>% min(), gtdf$off_wr_comb_rk %>% max())
    )
  ) %>%
  data_color( # Total Combined rank column, scale lowest to highest
    columns = c(13),
    colors = scales::col_quantile(
      # palette = c('#7fbf7b', '#f7f7f7', '#af8dc3')
      # palette = c('#ff7f00', '#f7f7f7'),
      palette = c(color_cw[6], color_cw[2], color_cw[8]),
      domain = c(gtdf$total_wr_comb_rk %>% min(), gtdf$total_wr_comb_rk %>% max())
    )
  ) %>%
  data_color( # Total Combined rank column, scale lowest to highest
    columns = c(15),
    colors = scales::col_quantile(
      # palette = c('#7fbf7b', '#f7f7f7', '#af8dc3')
      # palette = c('#ff7f00', '#f7f7f7'),
      palette = c(color_cw[6], color_cw[2], color_cw[8]),
      domain = c(gtdf$matchup_def %>% min(), gtdf$matchup_def %>% max()),
      reverse = TRUE
    )
  ) %>%
  data_color( # Total Combined rank column, scale lowest to highest
    columns = c(16),
    colors = scales::col_quantile(
      # palette = c('#7fbf7b', '#f7f7f7', '#af8dc3')
      # palette = c('#ff7f00', '#f7f7f7'),
      palette = c(color_cw[6], color_cw[2], color_cw[8]),
      domain = c(gtdf$matchup_off %>% min(), gtdf$matchup_off %>% max()),
      reverse = TRUE
    )
  ) %>%
  text_transform(
    locations = cells_body(vars(team_abbr, oppteam)),
    fn = function(x) web_image(url = glue('https://a.espncdn.com/i/teamlogos/nfl/500/{x}.png'), height = 50)
  ) %>% 
  cols_label(
    team_abbr = '',
    prwr_rk = 'RK',
    prwr = 'Pass Rush',
    pbwr_rk = 'RK',
    pbwr = 'Pass Block',
    rswr_rk = 'RK',
    rswr = 'Run Stop',
    rbwr_rk = 'RK',
    rbwr = 'Run Block',
    off_wr_comb_rk = 'Off. Combined RK',
    def_wr_comb_rk = 'Def. Combined RK',
    total_wr_comb_rk = 'Total Combined RK',
    oppteam = "Opponent",
    # matchup_pr = 'Pass Rush',
    # matchup_rs = 'Run Stop',
    matchup_def = 'Def. Total',
    # matchup_pb = 'Pass Block',
    # matchup_rb = 'Run Block',
    matchup_off = 'Off. Total'
  ) %>% 
  opt_all_caps() %>% 
  cols_width(
    vars(team) ~ px(264),
    columns = contains('r_rk') ~ px(50),
    columns = contains('comb') ~ px(100),
    vars(team_abbr) ~ px(75),
    everything() ~ px(80)
  ) %>% 
  tab_style(
    style = cell_borders(
      sides = 'left',
      color = color_cw[5],
      weight = px(3)
    ),
    locations = list(
      cells_body(
        columns = c(3,7,8,12,13)
      # ),
      # cells_grand_summary(
      #   columns = 3
      )
    )
  ) %>%
  fmt_percent(
    columns = c(4, 6, 9, 11), 
    decimals = 0
  ) %>% 
  # tab_style(
  #   style = cell_text(
  #     weight = 'bold',
  #     color = color_cw[3]
  #   ),
  #   locations = cells_body(
  #     vars(team_abbr, team)
  #   )
  # ) %>%
  tab_spanner(
    label = 'DEFENSE',
    columns = 3:7
  ) %>% 
  tab_spanner(
    label = 'OFFENSE',
    columns = 8:12
  ) %>% 
  tab_spanner(
    label = 'MATCHUP DIFFERENCE',
    columns = 14:16
  ) %>% 
  tab_source_note(
    source_note = md('**Chart:** Colin Welsh | **Data:** ESPN')
  ) %>% 
  cols_align(align = 'center', columns = 2:16) %>% 
  cols_align(align = 'left', columns = c(1,2)) %>% 
  tab_header(
    title = '2020 NFL pass-rushing, run-stopping, blocking leaderboard: Win rate rankings',
    subtitle = glue('Data through week {n_week}')
  ) %>% 
  tab_style(
    style = cell_borders(
      sides = 'bottom', 
      color = color_cw[5], 
      weight = px(3)
    ),
    locations = list(
      cells_body(
        rows = nrow(.$`_data`)
      )
    )
  ) %>% 
  tab_options(
    table.font.color = color_cw[5],
    table.font.names = "Monsterrat",
    # table.border.bottom.color = 'transparent',
    table.border.bottom.width = px(3),
    table.border.top.color = 'transparent',
    # table.border.top.width = px(3),
    heading.align = 'left',
    column_labels.border.bottom.color = color_cw[5],
    column_labels.border.bottom.width = px(3),
    heading.title.font.size = px(30),
    heading.title.font.weight = 'bold',
    heading.subtitle.font.size = px(20),
    heading.border.bottom.color = 'transparent',
    heading.border.bottom.width = px(3),
    table.background.color = color_cw[1],
    table_body.hlines.color = color_cw[3],
    grand_summary_row.border.color = color_cw[3],
    grand_summary_row.border.width = px(3),
    row.striping.background_color = color_cw[2],
    row.striping.include_table_body = TRUE
  ) %>% 
  opt_table_font(font = google_font(name = 'Chivo'))

# gt_tab

gt::gtsave(gt_def_tab, filename = 'espn_winrate_def_matchup.png', path = 'plots/desktop/', zoom = 1, vwidth = 100000)


# Sort by offense matchup
gt_off_tab <- gtdf %>% 
  select(-matchup_pr, -matchup_rs, -matchup_pb, -matchup_rb) %>% 
  arrange(-matchup_off) %>% 
  # gt(rowname_col = 'team') %>%
  gt() %>%
  # grand_summary_rows(
  #   columns = c(4,6,9,11),
  #   fns = list(`NFL AVERAGE` = ~mean(.)),
  #   formatter = fmt_percent,
  #   decimals = 0
  # ) %>%
  data_color( # Normal rank columns, scale 1:32
    columns = c(3,5,8,10),
    colors = scales::col_quantile(
      # palette = c('#7fbf7b', '#f7f7f7', '#af8dc3')
      # palette = c('#ff7f00', '#f7f7f7'),
      palette = c(color_cw[6], color_cw[2], color_cw[8]),
      domain = c(1, 32)
    )
  ) %>%
  data_color( # Def Combined rank column, scale lowest to highest
    columns = c(7),
    colors = scales::col_quantile(
      # palette = c('#7fbf7b', '#f7f7f7', '#af8dc3')
      # palette = c('#ff7f00', '#f7f7f7'),
      palette = c(color_cw[6], color_cw[2], color_cw[8]),
      domain = c(gtdf$def_wr_comb_rk %>% min(), gtdf$def_wr_comb_rk %>% max())
    )
  ) %>%
  data_color( # Off Combined rank column, scale lowest to highest
    columns = c(12),
    colors = scales::col_quantile(
      # palette = c('#7fbf7b', '#f7f7f7', '#af8dc3')
      # palette = c('#ff7f00', '#f7f7f7'),
      palette = c(color_cw[6], color_cw[2], color_cw[8]),
      domain = c(gtdf$off_wr_comb_rk %>% min(), gtdf$off_wr_comb_rk %>% max())
    )
  ) %>%
  data_color( # Total Combined rank column, scale lowest to highest
    columns = c(13),
    colors = scales::col_quantile(
      # palette = c('#7fbf7b', '#f7f7f7', '#af8dc3')
      # palette = c('#ff7f00', '#f7f7f7'),
      palette = c(color_cw[6], color_cw[2], color_cw[8]),
      domain = c(gtdf$total_wr_comb_rk %>% min(), gtdf$total_wr_comb_rk %>% max())
    )
  ) %>%
  data_color( # Total Combined rank column, scale lowest to highest
    columns = c(15),
    colors = scales::col_quantile(
      # palette = c('#7fbf7b', '#f7f7f7', '#af8dc3')
      # palette = c('#ff7f00', '#f7f7f7'),
      palette = c(color_cw[6], color_cw[2], color_cw[8]),
      domain = c(gtdf$matchup_def %>% min(), gtdf$matchup_def %>% max()),
      reverse = TRUE
    )
  ) %>%
  data_color( # Total Combined rank column, scale lowest to highest
    columns = c(16),
    colors = scales::col_quantile(
      # palette = c('#7fbf7b', '#f7f7f7', '#af8dc3')
      # palette = c('#ff7f00', '#f7f7f7'),
      palette = c(color_cw[6], color_cw[2], color_cw[8]),
      domain = c(gtdf$matchup_off %>% min(), gtdf$matchup_off %>% max()),
      reverse = TRUE
    )
  ) %>%
  text_transform(
    locations = cells_body(vars(team_abbr, oppteam)),
    fn = function(x) web_image(url = glue('https://a.espncdn.com/i/teamlogos/nfl/500/{x}.png'), height = 50)
  ) %>% 
  cols_label(
    team_abbr = '',
    prwr_rk = 'RK',
    prwr = 'Pass Rush',
    pbwr_rk = 'RK',
    pbwr = 'Pass Block',
    rswr_rk = 'RK',
    rswr = 'Run Stop',
    rbwr_rk = 'RK',
    rbwr = 'Run Block',
    off_wr_comb_rk = 'Off. Combined RK',
    def_wr_comb_rk = 'Def. Combined RK',
    total_wr_comb_rk = 'Total Combined RK',
    oppteam = "Opponent",
    # matchup_pr = 'Pass Rush',
    # matchup_rs = 'Run Stop',
    matchup_def = 'Def. Total',
    # matchup_pb = 'Pass Block',
    # matchup_rb = 'Run Block',
    matchup_off = 'Off. Total'
  ) %>% 
  opt_all_caps() %>% 
  cols_width(
    vars(team) ~ px(264),
    columns = contains('r_rk') ~ px(50),
    columns = contains('comb') ~ px(100),
    vars(team_abbr) ~ px(75),
    everything() ~ px(80)
  ) %>% 
  tab_style(
    style = cell_borders(
      sides = 'left',
      color = color_cw[5],
      weight = px(3)
    ),
    locations = list(
      cells_body(
        columns = c(3,7,8,12,13)
        # ),
        # cells_grand_summary(
        #   columns = 3
      )
    )
  ) %>% 
  fmt_percent(
    columns = c(4, 6, 9, 11), 
    decimals = 0
  ) %>% 
  # tab_style(
  #   style = cell_text(
  #     weight = 'bold',
  #     color = color_cw[3]
  #   ),
  #   locations = cells_body(
  #     vars(team_abbr, team)
  #   )
  # ) %>%
  tab_spanner(
    label = 'DEFENSE',
    columns = 3:7
  ) %>% 
  tab_spanner(
    label = 'OFFENSE',
    columns = 8:12
  ) %>% 
  tab_spanner(
    label = 'MATCHUP DIFFERENCE',
    columns = 14:16
  ) %>% 
  tab_source_note(
    source_note = md('**Chart:** Colin Welsh | **Data:** ESPN')
  ) %>% 
  cols_align(align = 'center', columns = 2:16) %>% 
  cols_align(align = 'left', columns = c(1,2)) %>% 
  tab_header(
    title = '2020 NFL pass-rushing, run-stopping, blocking leaderboard: Win rate rankings',
    subtitle = glue('Data through week {n_week}')
  ) %>% 
  tab_style(
    style = cell_borders(
      sides = 'bottom', 
      color = color_cw[5], 
      weight = px(3)
    ),
    locations = list(
      cells_body(
        rows = nrow(.$`_data`)
      )
    )
  ) %>% 
  tab_options(
    table.font.color = color_cw[5],
    table.font.names = "Monsterrat",
    # table.border.bottom.color = 'transparent',
    table.border.bottom.width = px(3),
    table.border.top.color = 'transparent',
    # table.border.top.width = px(3),
    heading.align = 'left',
    column_labels.border.bottom.color = color_cw[5],
    column_labels.border.bottom.width = px(3),
    heading.title.font.size = px(30),
    heading.title.font.weight = 'bold',
    heading.subtitle.font.size = px(20),
    heading.border.bottom.color = 'transparent',
    heading.border.bottom.width = px(3),
    table.background.color = color_cw[1],
    table_body.hlines.color = color_cw[3],
    grand_summary_row.border.color = color_cw[3],
    grand_summary_row.border.width = px(3),
    row.striping.background_color = color_cw[2],
    row.striping.include_table_body = TRUE
  ) %>% 
  opt_table_font(font = google_font(name = 'Chivo'))

# gt_tab

gt::gtsave(gt_off_tab, filename = 'espn_winrate_off_matchup.png', path = 'plots/desktop/', zoom = 1, vwidth = 100000)

# Win Rate Scatter Plot ---------------------------------------------------

# Team info

# Defense Pass Rush / Run Stop (up and right is good)
p <- matchup_df_adv %>% 
  mutate(team_logo_espn = glue('https://a.espncdn.com/i/teamlogos/nfl/500/{team_abbr}.png')) %>% 
  ggplot(aes(x = prwr, y = rswr)) +
  geom_image(aes(image = team_logo_espn), size = 0.05, asp = 4/3) +
  geom_hline(yintercept = mean(matchup_df_adv$rswr), color = "red", linetype = "dashed") +
  geom_vline(xintercept =  mean(matchup_df_adv$prwr), color = "red", linetype = "dashed") +
  labs(x = "Pass Rush Win Rate %",
       y = "Run Stop Win Rate %",
       # caption = "Data: @nflscrapR",
       title = glue("{year} NFL Defensive Line Win Rate"),
       subtitle = glue("Through week {n_week}")) +
  # geom_abline(slope= 1, intercept=0, alpha=.2) +
  theme_cw +
  theme(
    axis.title.y = element_text(angle = 90),
    legend.position = c(0.99, 0.99),
    legend.justification = c(1, 1) ,
    plot.title = element_text(size = 16),
    #panel.grid.minor = element_blank()
  )

brand_plot(p, asp = 16/12, save_name = glue('plots/desktop/defense_win_rate_{year}.png'), data_home = 'Data: ESPN', fade_borders = '')

# Offense Pass Block / Run Block (up and right is good)
p <- matchup_df_adv %>% 
  mutate(team_logo_espn = glue('https://a.espncdn.com/i/teamlogos/nfl/500/{team_abbr}.png')) %>% 
  ggplot(aes(x = pbwr, y = rbwr)) +
  geom_image(aes(image = team_logo_espn), size = 0.05, asp = 4/3) +
  geom_hline(yintercept = mean(matchup_df_adv$rbwr), color = "red", linetype = "dashed") +
  geom_vline(xintercept =  mean(matchup_df_adv$pbwr), color = "red", linetype = "dashed") +
  labs(x = "Pass Block Win Rate %",
       y = "Run Block Win Rate %",
       # caption = "Data: @nflscrapR",
       title = glue("{year} NFL Offensive Line Win Rate"),
       subtitle = glue("Through week {n_week}")) +
  # geom_abline(slope= 1, intercept=0, alpha=.2) +
  theme_cw +
  theme(
    axis.title.y = element_text(angle = 90),
    legend.position = c(0.99, 0.99),
    legend.justification = c(1, 1) ,
    plot.title = element_text(size = 16),
    #panel.grid.minor = element_blank()
  )

brand_plot(p, asp = 16/12, save_name = glue('plots/desktop/offense_win_rate_{year}.png'), data_home = 'Data: ESPN', fade_borders = '')


# Opponent/Matchup info

# Pass Rush vs Opp. Pass Block (up and right is good)
p <- matchup_df_adv %>% 
  mutate(team_logo_espn = glue('https://a.espncdn.com/i/teamlogos/nfl/500/{team_abbr}.png')) %>% 
  ggplot(aes(x = prwr, y = opp_pbwr)) +
  geom_image(aes(image = team_logo_espn), size = 0.05, asp = 4/3) +
  geom_hline(yintercept = mean(matchup_df_adv$opp_pbwr), color = "red", linetype = "dashed") +
  geom_vline(xintercept =  mean(matchup_df_adv$prwr), color = "red", linetype = "dashed") +
  scale_y_reverse() +
  labs(x = "Pass Rush Win Rate %",
       y = "Opp. Pass Block Win Rate %",
       # caption = "Data: @nflscrapR",
       title = glue("{year} Week {n_week + 1} OL/DL Passing Matchups"),
       subtitle = glue("Pass rush win rate vs. week {n_week + 1} opponent pass block win rate")) +
  # geom_abline(slope= 1, intercept=0, alpha=.2) +
  theme_cw +
  theme(
    axis.title.y = element_text(angle = 90),
    legend.position = c(0.99, 0.99),
    legend.justification = c(1, 1) ,
    plot.title = element_text(size = 16),
    #panel.grid.minor = element_blank()
  )

brand_plot(p, asp = 16/12, save_name = glue('plots/desktop/matchup_pass_rush_win_rate_{year}.png'), data_home = 'Data: ESPN', fade_borders = '')


# Run Stop vs Opp. Run Block (up and right is good)
p <- matchup_df_adv %>% 
  mutate(team_logo_espn = glue('https://a.espncdn.com/i/teamlogos/nfl/500/{team_abbr}.png')) %>% 
  ggplot(aes(x = rswr, y = opp_rbwr)) +
  geom_image(aes(image = team_logo_espn), size = 0.05, asp = 4/3) +
  geom_hline(yintercept = mean(matchup_df_adv$opp_rbwr), color = "red", linetype = "dashed") +
  geom_vline(xintercept =  mean(matchup_df_adv$rswr), color = "red", linetype = "dashed") +
  scale_y_reverse() +
  labs(x = "Run Stop Win Rate %",
       y = "Opp. Run Block Win Rate %",
       # caption = "Data: @nflscrapR",
       title = glue("{year} Week {n_week + 1} OL/DL Rushing Matchups"),
       subtitle = glue("Run stop win rate vs. week {n_week + 1} opponent run block win rate")) +
  # geom_abline(slope= 1, intercept=0, alpha=.2) +
  theme_cw +
  theme(
    axis.title.y = element_text(angle = 90),
    legend.position = c(0.99, 0.99),
    legend.justification = c(1, 1) ,
    plot.title = element_text(size = 16),
    #panel.grid.minor = element_blank()
  )

brand_plot(p, asp = 16/12, save_name = glue('plots/desktop/matchup_run_stop_win_rate_{year}.png'), data_home = 'Data: ESPN', fade_borders = '')

# Test --------------------------------------------------------------------

# proj_df <- scrape_fpi(stat = 'PROJ')
# 
# fpi_df %>% 
#   left_join(proj_df) %>% 
#   ggplot(aes(x = off, y = playoff_pct, size = def)) +
#   geom_point()
