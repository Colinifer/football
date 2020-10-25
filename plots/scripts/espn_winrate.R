library(tidyverse)
library(espnscrapeR)
library(nflfastR)
library(teamcolors)
library(gt)
library(webshot)

source('init.R')


pbp_df <- readRDS(url(glue('https://github.com/guga31bb/nflfastR-data/blob/master/data/play_by_play_2020.rds?raw=true')))
n_week <- pbp_df %>% select(week) %>% max()
rm(pbp_df)

all_win_rate <- scrape_espn_win_rate()

wide_win_rate <- all_win_rate %>%
  pivot_wider(names_from = stat,
              values_from = win_pct,
              id_col = team) %>%
  purrr::set_names(nm = c("team", "prwr", "rswr", "pbwr", "rbwr")) %>%
  mutate(prwr_rk = min_rank(desc(prwr)), .before = prwr) %>%
  mutate(rswr_rk = min_rank(desc(rswr)), .before = rswr) %>%
  mutate(pbwr_rk = min_rank(desc(pbwr)), .before = pbwr) %>%
  mutate(rbwr_rk = min_rank(desc(rbwr)), .before = rbwr) %>%
  mutate(def_wr_comb_rk = (prwr_rk + rswr_rk) / 2) %>%
  mutate(off_wr_comb_rk = (rbwr_rk + pbwr_rk) / 2) %>%
  mutate(total_wr_comb_rk = (prwr_rk + rswr_rk + rbwr_rk + pbwr_rk) / 4) %>% 
  left_join(teams_colors_logos %>% filter(team_abbr != "LAR") %>%
              select(team_abbr, team_name),
            by = c("team" = "team_name"))

wide_win_rate <- 
  wide_win_rate %>% 
  select(team, team_abbr, prwr_rk, prwr, rswr_rk, rswr, def_wr_comb_rk, pbwr_rk, pbwr, rbwr_rk, rbwr, off_wr_comb_rk, total_wr_comb_rk)

wide_win_rate %>% 
  select(!team_abbr) %>% 
  summarize(across(!contains("rk"), mean)) %>% 
  mutate(team = "NFL")


# Total -------------------------------------------------------------------

gt_tab <- wide_win_rate %>% 
  arrange(total_wr_comb_rk) %>% 
  gt(rowname_col = "team") %>%
  grand_summary_rows(
    columns = c(4,6,9,11),
    fns = list(`NFL AVERAGE` = ~mean(.)),
    formatter = fmt_percent,
    decimals = 0
  ) %>% 
  data_color( # Normal rank columns, scale 1:32
    columns = c(3,5,8,10),
    colors = scales::col_numeric(
      # palette = c("#7fbf7b", "#f7f7f7", "#af8dc3")
      # palette = c("#ff7f00", "#f7f7f7"),
      palette = c(color_cw[6], color_cw[2], color_cw[8]),
      domain = c(1, 32)
    )
  ) %>%
  data_color( # Def Combined rank column, scale lowest to highest
    columns = c(7),
    colors = scales::col_numeric(
      # palette = c("#7fbf7b", "#f7f7f7", "#af8dc3")
      # palette = c("#ff7f00", "#f7f7f7"),
      palette = c(color_cw[6], color_cw[2], color_cw[8]),
      domain = c(wide_win_rate$def_wr_comb_rk %>% min(), wide_win_rate$def_wr_comb_rk %>% max())
    )
  ) %>%
  data_color( # Off Combined rank column, scale lowest to highest
    columns = c(12),
    colors = scales::col_numeric(
      # palette = c("#7fbf7b", "#f7f7f7", "#af8dc3")
      # palette = c("#ff7f00", "#f7f7f7"),
      palette = c(color_cw[6], color_cw[2], color_cw[8]),
      domain = c(wide_win_rate$off_wr_comb_rk %>% min(), wide_win_rate$off_wr_comb_rk %>% max())
    )
  ) %>%
  data_color( # Total Combined rank column, scale lowest to highest
    columns = c(13),
    colors = scales::col_numeric(
      # palette = c("#7fbf7b", "#f7f7f7", "#af8dc3")
      # palette = c("#ff7f00", "#f7f7f7"),
      palette = c(color_cw[6], color_cw[2], color_cw[8]),
      domain = c(wide_win_rate$total_wr_comb_rk %>% min(), wide_win_rate$total_wr_comb_rk %>% max())
    )
  ) %>%
  text_transform(
    locations = cells_body(vars(team_abbr)),
    fn = function(x) web_image(url = glue('https://a.espncdn.com/i/teamlogos/nfl/500/{x}.png'))
  ) %>% 
  cols_label(
    team_abbr = "",
    prwr_rk = "RK",
    prwr = "Pass Rush",
    pbwr_rk = "RK",
    pbwr = "Pass Block",
    rswr_rk = "RK",
    rswr = "Run Stop",
    rbwr_rk = "RK",
    rbwr = "Run Block",
    off_wr_comb_rk = "Off. Combined RK",
    def_wr_comb_rk = "Def. Combined RK",
    total_wr_comb_rk = "Total Combined RK"
  ) %>% 
  opt_all_caps() %>% 
  cols_width(
    vars(team) ~ px(225),
    columns = contains("r_rk") ~ px(50),
    columns = contains("comb") ~ px(100),
    vars(team_abbr) ~ px(50),
    everything() ~ px(80)
  ) %>% 
  tab_style(
    style = cell_borders(
      sides = "left",
      color = color_cw[5],
      weight = px(3)
    ),
    locations = list(
      cells_body(
        columns = c(3,5,7,8,10,12,13)
      ),
      cells_grand_summary(
        columns = 3
      )
    )
  ) %>% 
  # tab_style(
  #   style = cell_text(
  #     weight = "bold",
  #     color = color_cw[3]
  #   ),
  #   locations = cells_body(
  #     vars(team_abbr, team)
  #   )
  # ) %>%
  tab_spanner(
    label = "DEFENSE",
    columns = 3:7
  ) %>% 
  tab_spanner(
    label = "OFFENSE",
    columns = 8:12
  ) %>% 
  tab_source_note(
    source_note = md("**Chart:** Colin Welsh | **Data:** ESPN")
  ) %>% 
  fmt_percent(columns = c(4, 6, 9, 11), decimals = 0) %>% 
  cols_align(align = "center", columns = 2:ncol(wide_win_rate)) %>% 
  cols_align(align = "left", columns = c(1,2)) %>% 
  tab_header(
    title = "2020 NFL pass-rushing, run-stopping, blocking leaderboard: Win rate rankings",
    subtitle = glue("Data through week {n_week}")
  ) %>% 
  tab_style(
    style = cell_borders(
      sides = "bottom", 
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
    heading.align = "left",
    # table.border.bottom.color = "transparent",
    table.border.bottom.width = px(3),
    table.border.top.color = "transparent",
    # table.border.top.width = px(3),
    column_labels.border.bottom.color = color_cw[5],
    column_labels.border.bottom.width = px(3),
    heading.title.font.size = px(30),
    heading.title.font.weight = "bold",
    heading.subtitle.font.size = px(20),
    heading.border.bottom.color = "transparent",
    heading.border.bottom.width = px(3),
    table.background.color = color_cw[1],
    table_body.hlines.color = color_cw[3],
    grand_summary_row.border.color = color_cw[3],
    grand_summary_row.border.width = px(3),
    row.striping.background_color = color_cw[2],
    row.striping.include_table_body = TRUE
  ) %>% 
  opt_table_font(font = google_font(name = "Chivo"))

# gt_tab

gt::gtsave(gt_tab, filename = "espn_winrate_total.png", path = "plots/desktop/", zoom = 1, vwidth = 1500)


# Offense -----------------------------------------------------------------

gt_tab <- wide_win_rate %>% 
  arrange(off_wr_comb_rk) %>% 
  gt(rowname_col = "team") %>%
  grand_summary_rows(
    columns = c(4,6,9,11),
    fns = list(`NFL AVERAGE` = ~mean(.)),
    formatter = fmt_percent,
    decimals = 0
  ) %>% 
  data_color( # Normal rank columns, scale 1:32
    columns = c(3,5,8,10),
    colors = scales::col_numeric(
      # palette = c("#7fbf7b", "#f7f7f7", "#af8dc3")
      # palette = c("#ff7f00", "#f7f7f7"),
      palette = c(color_cw[6], color_cw[2], color_cw[8]),
      domain = c(1, 32)
    )
  ) %>%
  data_color( # Def Combined rank column, scale lowest to highest
    columns = c(7),
    colors = scales::col_numeric(
      # palette = c("#7fbf7b", "#f7f7f7", "#af8dc3")
      # palette = c("#ff7f00", "#f7f7f7"),
      palette = c(color_cw[6], color_cw[2], color_cw[8]),
      domain = c(wide_win_rate$def_wr_comb_rk %>% min(), wide_win_rate$def_wr_comb_rk %>% max())
    )
  ) %>%
  data_color( # Off Combined rank column, scale lowest to highest
    columns = c(12),
    colors = scales::col_numeric(
      # palette = c("#7fbf7b", "#f7f7f7", "#af8dc3")
      # palette = c("#ff7f00", "#f7f7f7"),
      palette = c(color_cw[6], color_cw[2], color_cw[8]),
      domain = c(wide_win_rate$off_wr_comb_rk %>% min(), wide_win_rate$off_wr_comb_rk %>% max())
    )
  ) %>%
  data_color( # Total Combined rank column, scale lowest to highest
    columns = c(13),
    colors = scales::col_numeric(
      # palette = c("#7fbf7b", "#f7f7f7", "#af8dc3")
      # palette = c("#ff7f00", "#f7f7f7"),
      palette = c(color_cw[6], color_cw[2], color_cw[8]),
      domain = c(wide_win_rate$total_wr_comb_rk %>% min(), wide_win_rate$total_wr_comb_rk %>% max())
    )
  ) %>%
  text_transform(
    locations = cells_body(vars(team_abbr)),
    fn = function(x) web_image(url = glue('https://a.espncdn.com/i/teamlogos/nfl/500/{x}.png'))
  ) %>% 
  cols_label(
    team_abbr = "",
    prwr_rk = "RK",
    prwr = "Pass Rush",
    pbwr_rk = "RK",
    pbwr = "Pass Block",
    rswr_rk = "RK",
    rswr = "Run Stop",
    rbwr_rk = "RK",
    rbwr = "Run Block",
    off_wr_comb_rk = "Off. Combined RK",
    def_wr_comb_rk = "Def. Combined RK",
    total_wr_comb_rk = "Total Combined RK"
  ) %>% 
  opt_all_caps() %>% 
  cols_width(
    vars(team) ~ px(225),
    columns = contains("r_rk") ~ px(50),
    columns = contains("comb") ~ px(100),
    vars(team_abbr) ~ px(50),
    everything() ~ px(80)
  ) %>% 
  tab_style(
    style = cell_borders(
      sides = "left",
      color = color_cw[5],
      weight = px(3)
    ),
    locations = list(
      cells_body(
        columns = c(3,5,7,8,10,12,13)
      ),
      cells_grand_summary(
        columns = 3
      )
    )
  ) %>% 
  # tab_style(
  #   style = cell_text(
  #     weight = "bold",
  #     color = color_cw[3]
  #   ),
  #   locations = cells_body(
  #     vars(team_abbr, team)
  #   )
  # ) %>%
  tab_spanner(
    label = "DEFENSE",
    columns = 3:7
  ) %>% 
  tab_spanner(
    label = "OFFENSE",
    columns = 8:12
  ) %>% 
  tab_source_note(
    source_note = md("**Chart:** Colin Welsh | **Data:** ESPN")
  ) %>% 
  fmt_percent(columns = c(4, 6, 9, 11), decimals = 0) %>% 
  cols_align(align = "center", columns = 2:ncol(wide_win_rate)) %>% 
  cols_align(align = "left", columns = c(1,2)) %>% 
  tab_header(
    title = "2020 NFL pass-rushing, run-stopping, blocking leaderboard: Win rate rankings",
    subtitle = glue("Data through week {n_week}")
  ) %>% 
  tab_style(
    style = cell_borders(
      sides = "bottom", 
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
    heading.align = "left",
    # table.border.bottom.color = "transparent",
    table.border.bottom.width = px(3),
    table.border.top.color = "transparent",
    # table.border.top.width = px(3),
    column_labels.border.bottom.color = color_cw[5],
    column_labels.border.bottom.width = px(3),
    heading.title.font.size = px(30),
    heading.title.font.weight = "bold",
    heading.subtitle.font.size = px(20),
    heading.border.bottom.color = "transparent",
    heading.border.bottom.width = px(3),
    table.background.color = color_cw[1],
    table_body.hlines.color = color_cw[3],
    grand_summary_row.border.color = color_cw[3],
    grand_summary_row.border.width = px(3),
    row.striping.background_color = color_cw[2],
    row.striping.include_table_body = TRUE
  ) %>% 
  opt_table_font(font = google_font(name = "Chivo"))

# gt_tab

gt::gtsave(gt_tab, filename = "espn_winrate_off.png", path = "plots/desktop/", zoom = 1, vwidth = 1500)

# Defense -----------------------------------------------------------------

gt_tab <- wide_win_rate %>% 
  arrange(def_wr_comb_rk) %>% 
  # gt() %>% 
  gt(rowname_col = "team") %>%
  grand_summary_rows(
    columns = c(4,6,9,11),
    fns = list(`NFL AVERAGE` = ~mean(.)),
    formatter = fmt_percent,
    decimals = 0
  ) %>% 
  data_color( # Normal rank columns, scale 1:32
    columns = c(3,5,8,10),
    colors = scales::col_numeric(
      # palette = c("#7fbf7b", "#f7f7f7", "#af8dc3")
      # palette = c("#ff7f00", "#f7f7f7"),
      palette = c(color_cw[6], color_cw[2], color_cw[8]),
      domain = c(1, 32)
    )
  ) %>%
  data_color( # Def Combined rank column, scale lowest to highest
    columns = c(7),
    colors = scales::col_numeric(
      # palette = c("#7fbf7b", "#f7f7f7", "#af8dc3")
      # palette = c("#ff7f00", "#f7f7f7"),
      palette = c(color_cw[6], color_cw[2], color_cw[8]),
      domain = c(wide_win_rate$def_wr_comb_rk %>% min(), wide_win_rate$def_wr_comb_rk %>% max())
    )
  ) %>%
  data_color( # Off Combined rank column, scale lowest to highest
    columns = c(12),
    colors = scales::col_numeric(
      # palette = c("#7fbf7b", "#f7f7f7", "#af8dc3")
      # palette = c("#ff7f00", "#f7f7f7"),
      palette = c(color_cw[6], color_cw[2], color_cw[8]),
      domain = c(wide_win_rate$off_wr_comb_rk %>% min(), wide_win_rate$off_wr_comb_rk %>% max())
    )
  ) %>%
  data_color( # Total Combined rank column, scale lowest to highest
    columns = c(13),
    colors = scales::col_numeric(
      # palette = c("#7fbf7b", "#f7f7f7", "#af8dc3")
      # palette = c("#ff7f00", "#f7f7f7"),
      palette = c(color_cw[6], color_cw[2], color_cw[8]),
      domain = c(wide_win_rate$total_wr_comb_rk %>% min(), wide_win_rate$total_wr_comb_rk %>% max())
    )
  ) %>%
  text_transform(
    locations = cells_body(vars(team_abbr)),
    fn = function(x) web_image(url = glue('https://a.espncdn.com/i/teamlogos/nfl/500/{x}.png'))
  ) %>% 
  cols_label(
    team_abbr = "",
    prwr_rk = "RK",
    prwr = "Pass Rush",
    pbwr_rk = "RK",
    pbwr = "Pass Block",
    rswr_rk = "RK",
    rswr = "Run Stop",
    rbwr_rk = "RK",
    rbwr = "Run Block",
    off_wr_comb_rk = "Off. Combined RK",
    def_wr_comb_rk = "Def. Combined RK",
    total_wr_comb_rk = "Total Combined RK"
  ) %>% 
  opt_all_caps() %>% 
  cols_width(
    vars(team) ~ px(225),
    columns = contains("r_rk") ~ px(50),
    columns = contains("comb") ~ px(100),
    vars(team_abbr) ~ px(50),
    everything() ~ px(80)
  ) %>% 
  tab_style(
    style = cell_borders(
      sides = "left",
      color = color_cw[5],
      weight = px(3)
    ),
    locations = list(
      cells_body(
        columns = c(3,5,7,8,10,12,13)
      ),
      cells_grand_summary(
        columns = 3
      )
    )
  ) %>% 
  # tab_style(
  #   style = cell_text(
  #     weight = "bold",
  #     color = color_cw[3]
  #   ),
  #   locations = cells_body(
  #     vars(team_abbr, team)
  #   )
  # ) %>%
  tab_spanner(
    label = "DEFENSE",
    columns = 3:7
  ) %>% 
  tab_spanner(
    label = "OFFENSE",
    columns = 8:12
  ) %>% 
  tab_source_note(
    source_note = md("**Chart:** Colin Welsh | **Data:** ESPN")
  ) %>% 
  fmt_percent(columns = c(4, 6, 9, 11), decimals = 0) %>% 
  cols_align(align = "center", columns = 2:ncol(wide_win_rate)) %>% 
  cols_align(align = "left", columns = c(1,2)) %>% 
  tab_header(
    title = "2020 NFL pass-rushing, run-stopping, blocking leaderboard: Win rate rankings",
    subtitle = glue("Data through week {n_week}")
  ) %>% 
  tab_style(
    style = cell_borders(
      sides = "bottom", 
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
    heading.align = "left",
    # table.border.bottom.color = "transparent",
    table.border.bottom.width = px(3),
    table.border.top.color = "transparent",
    # table.border.top.width = px(3),
    column_labels.border.bottom.color = color_cw[5],
    column_labels.border.bottom.width = px(3),
    heading.title.font.size = px(30),
    heading.title.font.weight = "bold",
    heading.subtitle.font.size = px(20),
    heading.border.bottom.color = "transparent",
    heading.border.bottom.width = px(3),
    table.background.color = color_cw[1],
    table_body.hlines.color = color_cw[3],
    grand_summary_row.border.color = color_cw[3],
    grand_summary_row.border.width = px(3),
    row.striping.background_color = color_cw[2],
    row.striping.include_table_body = TRUE
  ) %>% 
  opt_table_font(font = google_font(name = "Chivo"))

# gt_tab

gt::gtsave(gt_tab, filename = "espn_winrate_def.png", path = "plots/desktop/", zoom = 1, vwidth = 1500)

# Test --------------------------------------------------------------------

# proj_df <- scrape_fpi(stat = "PROJ")
# 
# fpi_df %>% 
#   left_join(proj_df) %>% 
#   ggplot(aes(x = off, y = playoff_pct, size = def)) +
#   geom_point()
