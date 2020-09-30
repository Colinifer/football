library(tidyverse)
library(espnscrapeR)
library(gt)

all_win_rate <- scrape_espn_win_rate()

wide_win_rate <- all_win_rate %>%
  pivot_wider(names_from = stat,
              values_from = win_pct,
              id_col = team) %>%
  set_names(nm = c("team", "prwr", "rswr", "pbwr", "rbwr")) %>%
  mutate(prwr_rk = min_rank(desc(prwr)), .before = prwr) %>%
  mutate(rswr_rk = min_rank(desc(rswr)), .before = rswr) %>%
  mutate(pbwr_rk = min_rank(desc(pbwr)), .before = pbwr) %>%
  mutate(rbwr_rk = min_rank(desc(rbwr)), .before = rbwr) %>%
  mutate(def_wr_comp_rk = (prwr_rk + rswr_rk) / 2) %>%
  mutate(off_wr_comp_rk = (rbwr_rk + pbwr_rk) / 2) %>%
  left_join(teams_colors_logos %>%
              select(team_abbr, team_name),
            by = c("team" = "team_name"))

wide_win_rate <- 
  wide_win_rate %>% 
  select(team_abbr, team, prwr_rk, prwr, rswr_rk, rswr, def_wr_comp_rk, pbwr_rk, pbwr, rbwr_rk, rbwr, off_wr_comp_rk)

wide_win_rate %>% 
  summarize(across(!contains("rk"), mean)) %>% 
  mutate(team = "NFL")

gt_tab <- wide_win_rate %>% 
  arrange(def_wr_comp_rk) %>% 
  gt(rowname_col = "team") %>%
  grand_summary_rows(
    columns = c(4,6,9,11),
    fns = list(`NFL AVERAGE` = ~mean(.)),
    formatter = fmt_percent,
    decimals = 0
  ) %>% 
  data_color(
    columns = contains("rk"),
    colors = scales::col_numeric(
      # palette = c("#7fbf7b", "#f7f7f7", "#af8dc3")
      palette = c("#ff7f00", "#f7f7f7"),
      domain = c(1, 32)
    )
  ) %>%
  text_transform(
    locations = cells_body(vars(team_abbr)),
    fn = function(x) web_image(url = paste0('https://a.espncdn.com/i/teamlogos/nfl/500/',x,'.png'))
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
    off_wr_comp_rk = "Off. Composite RK",
    def_wr_comp_rk = "Def. Composite RK"
  ) %>% 
  opt_all_caps() %>% 
  cols_width(
    vars(team) ~ px(225),
    columns = contains("r_rk") ~ px(50),
    columns = contains("comp") ~ px(130),
    everything() ~ px(100)
  ) %>% 
  tab_style(
    style = cell_borders(
      sides = "left",
      color = "darkblue",
      weight = px(3)
    ),
    locations = list(
      cells_body(
        columns = c(3,5,7,8,9,11,12)
      ),
      cells_grand_summary(
        columns = 3
      )
    )
  ) %>% 
  tab_style(
    style = cell_text(
      weight = "bold",
      color = "dimgray"
    ),
    locations = cells_body(
      vars(team_abbr, team)
    )
  ) %>%
  tab_spanner(
    label = "DEFENSE",
    columns = 3:7
  ) %>% 
  tab_spanner(
    label = "OFFENSE",
    columns = 8:12
  ) %>% 
  tab_source_note(
    source_note = md("**Data:** ESPN")
  ) %>% 
  fmt_percent(columns = c(4, 6, 9, 11), decimals = 0) %>% 
  cols_align(align = "center", columns = 2:ncol(wide_win_rate)) %>% 
  cols_align(align = "left", columns = c(1,2)) %>% 
  tab_header(
    title = "2020 NFL pass-rushing, run-stopping, blocking leaderboard: Win rate rankings",
    subtitle = "Data through week 3"
  ) %>% 
  tab_style(
    style = cell_borders(
      sides = "bottom", 
      color = "darkblue", 
      weight = px(3)
    ),
    locations = list(
      cells_body(
        rows = nrow(.$`_data`)
      )
    )
  ) %>% 
  tab_options(
    table.font.color = 'darkblue',
    heading.align = "left",
    table.border.bottom.color = "transparent",
    table.border.bottom.width = px(3),
    table.border.top.color = "transparent",
    table.border.top.width = px(3),
    column_labels.border.bottom.color = "darkblue",
    column_labels.border.bottom.width = px(3),
    heading.title.font.size = px(30),
    heading.title.font.weight = "bold",
    heading.subtitle.font.size = px(20),
    heading.border.bottom.color = "transparent",
    heading.border.bottom.width = px(3),
    table_body.hlines.color = "#ededed",
    grand_summary_row.border.color = "darkblue",
    grand_summary_row.border.width = px(3)
  ) %>% 
  opt_table_font(font = google_font(name = "Chivo"))

gt_tab

gtsave(gt_tab, path = "plots/desktop/", filename = "gt-espn-winrate.png")

proj_df <- scrape_fpi(stat = "PROJ")

fpi_df %>% 
  left_join(proj_df) %>% 
  ggplot(aes(x = off, y = playoff_pct, size = def)) +
  geom_point()
