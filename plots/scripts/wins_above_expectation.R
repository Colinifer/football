# NOTES:

# Get avg drive length offense and defense, and success rate OR TD rate

library(tidyverse)

# lapply(1999:2019, function(x) {

# Parameter --------------------------------------------------------------------

current_season <- year
wp_limit <- 0.5

# Load the data ----------------------------------------------------------------
con <- fx.db_con(x.host = 'localhost')
pbp <- tbl(con, 'nflfastR_pbp') |> 
  filter(season >= current_season & 
           game_id != '2020_12_NO_DEN') |> # This game is pointless
  select(
    season,
    season_type,
    week,
    game_id,
    home_team,
    away_team,
    home_score,
    away_score,
    result,
    vegas_wp,
    posteam,
    defteam,
    rush,
    pass,
    play_type
  ) |> 
  collect() |> 
  decode_player_ids(fast = TRUE) |> 
  filter(season_type == 'REG') |> filter(!is.na(posteam) & (rush == 1 | pass == 1))

n_week <- fx.n_week(pbp)

# Compute outcomes and win percentage ------------------------------------------

outcomes <- pbp |>
  group_by(season, game_id, home_team) |>
  filter(row_number()==n()) |> 
  summarise(
    home_win = if_else(result > 0, 1, 0),
    home_tie = if_else(result == 0, 1, 0),
    home_score = first(home_score),
    home_result = first(home_score) - first(away_score)
  ) |>
  group_by(season, home_team) |>
  summarise(
    home_games = n(),
    home_wins = sum(home_win),
    home_ties = sum(home_tie),
    home_points = sum(home_score),
    home_result = sum(home_result)
  ) |>
  ungroup() |>
  full_join(
    # away games
    pbp |>
      group_by(season, game_id, away_team) |>
      filter(row_number()==n()) |> 
      summarise(
        away_win = if_else(result < 0, 1, 0),
        away_tie = if_else(result == 0, 1, 0),
        away_score = first(away_score),
        away_result = first(away_score) - first(home_score)
      ) |>
      group_by(season, away_team) |>
      summarise(
        away_games = n(),
        away_wins = sum(away_win),
        away_ties = sum(away_tie),
        away_points = sum(away_score),
        away_result = sum(away_result)
      ) |>
      ungroup(),
    by = c("season", "home_team" = "away_team")
  ) |>
  rename(team = "home_team") %>%
  replace(is.na(.), 0) |> 
  mutate(
    games = home_games + away_games,
    wins = home_wins + away_wins,
    losses = games - wins,
    ties = home_ties + away_ties,
    win_percentage = (wins + 0.5 * ties) / games,
    points_diff = home_result + away_result,
    points_for = home_points + away_points,
    points_against = points_for - points_diff,
    expo = ((points_for + points_against) / games) ^ 0.251, # From football perspective
    pyth = points_for ^ expo / (points_for ^ expo + points_against ^ expo),
    pyth_xwins_fp = games * pyth,
    pyth_xwins = (points_diff /(games * 2.0625)) + 8,
    pyth_xlosses = games - pyth_xwins,
    pyth_xwin_percentage = pyth_xwins / ifelse(season >= 2021, 17, 16) #total games at end of season
  ) |>
  select(
    season, team, games, wins, losses, ties, win_percentage, points_diff, points_for, points_against, pyth_xwins_fp, pyth_xwins, pyth_xlosses, pyth_xwin_percentage
  )

# Compute percentage of plays with wp > wp_lim ---------------------------------

wp_combined <- pbp |>
  filter(!is.na(vegas_wp) & !is.na(posteam)) |>
  group_by(season, posteam) |>
  summarise(
    pos_plays = n(),
    pos_wp_lim_plays = sum(vegas_wp > wp_limit)
  ) |>
  ungroup() |>
  left_join(
    pbp |>
      filter(!is.na(vegas_wp) & !is.na(posteam)) |>
      group_by(season, defteam) |>
      summarise(
        def_plays = n(),
        def_wp_lim_plays = sum(vegas_wp < wp_limit)
      ) |>
      ungroup(),
    by = c("season", "posteam" = "defteam")
  ) |>
  rename(team = "posteam") |>
  mutate(
    wp_lim_percentage = as.numeric(pos_wp_lim_plays + def_wp_lim_plays) / as.numeric(pos_plays + def_plays)
  ) |>
  select(season, team, wp_lim_percentage)

# Combine data and add colors and logos ----------------------------------------

chart <- outcomes |>
  left_join(wp_combined, by = c("season", "team")) |>
  filter(!is.na(wp_lim_percentage)) |>
  mutate(diff = 100 * (win_percentage - wp_lim_percentage)) |>
  group_by(team) |>
  # summarise_all(mean) |>
  ungroup() |>
  inner_join(
    nflfastR::teams_colors_logos |> select(team_abbr, team_color, team_logo_espn, team_logo_wikipedia),
    by = c("team" = "team_abbr")
  ) |>
  mutate(
    grob = map(seq_along(team_logo_espn), function(x) {
      grid::rasterGrob(magick::image_read(team_logo_espn[[x]]))
    })
  ) |>
  select(team, win_percentage, pyth_xwin_percentage, wp_lim_percentage, diff, team_color, grob) |>
  arrange(desc(diff))


# Create scatterplot -----------------------------------------------------------
wins_above_expected_scatter <- chart |>
  ggplot(aes(x = wp_lim_percentage, y = win_percentage)) +
  geom_abline(intercept = 0, slope = 1) +
  geom_hline(aes(yintercept = mean(win_percentage)), color = "red", linetype = "dashed") +
  geom_vline(aes(xintercept = mean(wp_lim_percentage)), color = "red", linetype = "dashed") +
  ggpp::geom_grob(aes(x = wp_lim_percentage, y = win_percentage, label = grob), vp.width = 0.05) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(
    x = glue::glue("Percentage of snaps with win probability (vegas_wp) over {100 * wp_limit}%"),
    y = "True win percentage (including ties as half a win)",
    title = glue("NFL Team Efficiency {current_season}"),
    subtitle = glue("Through week {n_week}")
  ) +
  # ggthemes::theme_stata(scheme = "sj", base_size = 8) +
  theme_cw_dark +
  theme(
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(hjust = 1),
    axis.text.y = element_text(angle = 0, vjust = 0.5),
    axis.title.y = element_text(angle = 90),
    legend.title = element_text(size = 8, hjust = 0, vjust = 0.5, face = "bold"),
    legend.position = "top"
  ) + NULL

brand_plot(wins_above_expected_scatter, asp = 16/10, save_name = glue('plots/desktop/team_wins/wins_above_expected_scatter_{current_season}.png'), data_home = 'Data: @nflfastR', fade_borders = '')

# Create bar plot  -------------------------------------------------------------
wins_above_expected_bar_dark <- chart |>
  ggplot(aes(x = seq_along(diff), y = diff)) +
  geom_hline(aes(yintercept = mean(diff)), color = "red", linetype = "dashed") +
  geom_col(width = 0.5, color = chart$team_color, fill = chart$team_color, alpha = 0.5) +
  ggpp::geom_grob(aes(x = seq_along(diff), y = diff, label = grob), vp.width = 0.035) +
  # scale_x_continuous(expand = c(0,0)) +
  labs(
    x = "Rank",
    y = "Win Percentage Over Expectation",
    title = glue("NFL Team Efficiency {current_season}"),
    subtitle = glue("How Lucky is each Team? Through week {n_week}")
  ) +
  # ggthemes::theme_stata(scheme = "sj", base_size = 8) +
  theme_cw_dark +
  theme(
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(hjust = 1),
    axis.title.y = element_text(angle = 90),
    axis.text.y = element_text(angle = 0, vjust = 0.5),
    legend.title = element_text(size = 8, hjust = 0, vjust = 0.5, face = "bold"),
    legend.position = "top"
  ) +
  NULL

brand_plot(wins_above_expected_bar_dark, asp = 16/10, save_name = glue('plots/desktop/team_wins/wins_above_expected_bar_{current_season}_dark.png'), data_home = 'Data: @nflfastR', fade_borders = '')

wins_above_expected_bar_light <- chart |>
  ggplot(aes(x = seq_along(diff), y = diff)) +
  geom_hline(aes(yintercept = mean(diff)), color = "red", linetype = "dashed") +
  geom_col(width = 0.5, color = chart$team_color, fill = chart$team_color, alpha = 0.5) +
  ggpp::geom_grob(aes(x = seq_along(diff), y = diff, label = grob), vp.width = 0.035) +
  # scale_x_continuous(expand = c(0,0)) +
  labs(
    x = "Rank",
    y = "Win Percentage Over Expectation",
    title = glue("NFL Team Efficiency {current_season}"),
    subtitle = glue("How Lucky is each Team? Through week {n_week}")
  ) +
  # ggthemes::theme_stata(scheme = "sj", base_size = 8) +
  theme_cw_light +
  theme(
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(hjust = 1),
    axis.title.y = element_text(angle = 90),
    axis.text.y = element_text(angle = 0, vjust = 0.5),
    legend.title = element_text(size = 8, hjust = 0, vjust = 0.5, face = "bold"),
    legend.position = "top"
  ) +
  NULL

brand_plot(wins_above_expected_bar_light, asp = 16/10, save_name = glue('plots/desktop/team_wins/wins_above_expected_bar_{current_season}_light.png'), data_home = 'Data: @nflfastR', fade_borders = '')


# Pythagorean Wins
pythagorean_wins_above_expected_scatter <- chart |>
  ggplot(aes(x = pyth_xwin_percentage, y = win_percentage)) +
  geom_abline(intercept = 0, slope = 1) +
  geom_hline(aes(yintercept = mean(win_percentage)), color = "red", linetype = "dashed") +
  geom_vline(aes(xintercept = mean(pyth_xwin_percentage)), color = "red", linetype = "dashed") +
  ggpp::geom_grob(aes(x = pyth_xwin_percentage, y = win_percentage, label = grob), vp.width = 0.05) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(
    x = glue::glue("Pythagorean expected win probability"),
    y = "True win percentage (including ties as half a win)",
    title = glue("NFL Team Pythagorean Expectation {current_season}"),
    subtitle = glue("Pythagorean expected wins through week {n_week}")
  ) +
  # ggthemes::theme_stata(scheme = "sj", base_size = 8) +
  theme_cw_dark +
  theme(
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(hjust = 1),
    axis.text.y = element_text(angle = 0, vjust = 0.5),
    axis.title.y = element_text(angle = 90),
    legend.title = element_text(size = 8, hjust = 0, vjust = 0.5, face = "bold"),
    legend.position = "top"
  ) + NULL

brand_plot(pythagorean_wins_above_expected_scatter, asp = 16/10, save_name = glue('plots/desktop/team_wins/pythag_wins_above_expected_scatter_{current_season}.png'), data_home = 'Data: @nflfastR', fade_borders = '')

rm(wp_limit, outcomes, wp_combined, chart, wins_above_expected_scatter, wins_above_expected_bar_dark, wins_above_expected_bar_light, pythagorean_wins_above_expected_scatter)

# })
