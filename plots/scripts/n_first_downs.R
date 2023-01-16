year <- current_season

my_week <- fx.n_week(pbp_df)

z_score <- function(x) {
  z_score = (x-mean(x))/sd(x)
  return(z_score)
}


# Total Points For --------------------------------------------------------

team_points <- pbp_df |> 
  arrange(old_game_id, play_id) |> 
  filter(!is.na(posteam)) |> 
  mutate(
    posteam_result = case_when(
      posteam == home_team ~ home_score,
      posteam == away_team ~ away_score
    )
  ) %>% 
  group_by(game_id, posteam) |> 
  summarise(
    posteam_result = custom_mode(posteam_result)
  ) |> 
  group_by(posteam) |> 
  summarise(
    total_points_for = sum(posteam_result, na.rm = T)
  ) |> 
  mutate(total_points_for_z_score = round(z_score(total_points_for), 2)) |> 
  arrange(total_points_for_z_score)

team_points |> 
  filter(!is.na((posteam))) |> 
  ggplot(aes(x = factor(posteam, levels = posteam), y = total_points_for)) + 
  coord_flip() + 
  nflplotR::geom_nfl_logos(aes(team_abbr = posteam), height = 0.066) +
  theme_cw_dark


# Eckel Rate/Ratio --------------------------------------------------------

eckel_rate <- pbp_df |> 
  arrange(old_game_id, play_id) |> 
  filter(!is.na(posteam)) |> 
  select(game_id, home_team, away_team, posteam, drive, drive_start_yard_line, drive_end_yard_line) |> 
  unique() |> 
  mutate(drive_start_yard_line = ifelse(drive_start_yard_line == 50, paste(posteam, drive_start_yard_line), drive_start_yard_line),
         drive_end_yard_line = ifelse(drive_end_yard_line == 50, paste(posteam, drive_end_yard_line), drive_end_yard_line)) |> 
  separate(col = drive_start_yard_line, 
           sep = ' ',
           into = c('drive_start_field_side', 'drive_start_yardline')
  ) |> 
  separate(col = drive_end_yard_line, 
           sep = ' ',
           into = c('drive_end_field_side', 'drive_end_yardline')
  ) |> 
  mutate(
    drive_start_yardline = ifelse(drive_start_field_side == posteam, 100-as.numeric(drive_start_yardline), as.numeric(drive_start_yardline)),
    drive_end_yardline = ifelse(drive_end_field_side == posteam, 100-as.numeric(drive_end_yardline), as.numeric(drive_end_yardline)),
    eckel_drive = ifelse(drive_end_yardline <= 40, 1, 0)
  ) |> 
  group_by(posteam) |> 
  summarise(
    drives = n(),
    eckel_drive = sum(eckel_drive, na.rm = T)
  ) |> 
  mutate(eckel_ratio = eckel_drive/drives) |> 
  arrange(eckel_ratio)

eckel_rate |> 
  filter(!is.na((posteam))) |> 
  ggplot(aes(x = factor(posteam, levels = posteam), y = eckel_ratio)) + 
  ylim(round(min(eckel_rate$eckel_ratio), 2)-.05, 
       round(max(eckel_rate$eckel_ratio), 2)+.05) + 
  coord_flip() + 
  nflplotR::geom_nfl_logos(aes(team_abbr = posteam), height = 0.066) + 
  theme_cw_dark


# Success Rate ------------------------------------------------------------

success_rate <- pbp_df |> 
  arrange(old_game_id, play_id) |> 
  filter(!is.na(posteam)) |> 
  group_by(posteam) |> 
  summarise(
    plays = sum(play, na.rm = T),
    success = sum(success, na.rm = T)
  ) |> 
  mutate(success_rate = success/plays, 
         success_rate_z_score = (success_rate-mean(success_rate))/sd(success_rate)) |> 
  arrange(success_rate_z_score)

success_rate |> 
  filter(!is.na((posteam))) |> 
  ggplot(aes(x = factor(posteam, levels = posteam), y = success_rate_z_score)) + 
  ylim(-3, 3) + 
  coord_flip() + 
  nflplotR::geom_nfl_logos(aes(team_abbr = posteam), height = 0.066) + 
  theme_cw_dark


# Series Success ----------------------------------------------------------

series_success <- pbp_df |> 
  arrange(old_game_id, play_id) |> 
  filter(!is.na(posteam)) |> 
  select(game_id, drive, posteam, 
         series, series_result, series_success) |> 
  unique() |> 
  group_by(posteam) |> 
  summarise(
    series = n(),
    series_success = sum(series_success, na.rm = T)
  ) |> 
  mutate(series_success_rate = series_success/series,
         series_success_rate_percentile_rank = rank(series_success_rate)/length(series_success_rate),
         series_success_z_score = (series_success_rate-mean(series_success_rate))/sd(series_success_rate)) |> 
  arrange(series_success_z_score)

series_success |> 
  filter(!is.na((posteam))) |> 
  ggplot(aes(x = factor(posteam, levels = posteam), y = series_success_z_score)) + 
  ylim(-3, 3) + 
  coord_flip() + 
  nflplotR::geom_nfl_logos(aes(team_abbr = posteam), height = 0.066) + 
  theme_cw_dark

pbp_df |> 
  arrange(old_game_id, play_id) |> 
  filter(!is.na(posteam)) |> 
  select(game_id, drive, posteam, 
         series, series_result, series_success) |> 
  unique() |> 
  summarise(
    series = n(),
    series_success = sum(series_success, na.rm = T)
  ) |> 
  mutate(series_success_rate = series_success/series) |> 
  arrange(-series_success_rate)

pbp_df %>% 
  # filter(season == 2021) |> 
  filter(!is.na(posteam)) %>% 
  mutate(
    eckel_success = ifelse((down == 1 & yardline_100 <= 40) | (yards_gained > 40 & touchdown == 1), 1, 0)
  ) %>% 
  group_by(
    posteam, game_id, drive
  ) %>% 
  summarise(
    n_drive = n_distinct(game_id, drive),
    total_eckel = sum(eckel_success, na.rm = T)
  ) %>% 
  mutate(
    total_eckel = ifelse(total_eckel > 0, 1, 0)
  ) %>% 
  ungroup() %>% 
  group_by(posteam) %>% 
  summarise(
    n_drive = sum(n_drive, na.rm = T),
    total_eckel = sum(total_eckel, na.rm =T)
  ) %>% 
  left_join( # join team points to 
    team_points,
    by = c('posteam')
  ) %>% 
  mutate(
    eckel_rate = total_eckel / n_drive,
    eckel_ratio = round(total_points_for / total_eckel, 2)
  ) %>% 
  left_join(
    teams_colors_logos %>% 
      select(team_abbr, team_logo_espn),
    by = c('posteam' = 'team_abbr')
  ) %>% 
  select(
    team_logo_espn,
    posteam, 
    everything()
  ) %>% 
  arrange(-eckel_rate) %>% 
  gt() %>% 
  tab_header(title = 'Team Quality Possesions', 
             subtitle = glue('Through week {my_week}')) %>% 
  cols_label(
    team_logo_espn = 'Team',
    posteam = '',
    n_drive = 'Total Drives',
    total_eckel = '# Eckel Drives',
    total_points_for = 'Points For',
    eckel_rate = 'Eckel Rate',
    eckel_ratio = 'Pts/Eckel'
  ) %>% 
  fmt_percent(columns = c(eckel_rate), decimals = 1) %>% 
  gt_theme_cw(image_columns = 'team_logo_espn') %>% 
  gtsave(filename = glue("team_stats/eckel_rate_{current_season}.png"), path = "plots/desktop")



# Air Yards ---------------------------------------------------------------

tbl <- pbp_df |> 
  filter(!is.na(passer_player_id)) |> 
  group_by(passer_player_id) |> 
  summarise(
    passer_player_name = custom_mode(passer_player_name),
    posteam = custom_mode(posteam),
    games_played = n_distinct(game_id),
    total_air_yards = sum(air_yards, na.rm = TRUE),
    mean_air_yards = round(mean(air_yards, na.rm = TRUE), 2),
    total_pass_attempts = sum(pass_attempt, na.rm = TRUE)
  ) |> 
  ungroup() |> 
  mutate(
    adot = round(total_air_yards / total_pass_attempts, 2)
  ) |> 
  filter(total_pass_attempts >= (max(games_played)*8)) |> 
  arrange(-mean_air_yards) |> 
  left_join(
    teams_colors_logos |>  
      select(team_abbr, team_logo_espn),
    by = c('posteam' = 'team_abbr')
  ) |> 
  select(
    passer_player_name,
    team_logo_espn,
    posteam,
    games_played,
    total_air_yards,
    mean_air_yards,
    total_pass_attempts,
    adot
  ) |> 
  reactable(
    columns = list(
      passer_player_name = colDef('Player'),
      team_logo_espn = colDef('',
                              sortable = FALSE,
                              filterable = FALSE,
                              cell = function(value) {
                                image <-
                                  img(src = value,
                                      style = "height: 24px;",
                                      alt = value
                                      )
                                tagList(div(style = "display: inline-block; width: 45px;", image))
                              }),
      posteam = colDef('Team'),
      games_played = colDef('GP'),
      total_air_yards = colDef('Total Air Yards'),
      mean_air_yards = colDef('Mean Air Yards'),
      total_pass_attempts = colDef('Pass Attempts'),
      adot = colDef('ADoT')
    ),
    filterable = TRUE,
    showPageSizeOptions = TRUE,
    striped = TRUE,
    highlight = TRUE,
    theme = reactableTheme(
      color = "hsl(233, 9%, 87%)",
      backgroundColor = "hsl(233, 9%, 19%)",
      borderColor = "hsl(233, 9%, 22%)",
      stripedColor = "hsl(233, 12%, 22%)",
      highlightColor = "hsl(233, 12%, 24%)",
      inputStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
      selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
      pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
      pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)")
    )
  )

tbl

div(
  class = 'salary',
  div(
    # this can be called with CSS now via .title
    class = 'title',
    h2('Air Yards'),
    glue('Through week {my_week}')
  ),
  tbl,
  # I use a span here so I can assigna  color to this text
  tags$span(style = "color:#C8C8C8", "TABLE: @THOMAS_MOCK | DATA: PRO-FOOTBALL-REFERENCE.COM & OVERTHECAP.COM")
)

  gt() |> 
  tab_header(title = 'Air Yards', 
             subtitle = glue('Through week {my_week}')) |> 
  cols_label(
    passer_player_name = 'Player',
    team_logo_espn = 'Team',
    posteam = '',
    games_played = 'GP',
    total_air_yards = 'Total Air Yards',
    mean_air_yards = 'Mean Air Yards',
    total_pass_attempts = 'Pass Attempts',
    adot = 'ADoT'
  ) |> 
  fmt_number(columns = c(mean_air_yards, adot), decimals = 2) |> 
  gt_theme_cw(image_columns = c('team_logo_espn')) |> 
  gtsave(filename = glue("qb_passing/air_yards_{current_season}.png"), path = "plots/desktop")

pbp_df %>% 
  group_by(passer_player_id) %>% 
  filter(qb_dropback == 1 & !is.na(qb_dropback) & !is.na(passer_player_id) & penalty == 0 & two_point_attempt == 0) %>% 
  summarise(passer_player_name = first(passer_player_name),
            posteam = first(posteam),
            games_played = n_distinct(game_id),
            n_dropbacks = sum(qb_dropback, na.rm = TRUE), 
            n_first_downs = sum(first_down, na.rm = TRUE),
            n_success = sum(success, na.rm = TRUE)) %>% 
  mutate(success_rate = n_success / n_dropbacks,
         first_down_rate = n_first_downs / n_dropbacks) %>% 
  arrange(-first_down_rate) %>% 
  filter(n_dropbacks > max(games_played)*8) %>% 
  mutate(rk = paste0('#',row_number())) %>%
  left_join(
    teams_colors_logos %>% 
      select(team_abbr, team_logo_espn),
    by = c('posteam' = 'team_abbr')
  ) %>% 
  left_join(
    roster_df %>% 
      select(
        passer_player_id = gsis_id,
        headshot_url
      )
  ) %>% 
  select(rk, headshot_url, passer_player_name, team_logo_espn, posteam, first_down_rate, success_rate) %>% 
  gt() %>% 
  tab_header(title = 'Passer 1st Down Rate', 
             subtitle = glue('Through week {my_week} | Min. {my_week*8} dropbacks')) %>% 
  cols_label(
    rk = 'Rank',
    headshot_url = '',
    passer_player_name = 'Player',
    team_logo_espn = '',
    posteam = 'Team',
    first_down_rate = '1D/DB',
    success_rate = 'Success Rate'
  ) %>% 
  fmt_percent(columns = c(first_down_rate, success_rate), decimals = 1) %>% 
  gt_theme_cw(image_columns = c('headshot_url', 'team_logo_espn')) %>% 
  gtsave(filename = glue("qb_passing/1db_{current_season}.png"), path = "plots/desktop")

