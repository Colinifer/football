qbs_df <- nflfastR::fast_scraper_roster(year) |> 
  filter(position == 'QB') |> 
  select(
    team,
    full_name,
    birth_date,
    gsis_id,
    entry_year,
    rookie_year
  )
  
  
stats_df <- calculate_player_stats(pbp = pbp_df)

stats_df |> 
  filter(position == "QB"
         # & attempts > stats_df |> 
         #   filter(attempts > 0) |>
         #   pull(attempts) |> 
         #   median()
         ) |> 
  left_join(
    qbs_df,
    by = c("player_id" = "gsis_id")
  ) |> 
  mutate(
    days_old = Sys.Date() - birth_date,
    epa_per_attempt = passing_epa / attempts,
    yards_per_attempt = passing_yards / attempts,
    adot = passing_air_yards / attempts,
    completion_percentage = completions / attempts
  ) |> 
  select(
    player_id,
    player_name,
    position,
    birth_date,
    days_old,
    entry_year,
    rookie_year,
    games,
    attempts,
    completions,
    passing_yards,
    passing_tds,
    passing_air_yards,
    passing_yards_after_catch,
    passing_first_downs,
    passing_epa,
    epa_per_attempt,
    yards_per_attempt,
    adot,
    completion_percentage,
  ) |> 
  arrange(desc(entry_year), desc(passing_epa)) |> 
  # filter(entry_year <= 2020) |> 
  view()

plot_wp_dist <- function(wk) {
  return(
    pbp_df |> 
      arrange(week, old_game_id, play_id) |> 
      filter(week == wk) |> 
      ggplot() + 
      geom_density(aes(x = home_wp)) + 
      # geom_density(aes(x = vegas_home_wp)) + 
      facet_wrap(facets = "game_id") +
      # labs(title = game) +
      xlim(0,1)
  )
}

plot_wp_worm <- function(wk) {
  return(
    pbp_df |> 
      arrange(week, old_game_id, play_id) |> 
      filter(week == wk) |> 
      ggplot(aes(x = game_seconds_remaining)) + 
      geom_line(aes(y = home_wp)) +
      geom_line(aes(y = vegas_home_wp)) +
      facet_wrap(facets = "game_id") +
      # labs(title = game_id) + 
      scale_x_reverse() + 
      ylim(0, 1)
  )
}


plot_wp_dist(17)
plot_wp_worm(17)

pbp_df |> 
  select(team = posteam, epa) |> 
  group_by(team) |> 
  summarise(count_off = n(),
            off_epa = mean(epa, na.rm = T)) |> 
  left_join(
    pbp_df |> 
      select(team = defteam, epa) |> 
      group_by(team) |> 
      summarise(count_def = n(),
                def_epa = mean(epa, na.rm = T)),
    by = "team"
  ) |> 
  mutate(
    net_epa = off_epa - def_epa
  ) |> 
  arrange(desc(net_epa))

roster_df |> 
  filter(position == "QB"
         & status == "ACT") |> 
  select(
    season, team, position, status, full_name, college, gsis_id
  ) |> 
  left_join(
    calculate_stats() |> 
      select(
        season, player_id, targets, receptions, receiving_yards, receiving_epa, receiving_tds
      ),
    by = c("season" = "season", "gsis_id" = "player_id")
  ) |> 
  arrange(desc(passing_yards))
