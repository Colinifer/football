
con <- fx.db_con(x.host = 'localhost')
pbp <- tbl(con, 
           'nflfastR_pbp',
           x.host = 'localhost') |> 
  filter(season >= max(season)-2) |> 
  collect() # |> 
dbDisconnect(con)

fantasy_stats <- pbp |> 
  calculate_player_stats(weekly = TRUE)

iso_team <- 'lac'

fantasy_stats |> 
  filter(tolower(recent_team) %in% iso_team) |> 
  left_join(
    fantasy_stats |> 
      filter(tolower(recent_team) %in% iso_team) |> 
      select(season, week, recent_team) |> 
      unique() |> 
      mutate(
        game_number = row_number()
      ),
    by = c('recent_team', 'season', 'week')
  ) |> 
  group_by(player_id) |> 
  mutate(
    roll_targets = rollmean(targets, 4, na.pad = TRUE)
  ) |> 
  select(season, player_id, player_display_name, game_number, roll_targets) |> 
  inner_join(
    fantasy_stats |> 
      filter(tolower(recent_team) %in% iso_team) |> 
      group_by(player_id) |> 
      summarise(
        player_display_name = first(player_display_name),
        total_targets = sum(targets, na.rm=TRUE)
      ) |> 
      ungroup() |> 
      mutate(total_target_share = total_targets/sum(total_targets)) |> 
      select(player_id, player_display_name, total_targets, total_target_share) |> 
      arrange(desc(total_target_share)) |> 
      head(8),
    by = c('player_id', 'player_display_name')
  ) |> 
  ggplot(aes(x = game_number, y = roll_targets, color = player_display_name)) +
  #geom_line(linewidth=1.2) + 
  geom_smooth(se = FALSE, linewidth=1) + 
  guides(color = guide_legend(override.aes = list(linewidth = 4)))

