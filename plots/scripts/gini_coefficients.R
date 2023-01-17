gini_df <- map_df(1:max(player_stats_weekly$week), function(x.week){
  weekly_teams <- player_stats_weekly |> 
    filter(week == x.week) |> 
    pull(recent_team) |> 
    unique() |> 
    sort()
  
  weekly_game_ids <- schedule_df |> 
    filter(season == current_season & 
             week == x.week) |> 
    pull(game_id) |> 
    unique() |> 
    sort()
  
  map_df(weekly_teams, function(x.team){
    target_gini_value <- player_stats_weekly |> 
      left_join(roster_df |> 
                  select(player_id = gsis_id, position), 
                by = c('player_id')) |> 
      filter(week == x.week & 
                recent_team == x.team &
                position %in% c('QB', 'RB', 'WR', 'TE')) |> 
      # group_by(game_id, recent_team) |> 
      pull(targets) |> 
      Gini(unbiased = FALSE)
    
    rush_gini_value <- player_stats_weekly |> 
      left_join(roster_df |> 
                  select(player_id = gsis_id, position), 
                by = c('player_id')) |> 
      filter(game_id %in% c(weekly_game_ids) & 
               recent_team == x.team &
               position %in% c('QB', 'RB', 'WR', 'TE')) |> 
      # group_by(game_id, recent_team) |> 
      pull(carries) |> 
      Gini(unbiased = FALSE)
    
    # helper_df <- pbp_df |> 
    #   filter(posteam == x.team & 
    #            game_id %in% weekly_game_ids) |> 
    #   select(posteam, week, game_id, play_id) |> 
    #   arrange(game_id, play_id) |> 
    #   left_join(
    #     nflreadr::load_participation() |> 
    #       filter(possession_team == x.team &
    #                nflverse_game_id %in% c(weekly_game_ids)) |>
    #       select(game_id = nflverse_game_id, 
    #              posteam = possession_team, 
    #              play_id, 
    #              offense_personnel, 
    #              offense_formation),
    #     by = c('posteam', 'game_id', 'play_id')
    #   ) |> 
    #   drop_na()
    
    personnel_gini_value <- helper_df |> 
      filter(posteam == x.team) |> 
      group_by(offense_personnel) |> 
      count() |> 
      pull(n) |> 
      Gini(unbiased = FALSE)
    
    formation_gini_value <- helper_df |> 
      filter(posteam == x.team) |> 
      group_by(offense_formation) |> 
      count() |> 
      pull(n) |> 
      Gini(unbiased = FALSE)
    
    df <- tibble(
      team = c(x.team),
      week = c(x.week),
      target_gini = c(target_gini_value),
      rush_gini = c(rush_gini_value),
      personnel_gini = c(personnel_gini_value),
      formation_gini = c(formation_gini_value)
    )
    
    return(df)
  })
})

gini_df

player_stats_weekly |> 
  left_join(roster_df |> 
              select(player_id = gsis_id, position)) |> 
  filter(position %in% c('RB', 'WR', 'TE') 
         & recent_team == 'LAC') |> 
  # group_by(game_id, recent_team) |> 
  pull(targets) |> 
  Gini(unbiased = FALSE)
