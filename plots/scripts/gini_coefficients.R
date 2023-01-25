get_gini <- function(stats_df = player_stats_weekly, 
                    pbp = pbp_df,
                    participation = nflreadr::load_participation(), 
                    schedule = schedule_df, 
                    weekly = TRUE){
  
  stats_df <- stats_df |> 
    filter(offense_snaps > 0) |> 
    left_join(roster_df |> 
                select(player_id = gsis_id, position), 
              by = c('player_id'))
  
  helper_df <- pbp |>
    select(posteam, week, game_id, play_id) |>
    arrange(game_id, play_id) |>
    left_join(
      participation |>
        select(game_id = nflverse_game_id,
               posteam = possession_team,
               play_id,
               offense_personnel,
               offense_formation),
      by = c('posteam', 'game_id', 'play_id')
    ) |>
    drop_na()
  
  if (weekly == TRUE) {
    
    stats_df <- stats_df |> 
      filter(week <= 18)
    
    map_df(1:max(stats_df$week), function(x.week) {
      
      # tic()
      
      weekly_teams <- stats_df |> 
        filter(week == x.week) |> 
        pull(recent_team) |> 
        unique() |> 
        sort()
      
      weekly_game_ids <- schedule |> 
        filter(season == current_season & 
                 week == x.week) |> 
        pull(game_id) |> 
        unique() |> 
        sort()
      
      map_df(weekly_teams, function(x.team) {
        
        target_gini_value <- stats_df |> 
          filter(week == x.week & 
                   recent_team == x.team &
                   position %in% c('QB', 'RB', 'WR', 'TE')) |> 
          # group_by(game_id, recent_team) |> 
          pull(targets) |> 
          Gini()
        
        rush_gini_value <- stats_df |> 
          filter(game_id %in% c(weekly_game_ids) & 
                   recent_team == x.team &
                   position %in% c('QB', 'RB', 'WR', 'TE')) |> 
          # group_by(game_id, recent_team) |> 
          pull(carries) |> 
          Gini()
        
        personnel_vector <- helper_df |> 
          filter(posteam == x.team & 
                   game_id %in% weekly_game_ids) |> 
          group_by(offense_personnel) |> 
          count() |> 
          pull(n)
        
        if (length(personnel_vector) == 1) {
          personnel_vector <- c(personnel_vector, 0)
        }
        
        personnel_gini_value <- personnel_vector |> 
          Gini()
        
        formation_vector <- helper_df |> 
          filter(posteam == x.team & 
                   game_id %in% weekly_game_ids) |> 
          group_by(offense_formation) |> 
          count() |> 
          pull(n)
        
        if (length(formation_vector) == 1) {
          formation_vector <- c(formation_vector, 0)
        }
        
        formation_gini_value <- formation_vector |> 
          Gini()
        
        df <- tibble(
          team = c(x.team),
          week = c(x.week),
          target_gini = c(target_gini_value),
          rush_gini = c(rush_gini_value),
          personnel_gini = c(personnel_gini_value),
          formation_gini = c(formation_gini_value)
        )
        
        return(df)
      }) # end team loop
      # toc()
    }) # end week loop
    
  } # end (weekly == TRUE)
  
  if (weekly == FALSE) {
    
    teams <- stats_df |> 
      pull(recent_team) |> 
      unique() |> 
      sort()
    
    map_df(teams, function(x.team) {
      
      target_gini_value <- stats_df |> 
        filter(recent_team == x.team &
                 offense_snaps > 0 &
                 position %in% c('QB', 'RB', 'WR', 'TE')
               ) |> 
        # group_by(game_id, recent_team) |> 
        pull(targets) |> 
        Gini()
      
      rush_gini_value <- stats_df |> 
        filter(recent_team == x.team &
                 offense_snaps > 0 &
                 position %in% c('QB', 'RB', 'WR', 'TE')
                 ) |> 
        # group_by(game_id, recent_team) |> 
        pull(carries) |> 
        Gini()
      
      personnel_vector <- helper_df |> 
        filter(posteam == x.team & 
                 game_id %in% weekly_game_ids) |> 
        group_by(offense_personnel) |> 
        count() |> 
        pull(n)
      
      if (length(personnel_vector) == 1) {
        personnel_vector <- c(personnel_vector, 0)
      }
      
      personnel_gini_value <- personnel_vector |> 
        Gini()
      
      formation_vector <- helper_df |> 
        filter(posteam == x.team & 
                 game_id %in% weekly_game_ids) |> 
        group_by(offense_formation) |> 
        count() |> 
        pull(n)
      
      if (length(formation_vector) == 1) {
        formation_vector <- c(formation_vector, 0)
      }
      
      formation_gini_value <- formation_vector |> 
        Gini()
      
      df <- tibble(
        team = c(x.team),
        target_gini = c(target_gini_value),
        rush_gini = c(rush_gini_value),
        personnel_gini = c(personnel_gini_value),
        formation_gini = c(formation_gini_value)
      )
      
      return(df)
    })
    
  }
  
}

gini_weekly_df <- get_gini(weekly = TRUE)

gini_weekly_df

gini_season_df <- get_gini(stats_df = player_stats, 
                           weekly = FALSE)

gini_season_df

player_stats_weekly |> 
  left_join(roster_df |> 
              select(player_id = gsis_id, position)) |> 
  filter(position %in% c('RB', 'WR', 'TE') 
         & recent_team == 'LAC') |> 
  # group_by(game_id, recent_team) |> 
  pull(targets) |> 
  Gini(unbiased = FALSE)
