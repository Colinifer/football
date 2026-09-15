#' Create a matchup dataframe
#'
#' @param schedule_df A dataframe containing the NFL schedule
#'
#' @return A dataframe containing the matchups for the given season
#'
#' @examples
#' matchup_df <- fx.matchups()
#'
#' # Print the first 10 rows of the matchup dataframe
#' head(matchup_df)
#'
#' @export
fx.matchups <- function(con = fx.db_con(x.host = 'localhost'),
                        season = fx.get_year()) {
  
  on.exit(dbDisconnect(con), add = TRUE)
  
  # Filter the schedule dataframe to only include the games for the current season
  matchup_df <- tbl(con, 'nflfastR_schedule') |>
    filter(season == season) |>
    
    # Add a new column called `posteam` that contains the home team
    # Add a new column called `oppteam` that contains the away team
    # Select the columns that we want to include in the matchup dataframe
    select(
      game_id,
      season,
      game_type,
      week,
      gameday,
      weekday,
      gametime,
      away_team,
      home_team,
      away_score,
      home_score,
      home_result = result,
      stadium,
      location,
      roof,
      surface,
      old_game_id
    ) |>
    collect() |>
    mutate(posteam = home_team,
           oppteam = away_team) |> 
    
    # Bind the filtered schedule dataframe to a reversed schedule dataframe
    rbind(
      tbl(con, 'nflfastR_schedule') |>
        filter(season == season) |>
        
        # Add a new column called `posteam` that contains the away team
        # Add a new column called `oppteam` that contains the home team
        # Select the columns that we want to include in the matchup dataframe
        select(
          game_id,
          season,
          game_type,
          week,
          gameday,
          weekday,
          gametime,
          away_team,
          home_team,
          away_score,
          home_score,
          home_result = result,
          stadium,
          location,
          roof,
          surface,
          old_game_id
        ) |> 
        collect() |> 
        mutate(posteam = home_team,
               oppteam = away_team)
    ) |>
    
    # Arrange the matchup dataframe by season, week, weekday, game time, and home team
    arrange(season,
            week,
            desc(weekday),
            gametime,
            posteam)
  
  # Return the matchup dataframe
  return(matchup_df)
}
