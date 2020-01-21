scrapePBP <- function(date) {
  ## read Game IDs
  game_ids <- read.csv(fgame_ids, check.names = FALSE)
  ## save Game IDs 
  # write.csv(game_ids, file = fgame_ids, row.names = FALSE)
  
  y <- data.frame()
  
  currentGameIDs <- game_ids$game_id
  #pull games in 2019 season that match today's date
  ##currentGames <- grep(date, currentGameIDs)
  currentGames <- currentGameIDs <= date
  games_in_play <- currentGameIDs[currentGames == TRUE]
  
  ##can't figure this out yet
  #  
  #  games_in_play <- game_ids$state_of_game[currentGames] != "POST"
  #
  #  nplay <- length(games_in_play)
  #  nplayLoop <- 1
  #
  
  # scrape pbp of active games
  
  # if 0 games, scrape scores
  for (x in games_in_play)
  {
    f <- paste("data/games/", userYear, "/", x, ".csv", sep = "")
    fplayers <-  paste("data/players/", userYear, "/", x, "players", ".csv", sep = "")
    
    if (file.exists(f)==TRUE)
    {
      
      #read game csv
      y <- read.csv(f, check.names=FALSE)
      tail(y)
      
      #check if y$desc contains "END GAME"
      #if x has END GAME change state_of_game to POST
      if (grepl("END GAME", y$desc[nrow(y)]) == TRUE)
      {
        print(paste("Game", x, "is over.", sep = " "))
        game_ids[game_ids$game_id == x, "state_of_game"] <- "POST"
        game_ids[game_ids$game_id == x, "away_score"] <- y$total_away_score[nrow(y)]
        game_ids[game_ids$game_id == x, "home_score"] <- y$total_home_score[nrow(y)]
        print(paste("Changing the state of game for", x, "to POST", sep = " "))
        write.csv(game_ids, fgame_ids, row.names = FALSE)
      }
      else
      {
        #scrape
        print(
          paste(
            "Scraping game ", 
            x, 
            sep = ""
            )
          )
        print(
          paste(
            game_ids[game_ids$game_id == x, "away_team"], 
            "vs", 
            game_ids[game_ids$game_id == x, "home_team"], 
            sep = " "
            )
          )
        y <- scrape_json_play_by_play(x)
        tail(y)
        game_ids$X <- NULL ## annoying glitch
        if (grepl("END GAME", y$desc[nrow(y)]) == TRUE) {
          print(
            paste(
              "Game", 
              x, 
              "is over.", 
              sep = " "
              )
            )
          print(
            paste(
              game_ids[game_ids$game_id == x, "away_team"], 
              "vs", 
              game_ids[game_ids$game_id == x, "home_team"], 
              sep = " "
              )
            )
          game_ids[game_ids$game_id == x, "state_of_game"] <- "POST"
          game_ids[game_ids$game_id == x, "away_score"] <- y$total_away_score[nrow(y)]
          game_ids[game_ids$game_id == x, "home_score"] <- y$total_home_score[nrow(y)]
          print(paste("Changing the state of game for ", x, " to POST", sep = ""))
          write.csv(game_ids, fgame_ids, row.names=FALSE)
        }
        write.csv(y, file = paste("data/games/", userYear,"/", x, ".csv", sep = ""), row.names=FALSE)
      }
    }
    else
    {
      print(
        paste(
          "Scraping game", 
          x, 
          sep = " "
          )
        )
      print(
        paste(
          game_ids[
            game_ids$game_id == x, "away_team"], 
          "vs", 
          game_ids[game_ids$game_id == x, "home_team"], 
          sep = " "
        )
      )
      y <- scrape_json_play_by_play(x)
      tail(y)
      write.csv(y, file = paste("data/games/", userYear,"/", x, ".csv", sep = ""), row.names=FALSE)
    }
    xawayscore <- y$total_away_score[nrow(y)]
    xawayteam <- y$away_team[1]
    xhomescore <- y$total_home_score[nrow(y)]
    xhometeam <- y$home_team[1]
    print(paste(xawayteam, ": ", xawayscore, " @ ", xhometeam, ": ", xhomescore, sep = ""))
    
    print(
      paste(
        "Last play:", 
        y$desc[nrow(y)], 
        sep=""
        )
      )
    
    ## add to the normal scrape functions
    if (file.exists(fplayers)==TRUE & grepl("END GAME", y$desc[nrow(y)]) == TRUE)
    {
      xplayers <- player_game(x)
      write.csv(xplayers, fplayers, row.names = FALSE)
      print(paste("X =", x))
      xpbp <- game_play_by_play(x)
      addTargets(x)
    } else {
      ## scrape player stats
      xplayers <- player_game(x)
      print(paste("X =", x))
      xpbp <- game_play_by_play(x)
      addTargets(x)
    }
    
    addTargets(x)
  }
}
