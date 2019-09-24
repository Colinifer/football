##set custom variables
  userYear <- 2019 ##necessary for saved 
  userWeek <- 3 ##not necessary at the moment
  today <- Sys.Date()
  
  #test date
  ##date <- 20190922
  date <- format(today, format="%Y%m%d")
  
game_ids <- read.csv("data/games_data/reg_season/reg_games_2019.csv")
game_ids <- read.csv("data/games_data/reg_season/reg_games_2019.csv")
  
  currentGameIDs <- game_ids$game_id
  #pull games in 2019 season that match today's date
  currentGames <- grep(date, currentGameIDs)
  games_in_play <- currentGameIDs[currentGames]

##can't figure this out yet
##  
##  games_in_play <- game_ids$state_of_game[currentGames] != "POST"
##
##  nplay <- length(games_in_play)
##  nplayLoop <- 1
##

#scrape pbp of active games
for (x in games_in_play) {
  #read game csv
  if (file.exists(f)) {
    y <- read.csv(f)
    #check if y$desc contains "END GAME"
    endGame <- grepl("END GAME", y$desc)
    y <- game_ids
    #check if y$desc contains "END GAME"
    endGame <- grepl("END GAME", y$desc)
  } else {
    y <- game_ids
    #check if y$desc contains "END GAME"
    endGame <- 2 > 3
  }
  #if x has END GAME change state_of_game to POST
  if(any(endGame == TRUE)) {
    f <- paste("data/games_data/", userYear, "/", x, ".csv", sep = "")
    game_ids[game_ids$game_id == x, "state_of_game"] <- "POST"
    print(paste("Changing the state of game for ", x, " to POST", sep = ""))
    ##save changes to season game_ids
    write.csv(game_ids, "data/games_data/reg_season/reg_games_2019.csv")
  } else {
    #scrape
    print(paste("Scraping game ", x, sep = ""))
    y <- scrape_json_play_by_play(x)
    write.csv(y, file = paste("data/games_data/", userYear,"/", x, ".csv", sep = ""))
    endGame <- grepl("END GAME", y$desc)
    #check for end game and add post status if scrape includes
    if(any(endGame == TRUE)) {
      game_ids[game_ids$game_id == x, "state_of_game"] <- "POST"
      print(paste("Changing the state of game for ", x, " to POST", sep = ""))
    }
  }
}
  
  
  
##  game_ids[game_ids$game_id == 2019092300, "state_of_game"] <- "PRE"
##  write.csv(game_ids, "data/games_data/reg_season/reg_games_2019.csv")