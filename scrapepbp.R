##set custom variables
  userYear <- 2019
  userWeek <- 3
  today <- Sys.Date()
  
  #test date
  date <- 20190919
  date <- format(today, format="%Y%m%d")
  
game_ids <- read.csv("data/games_data/reg_season/reg_games_2019.csv")
  
  currentGameIDs <- game_ids$game_id
  #pull games in 2019 season that match today's date
  currentGames <- grep(date, currentGameIDs)
  games_in_play <- currentGameIDs[currentGames]

##can't figure this out yet
  games_in_play <- game_ids$state_of_game[currentGames] != "POST"
##
  nplay <- length(games_in_play)
  nplayLoop <- 1
##

#scrape pbp of active games
for (x in games_in_play) {
  #read game csv
  y <- read.csv(paste("data/games_data/", userYear, "/", x, ".csv", sep = ""))
  
  #check if y$desc contains "END GAME"
  endGame <- grepl("END GAME", y$desc)
  
  #if pbp has END GAME change state_of_game to POST
  if(any(endGame == TRUE)) {
    game_ids[game_ids$game_id == x, "state_of_game"] <- "POST"
    print(paste("Change state of game for ", x, " to POST", sep = ""))
  } else {
    #scrape
    print(paste("Scraping game ", x, sep = ""))
    y <- scrape_json_play_by_play(x)
    write.csv(y, file = paste("data/games_data/", userYear,"/", x, ".csv", sep = ""))
    endGame <- grepl("END GAME", y$desc)
    #check for end game and add post status if scrape includes
    if(any(endGame == TRUE)) {
      game_ids[game_ids$game_id == x, "state_of_game"] <- "POST"
      print(paste("Change state of game for ", x, " to POST", sep = ""))
    }
  }
}