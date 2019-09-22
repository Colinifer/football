##install.packages("devtools", "tidyverse", "readr")
##devtools::install_github(repo = "maksimhorowitz/nflscrapR")

library(nflscrapR)
library(tidyverse)
library(readr)

##set custom variables
  userYear <- 2019
  userWeek <- 3
  today <- Sys.Date()
  
  #test date
  #date <- 20190915
  date <- format(today, format="%Y%m%d")
  
game_ids <- read.csv("data/games_data/reg_season/reg_games_2019.csv")
  
  currentGameIDs <- game_ids$game_id
  currentGames <- grep(date, currentGameIDs)
  games_in_play <- currentGameIDs[currentGames]
  games_in_play
  nplay <- length(games_in_play)
  nplayLoop <- 1
##

ntrue <- (which(currentGames==TRUE))


#end of night
game_ids <- scrape_game_ids(userYear)
game_ids[currentGames , "state_of_game"] <- "POST"
game_ids[game_ids$week==userWeek , "state_of_game"]
write.csv(game_ids, file = paste("data/games_data/reg_season/reg_games_", userYear, ".csv", sep = ""),row.names=FALSE)

#need new loop for the currentGameIDS to scrape
#don't scrape if state_of_game POST
pbp <- scrape_json_play_by_play(games_in_play)
write.csv(pbp, file = paste("data/games_data/", userYear,"/", games_in_play, ".csv", sep = ""))


#scrape pbp of active games
for (x in games_in_play) {
  print(paste("Scraping game ", x, sep = ""))
  y <- scrape_json_play_by_play(x)
  write.csv(y, file = paste("data/games_data/", userYear,"/", x, ".csv", sep = ""))
}
#if pbp has END GAME
for (x in games_in_play) {
    
  }
  
  else if (currentGames[trueLoop] == FALSE) { 
        trueLoop <- trueLoop + 1
        game_idRows[trueLoop]
        print(paste("trueLoop:", trueLoop, sep = " "))
      }
    }
#}
  #write.csv(game_ids, file = paste("data/games_data/reg_season/reg_games_", userYear, ".csv", sep = ""),row.names=FALSE)
  #print(paste("Game IDs gathered for", userYear, sep = " "))
  #userYear = userYear + 1
#}

gameIDselector <- 1
gameIDpbploop <- gameIDvalue[gameIDselector]
season2019 <- pbp_data

while(trueLoop <= ntrue) {
  gameIDpbploop <- game_ids[trueLoop]
  print(gameIDpbploop)
  pbp_data <- scrape_json_play_by_play(gameIDpbploop)
  #write.csv(pbp_data, file= paste("data/games/", gameIDpbploop, ".csv", sep = ""))
  ##write.csv(pbp_data, file = "data/season_total/season2019.csv",row.names=FALSE)
  #season2019 <- read.csv(file = "data/season_total/season2019.csv")
  season2019 <- rbind(season2019, pbp_data)
  write.csv(season2019, "data/season_total/season2019.csv")
  print(paste("Play by play gathered for", gameIDpbploop, sep = " "))
  trueLoop = trueLoop + 1
}


class(game_ids$game_id)

game_ids <- scrape_game_ids(userYear, weeks = userWeek)
game_idRows <- rownames(game_ids)
game_ids %>%
  pander::pander()
week2 <- game_ids %>%
  filter(game_id == date) %>%
  pull(game_id) %>%
  scrape_json_play_by_play()
  
