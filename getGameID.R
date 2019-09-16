##install.packages("devtools", "tidyverse", "readr")
##devtools::install_github(repo = "maksimhorowitz/nflscrapR")

library(nflscrapR)
library(tidyverse)
library(readr)


##Loop through game IDs and scrap json
# get season game IDs


userYear <- 2019
userWeek <- 2
today <- Sys.Date()
date <- 20190915
date <- format(today, format="%Y%m%d")
trueLoop <- 1
currentGameIDs <- game_ids$game_id
currentGames <- grepl(date, currentGameIDs)
nTRUE <- length(which(currentGames==TRUE))
nGames <- game_idRows[trueLoop]
currentGameIDs[trueLoop]
while (userYear <= 2019) {
  print(userYear)
  game_ids <- scrape_game_ids(userYear, weeks = userWeek)
  #if (file.exists("data/games_data/reg_season/reg_games_2019.csv")) {
    while (trueLoop <= 16) {
      if (currentGames[trueLoop] == TRUE) {
        print(paste("Scraping game ", currentGameIDs[trueLoop], sep = ""))
        pbp <- scrape_json_play_by_play(currentGameIDs[trueLoop])
        write.csv(pbp, file = paste("data/games_data/", userYear,"/", currentGameIDs[trueLoop], ".csv", sep = ""))
        trueLoop <- trueLoop + 1
        game_idRows[trueLoop]
        print(paste("trueLoop:", trueLoop, sep = " "))
      } else if (currentGames[trueLoop] == FALSE) { 
        trueLoop <- trueLoop + 1
        game_idRows[trueLoop]
        print(paste("trueLoop:", trueLoop, sep = " "))
      }
    }
}
  #write.csv(game_ids, file = paste("data/games_data/reg_season/reg_games_", userYear, ".csv", sep = ""),row.names=FALSE)
  #print(paste("Game IDs gathered for", userYear, sep = " "))
  #userYear = userYear + 1
#}

gameIDselector <- 1
gameIDpbploop <- gameIDvalue[gameIDselector]
season2019 <- pbp_data

while(trueLoop <= nTRUE) {
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
  
