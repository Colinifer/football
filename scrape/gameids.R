##install.packages("devtools", "tidyverse", "readr")
##devtools::install_github(repo = "maksimhorowitz/nflscrapR")

library(nflscrapR)
library(tidyverse)
library(readr)


#season play by play
pbp_data <- read.csv(file ="data/season_total/season2019.csv")
pbp_data <- scrape_season_play_by_play(2019)
write.csv(pbp_data, file = "data/season_total/season2019.csv",row.names=FALSE)


##make a function
##
##game ID scrape
##
games <- read.csv("data/games/game_ids2019.csv")

##Get Game IDs for given week
week <- 1
selectedWeek <- games$week == week


pbp_data <- scrape_json_play_by_play(2019090901)
write.csv(pbp_data, file= paste("data/games/", 2019090901, ".csv", sep = ""))

###############

#create new data.frame
game_id_loop <- game_ids2019[selectedWeek,]

#sort by game ID
sort(game_id_loop$game_id, decreasing = FALSE, na.last = TRUE)

gameIDvalue <- game_id_loop$game_id

length(gameIDvalue)


#get postgame only
game_ids2019 <- read.csv("data/games/game_ids2019.csv")
pg_id <- game_id_loop$state_of_game == "POST"
postgame_id <- game_id_loop[pg_id,]

sort(postgame_id$game_id, decreasing = FALSE, na.last = TRUE)

n_complete_games <- length(which(pg_id))


gameIDselector <- 1
gameIDpbploop <- gameIDvalue[gameIDselector]
season2019 <- pbp_data

while(gameIDselector <= n_complete_games) {
  gameIDpbploop <- gameIDvalue[gameIDselector]
  print(gameIDpbploop)
  pbp_data <- scrape_json_play_by_play(gameIDpbploop)
  #write.csv(pbp_data, file= paste("data/games/", gameIDpbploop, ".csv", sep = ""))
  ##write.csv(pbp_data, file = "data/season_total/season2019.csv",row.names=FALSE)
  #season2019 <- read.csv(file = "data/season_total/season2019.csv")
  season2019 <- rbind(season2019, pbp_data)
  write.csv(season2019, "data/season_total/season2019.csv")
  print(paste("Play by play gathered for", gameIDpbploop, sep = " "))
  gameIDselector = gameIDselector + 1
}

pbp_data <- scrape_json_play_by_play(2019090901)
write.csv(pbp_data, file= paste("data/games/", 2019090901, ".csv", sep = ""))


game_ids <- scrape_game_ids(selectedYear, weeks = selectedWeeks)
write.csv(game_ids, file = paste("data/games_data/reg_season/reg_games_", selectedYear, ".csv", sep =""),row.names=FALSE)


pbp_data <- scrape_json_play_by_play(2019090806)
write.csv(pbp_data, file = "football/data/games/2019090806.csv",row.names=FALSE)
write.csv(pbp_data, file = "football/data/season_total/season2019.csv",row.names=FALSE)
season2019 <- read.csv(file = "football/data/season_total/season2019.csv")
merge(pbp_data, season2019, all.y = TRUE)

##  Note: on the pbp_data line, replace "2018091700" with the GameID of the game you want. To find a gameid:
##    Go to the week of the game you want on nfl.com, example: https://www.nfl.com/scores/2018/REG3
##  Click on the game you want, for Jets-Browns, example: https://www.nfl.com/gamecenter/2018092000/2018/REG3/jets@browns
##  The GameID is the set of numbers between the first /    / after gamecenter (in this case, 2018092000)

##  5. If you don't want to deal with R at all:
##  CSV files here: https://github.com/ryurko/nflscrapR-data/tree/master/play_by_play_data/regular_season
##  Note: these files do not update in real time like pulling with R does

pbp_18 <- read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_2018.csv")
write.csv(pbp_18, file = "/Users/ColinWelsh/Documents/dev/GitHub/ff19/scrape/season2018.csv",row.names=FALSE)


data.frame(pbp_data)
