##install.packages("devtools", "tidyverse", "readr")
##devtools::install_github(repo = "maksimhorowitz/nflscrapR")

library(nflscrapR)
library(tidyverse)
library(readr)


##Loop through game IDs and scrap json
# get season game IDs
userYear <- 2017
userWeek <- 1:17
while (userYear <= 2019) {
  print(userYear)
  game_ids <- scrape_game_ids(userYear, weeks = userWeek)
  write.csv(game_ids, file = paste("data/games_data/reg_season/reg_games_", userYear, ".csv", sep =""),row.names=FALSE)
  print(paste("Game IDs gathered for", userYear, sep = " "))
  userYear = userYear + 1
}
