install.packages("devtools")
devtools::install_github(repo = "maksimhorowitz/nflscrapR")

library(nflscrapR)
library(tidyverse)
setwd("/Users/ColinWelsh/Documents/dev/")
pbp_data <- scrape_season_play_by_play(2019)
write.csv(pbp_data, file = "/GitHub/ff19/scrape/season2019.csv",row.names=FALSE)

##  Note: the pbp_data line will take a long time; that's normal. On the write.csv line, change the part in "quotes"
##  to match where you want to save.

##  4. To get ONE GAME of data (lots faster). In R (again, change part in quotes to save to directory on your computer)

library(nflscrapR)
library(tidyverse)

# get season game IDs
game_ids2019 <- scrape_game_ids(2019, weeks = 1:17)
game_ids2018 <- scrape_game_ids(2018, weeks = 1:17)
write.csv(game_ids2019, file = "football/data/games/game_ids2019.csv",row.names=FALSE)
write.csv(game_ids2018, file = "football/data/games/game_ids2018.csv",row.names=FALSE)

pbp_data <- scrape_json_play_by_play(2019090500)
write.csv(pbp_data, file = "football/data/games/2019090500.csv",row.names=FALSE)
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
