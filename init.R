pkgs <- c("devtools", "tidyverse", "readr", "pander", "na.tools", "ggimage", "devtools", "teamcolors", "glue", "animate", "dplyr", "tictoc", "animation")

##install.packages(c("devtools", "tidyverse", "readr", "pander", "na.tools", "ggimage", "devtools", "teamcolors", "glue", "animate", "dplyr", "tictoc", "animation"))
##devtools::install_github(repo = "maksimhorowitz/nflscrapR")

library(nflscrapR)
library(tidyverse)
library(readr)
library(pander)
library(dplyr)
library(na.tools)
library(ggimage)
library(teamcolors)
library(plyr)
library(readr)
library(glue)
library(animate)
library(animation)
library(tictoc)

##reset
setwd("~/")
gid <- paste(getwd())
gid
device <- ""

if (gid == "/Volumes/HDD/Users/colinwelsh") {
  ##Maverick - MBP
  setwd("~/Documents/dev/football")
  device <- "Maverick (MBP)"
} else if (gid == "/Users/ColinWelsh") {
  ##Goose - iMac
  setwd("~/Documents/dev/football")
  device <- "Goose (iMac)"
  ##add Goose
} 
print(paste(device, "is ready for some football", sep = " "))
rm(gid, device)


# other dependent variables
today <- Sys.Date()

#   test date
##  date <- 201912
date <- format(today, format="%Y%m%d")
userYear <- 2019 ##necessary for saved 
userWeek <- 17 ##not necessary at the moment
seasonState <- "post"
fgame_ids <- paste("data/games/", seasonState, "_season/", seasonState, "_games_", userYear, ".csv", sep ="")
game_ids <- read.csv(fgame_ids)
game_ids <- scrape_game_ids(userYear, type = seasonState)
write.csv(game_ids, fgame_ids)

source("functions/scrapePBP.R")
source("functions/addTargets.R")
scrapePBP(date)

pbpSeason <- list.files(paste("data/games/", userYear, "/", sep = ""),
                      pattern = "*.csv", full.names = TRUE) %>%
  lapply(read_csv) %>%
  bind_rows
pbpSeason
write.csv(pbpSeason, file = paste("data/season_total/", userYear,"pbp.csv", sep = ""), row.names=FALSE)

playerSeason <- list.files(paste("data/players/", userYear, "/", sep = ""),
                      pattern = "*.csv", full.names = TRUE) %>%
  lapply(read_csv) %>%
  bind_rows
playerSeason
write.csv(playerSeason, file = paste("data/season_total/", userYear, "players.csv", sep = ""), row.names=FALSE)

rosterSeason <- list.files(paste("data/teams/", userYear, "/", sep = ""),
                           pattern = "*.csv", full.names = TRUE) %>%
  lapply(read_csv) %>%
  bind_rows
rosterSeason
write.csv(rosterSeason, file = paste("data/season_total/", userYear, "roster.csv", sep = ""), row.names=FALSE)
