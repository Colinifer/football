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
##library(animate)
library(animation)
library(tictoc)

# other dependent variables
today <- Sys.Date()

#   test date
##  date <- 201912
date <- format(today, format="%Y%m%d")
userYear <- 2019 ##necessary for saved 
userWeek <- 17 ##not necessary at the moment
seasonState <- "post"
fgame_ids <- paste("data/games/", seasonState, "_season/", seasonState, "_games_", userYear, ".csv", sep ="")
fteamabbr <- paste("data/season_total/team_abbr.csv", sep = "")
teams <- read.csv(fteamabbr)
game_ids <- read.csv(fgame_ids)
game_ids <- scrape_game_ids(userYear, type = seasonState)
write.csv(game_ids, fgame_ids)

positions <- c(
  "QUARTERBACK", "RUNNING_BACK", "WIDE_RECEIVER",
  "TIGHT_END", "DEFENSIVE_LINEMAN", "LINEBACKER",
  "DEFENSIVE_BACK", "KICKOFF_KICKER", "KICK_RETURNER",
  "PUNTER", "PUNT_RETURNER", "FIELD_GOAL_KICKER"
)

for (x in teams$nflscrapr_abbrev[20:nrow(teams)]) {
  print(paste("Scraping ", x, " ", userYear, " roster. ", "Row #", which(teams$nflscrapr_abbrev == x), sep = ""))
  for (p in positions) {
    roster <- get_season_rosters(userYear, teams = x, positions = p)
    froster <- paste("data/teams/", userYear, "/", x, userYear, "roster.csv", sep = "") #ARI2019roster.csv
    if (file.exists(froster)==TRUE) {
      roster <- bind_rows(roster, read.csv(froster))
      roster <- roster[!duplicated(roster),]
    } else  {
      
    }
    write.csv(roster, froster, row.names = FALSE)
  }
}