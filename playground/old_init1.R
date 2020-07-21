## devtools::install_github(repo = "maksimhorowitz/nflscrapR")

pkgs <- c("devtools", "tidyverse", "readr",
          "pander", "na.tools", "ggimage",
          "devtools", "teamcolors", "glue",
          "dplyr", "tictoc", "animation", 
          "gt", "DT", "ggthemes", 
          "bbplot", "ggtext", "ggforce", 
          "ggridges", "ggrepel", "ggbeeswarm", 
          "extrafont")
installed_packages <- pkgs %in%
  rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(pkgs[!installed_packages])
}
invisible(lapply(pkgs, library, character.only = TRUE))
library("nflscrapR")


##install.packages(c("devtools", "tidyverse", "readr", "pander", "na.tools", "ggimage", "devtools", "teamcolors", "glue", "animate", "dplyr", "tictoc", "animation"))
##devtools::install_github(repo = "maksimhorowitz/nflscrapR")

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
##  date <- 202001
date <- format(today, format="%Y%m%d")
u.year <- 2019 ##necessary for saved 
u.week <- 17 ##not necessary at the moment
seasonState <- "reg"
f.game_ids <- paste("data/games/", seasonState, "_season/", seasonState, "_games_", u.year, ".csv", sep ="")
f.pbp_season <- paste("data/season_total/", u.year,"pbp.csv", sep = "")
f.player_season <- paste("data/season_total/", u.year, "players.csv", sep = "")
f.roster_season <- paste("data/season_total/", u.year, "roster.csv", sep = "")
f.team_abbr <- paste("data/season_total/team_abbr.csv", sep = "")

teams <- read.csv(f.team_abbr)
game_ids <- read.csv(f.game_ids)
## Leave this out until season start
## game_ids <- scrape_game_ids(u.year, type = seasonState)
## write.csv(game_ids, f.game_ids)


## GET FUNCTIONS

source("functions/scrapePBP.R")
source("functions/addTargets.R")
scrapePBP(date)



pbp_season <- list.files(paste("data/games/", u.year, "/", sep = ""),
                      pattern = "*.csv", full.names = TRUE) %>%
  lapply(read_csv) %>%
  bind_rows
pbpSeason
write.csv(pbp_season, file = f.pbp_season, row.names = FALSE)

player_season <- list.files(paste("data/players/", u.year, "/", sep = ""),
                      pattern = "*.csv", full.names = TRUE) %>%
  lapply(read_csv) %>%
  bind_rows
player_season
write.csv(player_season, file = f.player_season, row.names = FALSE)

roster_season <- list.files(paste("data/teams/", u.year, "/", sep = ""),
                           pattern = "*.csv", full.names = TRUE) %>%
  lapply(read_csv) %>%
  bind_rows
rosters_season
write.csv(rosterSeason, file = f.roster_season, row.names = FALSE)
