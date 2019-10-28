##install.packages("devtools", "tidyverse", "readr", "pander", "na.tools", "ggimage", "devtools", "teamcolors")
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

fgame_ids <- paste("data/games_data/reg_season/reg_games_", userYear, ".csv", sep ="")
