# Packages & Init Setup ---------------------------------------------------

# devtools::install_github(repo = "maksimhorowitz/nflscrapR")
# devtools::install_github("mrcaseb/nflfastR")
# devtools::install_github("dynastyprocess/ffscrapr")

pkgs <- c(
  "devtools",
  "tidyverse",
  "readr",
  "pander",
  "furrr",
  "na.tools",
  "ggimage",
  "teamcolors",
  "glue",
  "dplyr",
  "tictoc",
  "animation",
  "gt",
  "DT",
  "ggthemes",
  "ggforce",
  "ggridges",
  "ggrepel",
  "ggbeeswarm",
  "extrafont",
  "tidytext",
  "RCurl"
)
installed_packages <- pkgs %in%
  rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(pkgs[!installed_packages])
}
lapply(pkgs, library, character.only = TRUE)
# library("nflscrapR") # doesn't work anymore
library("nflfastR")
library("ffscrapr")

##reset
setwd("~/")
gid <- paste(getwd())
gid
device <- ""

if (gid == "/Volumes/HDD/Users/colinwelsh") {
  ## Maverick - MBP
  setwd("~/Documents/dev/football")
  device <- "Maverick (MBP)"
} else if (gid == "/Users/ColinWelsh") {
  ## Goose - iMac
  setwd("~/Documents/dev/football")
  device <- "Goose (iMac)"
} else if (gid == "/home/rstudio-user") {
  ## RStudio Cloud
  setwd("/cloud/project")
  device <- "RStudio Cloud"
}
print(paste(device, "is ready for some football", sep = " "))
rm(gid, device)


# Create standard objects -------------------------------------------------

teams_colors_logos <- teams_colors_logos
today <- format(Sys.Date(), "%Y/%d/%m")
years <- c(2000:2019)

get_pbp <- function(x) {
  paste0("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_", x, ".rds") %>% 
    url() %>% 
    readRDS()
}

pbp_list <- years %>% 
  lapply(get_pbp)

pbp_db <- do.call(rbind, pbp_list)


# Live scrape # need games on date in vector ------------------------------

schedules <- fast_scraper_schedules(years, pp = TRUE)

pbp <- fast_scraper(schedules$game_id, source = "nfl", pp = TRUE) %>% 
  clean_pbp() %>% 
  calculate_expected_points() %>% 
  calculate_win_probability()

pbp <- clean_pbp(pbp) %>% 
  calculate_expected_points() %>% 
  calculate_win_probability()