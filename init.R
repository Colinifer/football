# Packages & Init Setup ---------------------------------------------------

# devtools::install_github("mrcaseb/nflfastR")
# devtools::install_github("dynastyprocess/ffscrapr")
proj_name <- "football"

pkgs <- c(
  "devtools",
  "tidyverse",
  "DBI",
  "odbc",
  "RMariaDB",
  "readr",
  "pander",
  "furrr",
  "na.tools",
  "ggimage",
  "teamcolors",
  "glue",
  "dplyr",
  "jsonlite",
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
# library("ffscrapr")

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
rm(gid)


# Create standard objects -------------------------------------------------
source("../initR/con.R") # set condition based on device to update local vs remote connections to document
length(dbListTables(con))
dbTables <- dbListTables(con)[1:length(dbListTables(con))]
dbListObjects(con)
dbListFields(con, "pbp")
dbListFields(con, "schedule")

teams_colors_logos <- teams_colors_logos
today <- format(Sys.Date(), "%Y/%d/%m")
years <- c(2000:2019)


# PBP ---------------------------------------------------------------------

get_pbp <- function(x) {
  paste0("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_", x, ".rds") %>% 
    url() %>% 
    readRDS()
}

f.pbp_db_rds <- paste0("data/pbp_db.rds")
f.pbp_db_csv <- paste0("data/pbp_db.csv")

ifelse(
  file.exists(f.pbp_db) == TRUE,
  pbp_db <-
    readRDS(f.pbp_db_rds),
  pbp_db <-
    rbind %>%
    do.call(years %>%
              lapply(get_pbp)) %>%
    saveRDS(f.pbp_db_rds)
)

# convert rds to csv
write_csv(readRDS(f.pbp_db_rds), f.pbp_db_csv)

# write table
RMariaDB::dbWriteTable(con, "pbp", readRDS(f.pbp_db_rds), overwrite = TRUE)

# Schedule ----------------------------------------------------------------

RMariaDB::dbWriteTable(con, "schedule", fast_scraper_schedules(years, pp = TRUE))

# Roster data -------------------------------------------------------------

roster_legacy_map <- paste0("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/roster-data/legacy_id_map.rds") %>% 
  url() %>% 
  readRDS()

f.roster_legacy_map <- paste0("data/roster_legacy_map.rds")
saveRDS(roster_legacy_map, f.roster_legacy_map)
RMariaDB::dbWriteTable(con, "roster_legacy_data", readRDS(f.roster_legacy_map))


roster <- paste0("https://raw.github.com/guga31bb/nflfastR-data/master/roster-data/roster.rds") %>% 
  url() %>% 
  readRDS()

f.roster <- paste0("data/roster.rds")
saveRDS(roster, f.roster)
RMariaDB::dbWriteTable(con, "roster", readRDS(f.roster))

# Obsolete code, consolidated by function above
# This is useful if you need the pbp_list
# 
# pbp_list <- years %>% 
#   lapply(get_pbp)
# 
# pbp_db <- do.call(rbind, pbp_list)
# 
# f.pbp_db <- paste0("data/pbp_db.rds")
# pbp_db %>% saveRDS(file = f.pbp_db)


# Live scrape # need games on date in vector ------------------------------

schedules <- fast_scraper_schedules(years, pp = TRUE)