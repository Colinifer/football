# Packages & Init Setup ---------------------------------------------------

# devtools::install_github("mrcaseb/nflfastR")
# devtools::install_github("dynastyprocess/ffscrapr")
# devtools::install_github("jthomasmock/espnscrapeR")
# devtools::install_github("colinifer/initR", auth_token = authtoken)
proj_name <- "football"
pkgs <- c(
  "devtools",
  "tidyverse",
  "nflfastR",
  "gsisdecoder",
  "espnscrapeR",
  "DBI",
  "odbc",
  "RMariaDB",
  "arrow",
  "shiny",
  "distill",
  "httr",
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
  "reactable",
  "png",
  "DT",
  "ggthemes",
  "ggforce",
  "ggridges",
  "ggrepel",
  "ggpmisc",
  "ggbeeswarm",
  "cowplot",
  "gridExtra",
  "grid",
  "extrafont",
  "shadowtext",
  "viridis",
  "tidytext",
  "RCurl",
  "pracma",
  'initR'
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

# Detach all packages
# lapply(paste0('package:', names(sessionInfo()$otherPkgs)), detach, character.only=TRUE, unload=TRUE)

rm(pkgs, installed_packages)

`%notin%` <- Negate(`%in%`)

# source("../initR/init.R")
fx.setdir(proj_name)

# Create standard objects -------------------------------------------------

# Connect to DB
# Based on NAS sleep schedule
if ((
  Sys.Date() %>% lubridate::wday() > 1 & # If day is greater than Sunday
  Sys.Date() %>% lubridate::wday() < 6 & # and day is less than Saturday
  Sys.time() %>% format("%H") %>% as.integer() >= 17 & # and greater than 5PM
  Sys.time() %>% format("%H") %>% as.integer() <= 23 # and less than 12AM
) == TRUE) {
  source("../initR/con.R")
  dbListTables(con)
  dbDisconnect(con)
}

# Create variables --------------------------------------------------------
fx.get_sleeper_api_players()
# source("fantasy_football/ff_init.R")
fx.get_espn_players()

# nflfastR data
schedule_df <- fast_scraper_schedules(seasons = year, pp = TRUE)

schedule_df %>% 
  saveRDS(glue('data/schedules/sched_{year}.rds'))
  
matchup_df <- schedule_df %>% 
  mutate(posteam = home_team,
         oppteam = away_team) %>%
  select(
    game_id,
    season,
    game_type,
    week,
    gameday,
    weekday,
    gametime,
    posteam,
    oppteam,
    away_team,
    home_team,
    away_score,
    home_score,
    home_result,
    stadium,
    location,
    roof,
    surface,
    old_game_id
  ) %>% rbind(
    schedule_df %>%
      mutate(posteam = away_team,
             oppteam = home_team) %>%
      select(
        game_id,
        season,
        game_type,
        week,
        gameday,
        weekday,
        gametime,
        posteam,
        oppteam,
        away_team,
        home_team,
        away_score,
        home_score,
        home_result,
        stadium,
        location,
        roof,
        surface,
        old_game_id
      )
  ) %>% arrange(old_game_id)

sr_games_df <- readRDS('data/schedules/sportradar/games_2020.rds')

# source('data/master_sr_pbp.R')
pbp_df <- readRDS(glue('data/pbp/play_by_play_{year}.rds'))
pbp_df %>% select(game_date) %>% arrange(game_date) %>%  unique() %>%  tail()
pbp_df %>% select(game_id) %>% unique() %>% tail()
# pbp_df <-
#   readRDS(glue('data/pbp/play_by_play_{year}.rds')) %>%
#   decode_player_ids(fast = T)
# pbp_df <-
#   readRDS(url(
#     glue(
#       'https://github.com/guga31bb/nflfastR-data/blob/master/data/play_by_play_{year}.rds?raw=true'
#     )
#   )) %>%
#   decode_player_ids(fast = T)

# Participation dataframe
sr_part_df <- readRDS(glue('data/part/Sportradar_Part_{year}.rds'))

pbp_ds <- open_dataset('data/pbp/fastr', partitioning = 'year')
xyac_ds <- open_dataset('data/pbp/xyac', partitioning = 'year')

source('plots/assets/plot_theme.R')
# source('plots/scripts/team_tiers.R')
# source('plots/scripts/dakota_career.R')
# source('plots/scripts/qb_cayoe.R')
# source('fantasy_football/xfantasy_points.R')
# source('fantasy_football/xfantasy_points_test_theme.R')
# source('plots/scripts/espn_winrate.R')
# source('plots/scripts/defense_cpoe_adot.R')
# source('plots/scripts/defense_epa_cpoe.R')
# rm(list = ls())

