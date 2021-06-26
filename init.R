# Packages & Init Setup ---------------------------------------------------

# devtools::install_github("nflverse/nflfastR")
# devtools::install_github(repo = "saiemgilani/cfbfastR")
# devtools::install_github("dynastyprocess/ffscrapr")
# devtools::install_github("jthomasmock/espnscrapeR")
# devtools::install_github("colinifer/initR", auth_token = Sys.getenv('authtoken'))
proj_name <- 'football'
pkgs <- c(
  'devtools',
  'tidyverse',
  'nflfastR',
  'cfbfastR',
  'ffscrapr',
  'gsisdecoder',
  'espnscrapeR',
  'DBI',
  'odbc',
  'RPostgres',
  'arrow',
  'shiny',
  'qs',
  'distill',
  'httr',
  'readr',
  'pander',
  'furrr',
  'na.tools',
  'ggimage',
  'teamcolors',
  'glue',
  'dplyr',
  'jsonlite',
  'tictoc',
  'animation',
  'gt',
  'reactable',
  'png',
  'DT',
  'ggthemes',
  'ggforce',
  'ggridges',
  'ggrepel',
  'ggpmisc',
  'ggbeeswarm',
  'cowplot',
  'gridExtra',
  'grid',
  'extrafont',
  'shadowtext',
  'viridis',
  'tidytext',
  'RCurl',
  'pracma',
  'initR'
)
installed_packages <- pkgs %in%
  rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(pkgs[!installed_packages])
}
lapply(pkgs, library, character.only = TRUE)

# Detach all packages
# lapply(paste0('package:', names(sessionInfo()$otherPkgs)), detach, character.only=TRUE, unload=TRUE)

rm(pkgs, installed_packages)

options(tibble.print_min=25)

`%notin%` <- Negate(`%in%`)

# source("../initR/init.R")
fx.setdir(proj_name)

current_season <- fx.get_year()
year <- fx.get_year()

# Create standard objects -------------------------------------------------

# Based on NAS sleep schedule
# if ((
#   Sys.Date() %>% lubridate::wday() > 1 & # If day is greater than Sunday
#   Sys.Date() %>% lubridate::wday() < 6 & # and day is less than Saturday
#   Sys.time() %>% format("%H") %>% as.integer() >= 17 & # and greater than 5PM
#   Sys.time() %>% format("%H") %>% as.integer() <= 23 # and less than 12AM
# ) == TRUE) {
#   con <- fx.db_con()
#   # source("../initR/con.R")
#   dbListTables(con)
#   dbDisconnect(con)
# }

update_db(
  tblname = "nflfastR_pbp",
  force_rebuild = FALSE,
  db_connection = fx.db_con(), 
)

# Create variables --------------------------------------------------------
fx.get_sleeper_api_players()
# source("fantasy_football/ff_init.R")
fx.get_espn_players()

# nflfastR data
roster_df <-
  fast_scraper_roster(1999:2020)

schedule_df <- fast_scraper_schedules(seasons = year, pp = FALSE)

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
pbp_df <- readRDS(glue('data/pbp/play_by_play_{current_season}.rds'))
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
# sr_part_df <- do.call(
#   rbind, 
#   lapply(
#     dir(
#       glue('data/part/'), 
#       pattern = glue('{year}.rds'), 
#       full.names = T), 
#     readRDS)
#   )

# part_ds <- open_dataset('data/part/sportradar', partitioning = 'year')
# pbp_ds <- open_dataset('data/pbp/fastr', partitioning = 'year')
# xyac_ds <- open_dataset('data/pbp/xyac', partitioning = 'year')
# sr_pbp_df <- readRDS('data/pbp/sportradar/sr_pbp_2020.rds')

source('plots/assets/plot_theme.R', echo = F)

map(
  dir(path = 'plots/scripts', full.names = TRUE)[1], 
  source
  )

source('plots/scripts/season_point_differential.R', echo = F)
source('plots/scripts/team_tiers.R', echo = F)
source('plots/scripts/wins_above_expectation.R', echo = F)
source('plots/scripts/team_run_pass_efficiency.R', echo = F)
source('plots/scripts/dakota_career.R', echo = F)
source('plots/scripts/qb_cayoe.R', echo = F)
source('plots/scripts/qb_cpoe_adot.R', echo = F)
source('plots/scripts/qb_epa_cpoe.R', echo = F)
source('plots/scripts/wr_cayoe.R', echo = F)
# source('fantasy_football/xfantasy_points.R')
# source('fantasy_football/xfantasy_points_test_theme.R')
# source('plots/scripts/espn_winrate.R')
source('plots/scripts/defense_cpoe_adot.R', echo = F)
source('plots/scripts/defense_epa_cpoe.R', echo = F)
source('plots/scripts/defense_cayoe.R', echo = F)
source('plots/scripts/epa_winrate_efficiency.R', echo = F)
# rm(list = ls())

