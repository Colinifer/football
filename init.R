# Packages & Init Setup ---------------------------------------------------

# devtools::install_github("nflverse/nflfastR")
# devtools::install_github("guga31bb/nfl4th")
# devtools::install_github("saiemgilani/cfbfastR")
# devtools::install_github("dynastyprocess/ffscrapr")
# devtools::install_github("jthomasmock/espnscrapeR")
# devtools::install_github("colinifer/initR", auth_token = Sys.getenv('authtoken'))
# devtools::install_github('gregce/ipify')
proj_name <- 'football'
pkgs <- c(
  'devtools',
  'tidyverse',
  'nflfastR',
  'nflreadr',
  'cfbfastR',
  'nfl4th',
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
  'webshot',
  'gridExtra',
  'grid',
  'extrafont',
  'shadowtext',
  'viridis',
  'tidytext',
  'RCurl',
  'pracma',
  'DescTools',
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

source('plots/assets/plot_theme.R', echo = F)
# source('data/fastr_scrape.R')
source('https://raw.githubusercontent.com/nflverse/nflfastR/master/R/utils.R')
source('https://raw.githubusercontent.com/nflverse/nflfastR/master/R/aggregate_game_stats.R')
source('https://raw.githubusercontent.com/nflverse/nflfastR/master/R/helper_add_xyac.R')
source('https://raw.githubusercontent.com/nflverse/nflfastR/master/R/helper_add_nflscrapr_mutations.R')
source('data/fastr_mods.R')
# source('data/cfb_fastr_mods.R')

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

# Create variables & dataframes -------------------------------------------
# sleeper_players_df <- fx.get_sleeper_api_players()
# source("fantasy_football/ff_init.R")
# espn_players_df <- fx.get_espn_players() # not working, relies on roster load

# nflfastR data
con <- fx.db_con(x.host = 'localhost')
# update_roster_db(season = year, db_connection = fx.db_con(x.host = 'localhost'))
roster_df <- tbl(con, 'nflfastR_rosters') %>% 
  filter(season == year) %>%
  collect()
# update_schedule_db(season = year, db_connection = fx.db_con(x.host = 'localhost'))
schedule_df <- tbl(con, 'nflfastR_schedule') %>% 
  filter(season == year) %>% 
  collect()
# update_trades_db(season = year, db_connection = fx.db_con(x.host = 'localhost'))
trades_df <- tbl(con, 'nflfastR_trades') %>% 
  filter(season == year) %>% 
  collect()
# update_draft_db(season = year, db_connection = fx.db_con(x.host = 'localhost'))
draft_df <- tbl(con, 'nflfastR_draft') %>% 
  filter(season == year) %>% 
  collect()

pbp_df <- tbl(con, 'nflfastR_pbp') %>% 
  filter(season == year) %>% 
  collect()
dbDisconnect(con)


source('init_ff.R')
# Rosters
fantasy_rosters <- ff_rosters(ff_conn_beep_boop) %>%
  mutate(on_roster = TRUE,
         league = 'Beep Boop') %>%
  rbind(ff_rosters(ff_conn_drinkers) %>%
          mutate(on_roster = TRUE,
                 league = 'Drinkers')) %>%
  rbind(ff_rosters(ff_conn_kepler) %>%
          mutate(on_roster = TRUE,
                 league = 'Kepler')) %>%
  rbind(ff_rosters(ff_conn_family) %>%
          mutate(on_roster = TRUE,
                 league = 'Family')) %>% 
  left_join(roster_df %>% 
              select(
                gsis_id,
                espn_id),
            by = c('player_id' = 'espn_id'))

# schedule_df %>% 
#   saveRDS(glue('data/schedules/sched_{year}.rds'))

matchup_df <- schedule_df %>% 
  filter(season == year) %>% 
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

# sr_games_df <- readRDS(glue('data/schedules/sportradar/games_{year}.rds'))
# source('data/master_sr_pbp.R')


# Deprecated on M1 chipset
# part_ds <- open_dataset('data/part/sportradar', partitioning = 'year')
# pbp_ds <- open_dataset('data/pbp/fastr', partitioning = 'year')
# xyac_ds <- open_dataset('data/pbp/xyac', partitioning = 'year')
# sr_pbp_df <- readRDS('data/pbp/sportradar/sr_pbp_2020.rds')


# pbp_df <- readRDS(url("https://github.com/guga31bb/nflfastR-data/blob/master/data/play_by_play_2020.rds?raw=true"))
team_stats <- pbp_df %>%
  calculate_team_stats_mod() %>%
  left_join(
    pbp_df %>% 
      group_by(posteam) %>% 
      rename(team = posteam) %>% 
      filter(play_type %in% c('pass', 'run')) %>% 
      summarise(
        offense_snaps = n()
        )
  ) %>% 
  left_join(
    pbp_df %>% 
      group_by(defteam) %>% 
      rename(team = defteam) %>% 
      filter(play_type %in% c('pass', 'run')) %>% 
      summarise(
        defense_snaps = n()
      )
  )

team_stats_weekly <- pbp_df %>% 
  calculate_team_stats_mod(weekly = TRUE) %>% 
  left_join(
    pbp_df %>% 
      group_by(posteam, game_id) %>% 
      rename(team = posteam) %>% 
      filter(play_type %in% c('pass', 'run')) %>% 
      summarise(
        offense_snaps = n()
      ),
    by = c('team', 'game_id')
  ) %>% 
  left_join(
    pbp_df %>% 
      group_by(defteam, game_id) %>% 
      rename(team = defteam) %>% 
      filter(play_type %in% c('pass', 'run')) %>% 
      summarise(
        defense_snaps = n()
      ),
    by = c('team', 'game_id')
  )

player_stats <- pbp_df %>% 
  calculate_player_stats_mod() 

ff_free_agents <- fx.ff_free_agents(player_stats, 'Beep Boop')

  # %>%
  # left_join(
  #   roster_df %>%
  #     select(season, gsis_id, pfr_id),
  #   by = c('player_id' = 'gsis_id')
  # ) %>%
  # left_join(
  #   nflreadr::load_snap_counts(),
  #   by = c('pfr_id', 'game_id')
  # )
  
player_stats_weekly <- pbp_df %>% 
  calculate_player_stats_mod(weekly = TRUE)



# Update DBs --------------------------------------------------------------

nflfastR::update_db(
  tblname = "nflfastR_pbp",
  force_rebuild = FALSE,
  db_connection = initR::fx.db_con(x.host = 'localhost')
)

nflfastR::update_db(
  tblname = "nflfastR_pbp",
  force_rebuild = FALSE,
  db_connection = initR::fx.db_con()
)

# source('https://raw.githubusercontent.com/saiemgilani/cfbfastR/master/R/cfb_pbp.R') # used for update_cfb_db_mod function
cfbfastR::update_cfb_db(
  tblname = 'cfbfastR_pbp',
  force_rebuild = FALSE,
  db_connection = initR::fx.db_con(x.host = 'localhost')
)

cfbfastR::update_cfb_db(
  tblname = 'cfbfastR_pbp',
  force_rebuild = FALSE,
  db_connection = initR::fx.db_con()
)
 
