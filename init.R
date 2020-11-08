# Packages & Init Setup ---------------------------------------------------

# devtools::install_github("mrcaseb/nflfastR")
# devtools::install_github("dynastyprocess/ffscrapr")
# devtools::install_github("jthomasmock/espnscrapeR")
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
  "RJSONIO",
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
  "tidytext",
  "RCurl",
  "pracma"
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

source("../initR/init.R")
fx.setdir(proj_name)

# Create standard objects -------------------------------------------------

# Connect to DB
# Based on NAS sleep schedule
if ((
  Sys.Date() %>% lubridate::wday() > 1 & # If day is greater than Sunday
  Sys.Date() %>% lubridate::wday() < 7 & # and day is less than Saturday
  Sys.time() %>% format("%H") %>% as.integer() >= 17 & # and greater than 5PM
  Sys.time() %>% format("%H") %>% as.integer() <= 23 # and less than 12AM
) == TRUE) {
  source("../initR/con.R")
  dbListTables(con)
  dbDisconnect(con)
}


# Fantasy variables -------------------------------------------------------

# Create variables --------------------------------------------------------

# ESPN Fantasy Football
swid  <-  "{2BA315B4-5941-4B1C-A315-B459416B1CC1}"
espn_s2 <- "AEBtGuDXUCKk6SpqlY71qdBDW%2BYc5KGa80m%2F0EVX9NCF%2FIFBM5b8ZMKgrMovpUeUqFTp4M%2BrPbM1I4rT1Ra2oXbM847nUp25DBY9Q%2FsAPChAykF5VNEZ05VjF6Vu3thAU0WkzQeBbjkdzNGqfbmPtMNzrBy8oV7fcAlwh4X89q4XlfPNED8ppKynNj5admyBk7WaqNzQtZJLlStpyOjz3F3d5BwUtQ8kh390OPB5HEEPfiH4%2FBftKqsLF%2BlyhTFaDiM%3D"
kona_v3_environment <- '{"leagueId":1034400,"seasonId":null}'
kona_v3_teamcontrol <- '{"leagueId":1034400,"seasonId":2020,"teamId":8}'

ff_fantasy_key <- "fantasy_football/data/fantasy_key.rds"
# league_id <- c("1034400", "62746259", "39973580")
# team_id <- c("8", "9", "10")
# league_name <- c("Colin's Minions", "Drinker's Slushy Beer", "Family League 3.0")
# team_name <- c("Rhule Tide", "Golden Rhule", "Matt Rhules")
# fantasy_key <- data.frame(league_id, league_name, team_id, team_name)
# fantasy_key %>% write_rds(ff_fantasy_key)
cookies = c(`SWID` = swid,
            `espn_s2` = espn_s2)
# 'kona_v3_environment_season_ffl' = kona_v3_environment,
# 'kona_v3_teamcontrol_ffl' = kona_v3_teamcontrol)
cookie <- paste(names(cookies), cookies, sep = "=", collapse = ";")
# fantasy_key <- ff_fantasy_key %>% read_csv()
fantasy_key <- ff_fantasy_key %>% 
  readRDS()
base = "https://fantasy.espn.com/apis/v3/games/ffl/seasons/"
year = Sys.Date() %>% 
  format(format = "%Y") %>% 
  as.integer()
mid = "/segments/0/leagues/"
leagueID <- fantasy_key$league_id[3]
tail = "?view=mDraftDetail&view=mLiveScoring&view=mMatchupScore&view=mPendingTransactions&view=mPositionalRatings&view=mSettings&view=mTeam&view=modular&view=mNav&view=mMatchupScore"
player_tail = "?view=kona_player_info"
wl_tail = "?view=proTeamSchedules_wl"
user_agent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.14; rv:79.0) Gecko/20100101 Firefox/79.0"


# Player data
fx.get_sleeper_api_players <- function() {
  sleeper_api_players_url <-
    'https://api.sleeper.app/v1/players/nfl'
  sleeper_api_players <-
    jsonlite::fromJSON(url('https://api.sleeper.app/v1/players/nfl'), flatten = T)
  na_map <-
    readRDS(
      url(
        "https://github.com/mrcaseb/nflfastR-roster/blob/master/R/na_map.rds?raw=true"
      )
    )
  
  # source("https://github.com/mrcaseb/nflfastR-roster/blob/master/R/update_roster.R")
  sleep.players <-
    purrr::map_dfr(sleeper_api_players, function(x)
      purrr::map(x, function(y)
        ifelse(is.null(y), NA, y))) %>%
    dplyr::na_if("") %>%
    dplyr::mutate_if(is.character, stringr::str_trim) %>%
    dplyr::filter(
      !(is.na(team) &
          is.na(gsis_id)),
      !player_id %in% nflfastR::teams_colors_logos$team_abbr,
      first_name != "Duplicate"
    ) %>%
    dplyr::left_join(na_map, by = c("sportradar_id" = "id")) %>%
    dplyr::mutate(
      gsis_id = dplyr::if_else(is.na(gsis_id), gsis, gsis_id),
      update_dt = lubridate::now("America/New_York"),
      season = dplyr::if_else(
        lubridate::month(update_dt) < 3,
        lubridate::year(update_dt) - 1,
        lubridate::year(update_dt)
      ),
      index = 1:dplyr::n(),
      headshot_url = dplyr::if_else(is.na(espn_id), NA_character_, as.character(
        glue::glue(
          "https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/{espn_id}.png"
        )
      ))
    )
  assign(x = 'sleeper_players_df', sleep.players, envir = globalenv())
}
fx.get_sleeper_api_players()

fx.get_espn_players <- function(u.league_id = 1034400) {
  # espn.league_id <- fantasy_key$league_id[u.league_id]
  files_list <- list.files("fantasy_football/data/free_agents/", pattern = paste0(u.league_id, 'players.*\\', '.rds')) 
  
  espn.players <- readRDS(
    paste0(
      "fantasy_football/data/free_agents/",
      list.files("fantasy_football/data/free_agents/", pattern = paste0(u.league_id, 'players.*\\', '.rds'))[list.files("fantasy_football/data/free_agents/", pattern = paste0(u.league_id, 'players.*\\', '.rds')) %>% length]
      )
    )
  assign(x = 'espn_players_df', espn.players, envir = globalenv())
}
fx.get_espn_players()


# nflfastR data
schedule_df <- fast_scraper_schedules(seasons = year, pp = TRUE)
  
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

pbp_df <-
  readRDS(url(
    glue(
      'https://github.com/guga31bb/nflfastR-data/blob/master/data/play_by_play_{year}.rds?raw=true'
    )
  )) %>%
  decode_player_ids(fast = T)

source('plots/assets/plot_theme.R')
# source("fantasy_football/ff_init.R")
# source('plots/scripts/team_tiers.R')
# source('plots/scripts/dakota_career.R')
# source('plots/scripts/qb_cayoe.R')
# source('fantasy_football/xfantasy_points.R')
# source('fantasy_football/xfantasy_points_test_theme.R')
# source('plots/scripts/espn_winrate.R')
# rm(list = ls())

update_db <- function (dbdir = ".", dbname = "pbp_db", tblname = "nflfastR_pbp", 
                       force_rebuild = FALSE, db_connection = NULL) 
{
  rule_header("Update nflfastR Play-by-Play Database")
  if (!requireNamespace("DBI", quietly = TRUE) | (!requireNamespace("MariaDB", 
                                                                    quietly = TRUE) & is.null(db_connection))) {
    usethis::ui_stop("Packages {usethis::ui_value('DBI')} and {usethis::ui_value('MariaDB')} needed for database communication. Please install them.")
  }
  if (any(force_rebuild == "NEW")) {
    usethis::ui_stop("The argument {usethis::ui_value('force_rebuild = NEW')} is only for internal usage!")
  }
  if (!(is.logical(force_rebuild) | is.numeric(force_rebuild))) {
    usethis::ui_stop("The argument {usethis::ui_value('force_rebuild')} has to be either logical or numeric!")
  }
  if (!dir.exists(dbdir) & is.null(db_connection)) {
    usethis::ui_oops("Directory {usethis::ui_path(dbdir)} doesn't exist yet. Try creating...")
    dir.create(dbdir)
  }
  if (is.null(db_connection)) {
    connection <- con
  }
  else {
    connection <- db_connection
  }
  if (!DBI::dbExistsTable(connection, tblname)) {
    build_db(tblname, connection, rebuild = "NEW")
  }
  else if (DBI::dbExistsTable(connection, tblname) & all(force_rebuild != 
                                                         FALSE)) {
    build_db(tblname, connection, rebuild = force_rebuild)
  }
  usethis::ui_todo("Checking for missing completed games...")
  completed_games <- readRDS(url("https://github.com/leesharpe/nfldata/blob/master/data/games.rds?raw=true")) %>% 
    dplyr::filter(.data$season >= 1999, !is.na(.data$result), 
                  !.data$game_id %in% c("1999_01_BAL_STL", "2000_06_BUF_MIA", 
                                        "2000_03_SD_KC")) %>% dplyr::arrange(.data$gameday) %>% 
    dplyr::pull(.data$game_id)
  missing <- get_missing_games(completed_games, connection, 
                               tblname)
  if (length(missing) > 16) {
    build_db(tblname, connection, show_message = FALSE, rebuild = as.numeric(unique(stringr::str_sub(missing, 
                                                                                                     1, 4))))
    missing <- get_missing_games(completed_games, connection, 
                                 tblname)
  }
  if (length(missing) > 0) {
    if (!requireNamespace("furrr", quietly = TRUE)) {
      is_installed_furrr <- FALSE
      usethis::ui_info("Package {usethis::ui_value('furrr')} not installed. Can't use parallel processing. Please consider installing it.")
      usethis::ui_info("Will go on sequentially...")
    }
    else {
      is_installed_furrr <- TRUE
    }
    if (is_installed_furrr == TRUE & length(missing) < 5) {
      is_installed_furrr <- FALSE
    }
    new_pbp <- build_nflfastR_pbp(missing, pp = is_installed_furrr, 
                                  rules = FALSE)
    if (nrow(new_pbp) == 0) {
      usethis::ui_oops("Raw data of new games are not yet ready. Please try again in about 10 minutes.")
    }
    else {
      usethis::ui_todo("Appending new data to database...")
      DBI::dbWriteTable(connection, tblname, new_pbp, append = TRUE)
    }
  }
  message_completed("Database update completed", in_builder = TRUE)
  usethis::ui_info("Path to your db: {usethis::ui_path(DBI::dbGetInfo(connection)$dbname)}")
  DBI::dbDisconnect(connection)
  rule_footer("DONE")
}
