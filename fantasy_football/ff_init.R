library(tidyverse)
library(httr)
library(jsonlite)
# library(sqldf)
library(gt)
detach(package:purrr)
library(purrr)
library(glue)


# Create variables --------------------------------------------------------

# ESPN Fantasy Football
# swid  <-  "{2BA315B4-5941-4B1C-A315-B459416B1CC1}"
# espn_s2 <- "AEBtGuDXUCKk6SpqlY71qdBDW%2BYc5KGa80m%2F0EVX9NCF%2FIFBM5b8ZMKgrMovpUeUqFTp4M%2BrPbM1I4rT1Ra2oXbM847nUp25DBY9Q%2FsAPChAykF5VNEZ05VjF6Vu3thAU0WkzQeBbjkdzNGqfbmPtMNzrBy8oV7fcAlwh4X89q4XlfPNED8ppKynNj5admyBk7WaqNzQtZJLlStpyOjz3F3d5BwUtQ8kh390OPB5HEEPfiH4%2FBftKqsLF%2BlyhTFaDiM%3D"
# kona_v3_environment <- '{"leagueId":1034400,"seasonId":null}'
# kona_v3_teamcontrol <- '{"leagueId":1034400,"seasonId":2020,"teamId":8}'
# 
# ff_fantasy_key <- "fantasy_football/data/fantasy_key.rds"
# # league_id <- c("1034400", "62746259", "39973580")
# # team_id <- c("8", "9", "10")
# # league_name <- c("Colin's Minions", "Drinker's Slushy Beer", "Family League 3.0")
# # team_name <- c("Rhule Tide", "Golden Rhule", "Matt Rhules")
# # fantasy_key <- data.frame(league_id, league_name, team_id, team_name)
# # fantasy_key %>% write_rds(ff_fantasy_key)
# cookies = c(`SWID` = swid,
#             `espn_s2` = espn_s2)
#             # 'kona_v3_environment_season_ffl' = kona_v3_environment,
#             # 'kona_v3_teamcontrol_ffl' = kona_v3_teamcontrol)
# cookie <- paste(names(cookies), cookies, sep = "=", collapse = ";")
# # fantasy_key <- ff_fantasy_key %>% read_csv()
# fantasy_key <- ff_fantasy_key %>% readRDS()
# base = "https://fantasy.espn.com/apis/v3/games/ffl/seasons/"
# year = Sys.Date() %>% format(format = "%Y")
# mid = "/segments/0/leagues/"
# leagueID <- fantasy_key$league_id[3]
# tail = "?view=mDraftDetail&view=mLiveScoring&view=mMatchupScore&view=mPendingTransactions&view=mPositionalRatings&view=mSettings&view=mTeam&view=modular&view=mNav&view=mMatchupScore"
# player_tail = "?view=kona_player_info"
# wl_tail = "?view=proTeamSchedules_wl"
# user_agent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.14; rv:79.0) Gecko/20100101 Firefox/79.0"

x_fantasy_filter <-
  c(
    '{"players":{"filterSlotIds":{"value":[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,23,24]},"filterRanksForScoringPeriodIds":{"value":[2]},"limit":100,"offset":0,"sortPercOwned":{"sortAsc":false,"sortPriority":1},"sortDraftRanks":{"sortPriority":100,"sortAsc":true,"value":"STANDARD"},"filterRanksForRankTypes":{"value":["PPR"]},"filterRanksForSlotIds":{"value":[0,2,4,6,17,16]},"filterStatsForTopScoringPeriodIds":{"value":2,"additionalValue":["002020","102020","002019","1120202","022020"]}}}',
    '{"players":{"filterSlotIds":{"value":[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,23,24]},"filterRanksForScoringPeriodIds":{"value":[2]},"limit":100,"offset":100,"sortPercOwned":{"sortAsc":false,"sortPriority":1},"sortDraftRanks":{"sortPriority":100,"sortAsc":true,"value":"STANDARD"},"filterRanksForRankTypes":{"value":["PPR"]},"filterRanksForSlotIds":{"value":[0,2,4,6,17,16]},"filterStatsForTopScoringPeriodIds":{"value":2,"additionalValue":["002020","102020","002019","1120202","022020"]}}}',
    '{"players":{"filterSlotIds":{"value":[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,23,24]},"filterRanksForScoringPeriodIds":{"value":[2]},"limit":100,"offset":200,"sortPercOwned":{"sortAsc":false,"sortPriority":1},"sortDraftRanks":{"sortPriority":100,"sortAsc":true,"value":"STANDARD"},"filterRanksForRankTypes":{"value":["PPR"]},"filterRanksForSlotIds":{"value":[0,2,4,6,17,16]},"filterStatsForTopScoringPeriodIds":{"value":2,"additionalValue":["002020","102020","002019","1120202","022020"]}}}',
    '{"players":{"filterSlotIds":{"value":[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,23,24]},"filterRanksForScoringPeriodIds":{"value":[2]},"limit":100,"offset":300,"sortPercOwned":{"sortAsc":false,"sortPriority":1},"sortDraftRanks":{"sortPriority":100,"sortAsc":true,"value":"STANDARD"},"filterRanksForRankTypes":{"value":["PPR"]},"filterRanksForSlotIds":{"value":[0,2,4,6,17,16]},"filterStatsForTopScoringPeriodIds":{"value":2,"additionalValue":["002020","102020","002019","1120202","022020"]}}}',
    '{"players":{"filterSlotIds":{"value":[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,23,24]},"filterRanksForScoringPeriodIds":{"value":[2]},"limit":100,"offset":400,"sortPercOwned":{"sortAsc":false,"sortPriority":1},"sortDraftRanks":{"sortPriority":100,"sortAsc":true,"value":"STANDARD"},"filterRanksForRankTypes":{"value":["PPR"]},"filterRanksForSlotIds":{"value":[0,2,4,6,17,16]},"filterStatsForTopScoringPeriodIds":{"value":2,"additionalValue":["002020","102020","002019","1120202","022020"]}}}',
    '{"players":{"filterSlotIds":{"value":[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,23,24]},"filterRanksForScoringPeriodIds":{"value":[2]},"limit":100,"offset":500,"sortPercOwned":{"sortAsc":false,"sortPriority":1},"sortDraftRanks":{"sortPriority":100,"sortAsc":true,"value":"STANDARD"},"filterRanksForRankTypes":{"value":["PPR"]},"filterRanksForSlotIds":{"value":[0,2,4,6,17,16]},"filterStatsForTopScoringPeriodIds":{"value":2,"additionalValue":["002020","102020","002019","1120202","022020"]}}}'
  )

# Scrape free agents from all leagues
source('fantasy_football/fa_scrape.R')



# Roster data ----------------------------------------------------

# nflfastR roster
nflfastr.rosters <-
  readRDS(
    url(
      'https://github.com/mrcaseb/nflfastR-roster/blob/master/data/nflfastR-roster.rds?raw=true'
    )
  )
nflfastr.rosters %>%
  filter(pbp_id > 0)

# Sleeper API
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
