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
ff_fantasy_key <- "fantasy_football/data/fantasy_key.csv"
swid  <-  "{2BA315B4-5941-4B1C-A315-B459416B1CC1}"
espn_s2 <- "AEBtGuDXUCKk6SpqlY71qdBDW%2BYc5KGa80m%2F0EVX9NCF%2FIFBM5b8ZMKgrMovpUeUqFTp4M%2BrPbM1I4rT1Ra2oXbM847nUp25DBY9Q%2FsAPChAykF5VNEZ05VjF6Vu3thAU0WkzQeBbjkdzNGqfbmPtMNzrBy8oV7fcAlwh4X89q4XlfPNED8ppKynNj5admyBk7WaqNzQtZJLlStpyOjz3F3d5BwUtQ8kh390OPB5HEEPfiH4%2FBftKqsLF%2BlyhTFaDiM%3D"
kona_v3_environment <- '{"leagueId":1034400,"seasonId":null}'
kona_v3_teamcontrol <- '{"leagueId":1034400,"seasonId":2020,"teamId":8}'
# league_id <- c("1034400", "62746259", "39973580")
# team_id <- c("8", "9", "10")
# league_name <- c("Colin's Minions", "Drinker's Slushy Beer", "Family League 3.0")
# team_name <- c("Rhule Tide", "Golden Rhule", "Matt Rhules")
# fantasy_key <- data.frame(league_id, league_name, team_id, team_name)
# fantasy_key %>% write_csv(ff_fantasy_key)
cookies = c(`SWID` = swid,
            `espn_s2` = espn_s2)
            # 'kona_v3_environment_season_ffl' = kona_v3_environment,
            # 'kona_v3_teamcontrol_ffl' = kona_v3_teamcontrol)
cookie <- paste(names(cookies), cookies, sep = "=", collapse = ";")
fantasy_key <- ff_fantasy_key %>% read_csv()
base = "https://fantasy.espn.com/apis/v3/games/ffl/seasons/"
year = Sys.Date() %>% format(format = "%Y")
mid = "/segments/0/leagues/"
leagueID <- fantasy_key$league_id[3]
tail = "?view=mDraftDetail&view=mLiveScoring&view=mMatchupScore&view=mPendingTransactions&view=mPositionalRatings&view=mSettings&view=mTeam&view=modular&view=mNav&view=mMatchupScore"
player_tail = "?view=kona_player_info"
wl_tail = "?view=proTeamSchedules_wl"
user_agent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.14; rv:79.0) Gecko/20100101 Firefox/79.0"

x_fantasy_filter <-
  c(
    '{"players":{"filterSlotIds":{"value":[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,23,24]},"filterRanksForScoringPeriodIds":{"value":[2]},"limit":100,"offset":0,"sortPercOwned":{"sortAsc":false,"sortPriority":1},"sortDraftRanks":{"sortPriority":100,"sortAsc":true,"value":"STANDARD"},"filterRanksForRankTypes":{"value":["PPR"]},"filterRanksForSlotIds":{"value":[0,2,4,6,17,16]},"filterStatsForTopScoringPeriodIds":{"value":2,"additionalValue":["002020","102020","002019","1120202","022020"]}}}',
    '{"players":{"filterSlotIds":{"value":[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,23,24]},"filterRanksForScoringPeriodIds":{"value":[2]},"limit":100,"offset":100,"sortAppliedStatTotal":{"sortAsc":false,"sortPriority":1,"value":"002020"},"sortDraftRanks":{"sortPriority":100,"sortAsc":true,"value":"STANDARD"},"filterRanksForRankTypes":{"value":["PPR"]},"filterRanksForSlotIds":{"value":[0,2,4,6,17,16]},"filterStatsForTopScoringPeriodIds":{"value":2,"additionalValue":["002020","102020","002019","1120202","022020"]}}}',
    '{"players":{"filterSlotIds":{"value":[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,23,24]},"filterRanksForScoringPeriodIds":{"value":[2]},"limit":100,"offset":200,"sortAppliedStatTotal":{"sortAsc":false,"sortPriority":1,"value":"002020"},"sortDraftRanks":{"sortPriority":100,"sortAsc":true,"value":"STANDARD"},"filterRanksForRankTypes":{"value":["PPR"]},"filterRanksForSlotIds":{"value":[0,2,4,6,17,16]},"filterStatsForTopScoringPeriodIds":{"value":2,"additionalValue":["002020","102020","002019","1120202","022020"]}}}',
    '{"players":{"filterSlotIds":{"value":[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,23,24]},"filterRanksForScoringPeriodIds":{"value":[2]},"limit":100,"offset":300,"sortAppliedStatTotal":{"sortAsc":false,"sortPriority":1,"value":"002020"},"sortDraftRanks":{"sortPriority":100,"sortAsc":true,"value":"STANDARD"},"filterRanksForRankTypes":{"value":["PPR"]},"filterRanksForSlotIds":{"value":[0,2,4,6,17,16]},"filterStatsForTopScoringPeriodIds":{"value":2,"additionalValue":["002020","102020","002019","1120202","022020"]}}}'
  )

# 2020 nflfastr roster data
# https://github.com/mrcaseb/nflfastR-roster/blob/master/data/nflfastR-roster.rds?raw=true

# Jeremy roster data
# https://gist.githubusercontent.com/jeremyabramson
# https://gist.githubusercontent.com/jeremyabramson/c93abcf602809c9a2d7d225c8dd012e1/raw/0b0cd7aae3725b18b857080aff36b172fe0cf35a/10160000-0581-684c-9751-8a12a25d9dc8_roster.json


# nflfastR roster data ----------------------------------------------------

rosters <- readRDS(url("https://github.com/mrcaseb/nflfastR-roster/blob/master/data/nflfastR-roster.rds?raw=true"))

rosters %>% colnames()

# Get free agent data -----------------------------------------------------

# Get league info
url = paste0(base,year,mid,leagueID,tail)
ESPNget = GET (
  url,
  config = httr::config(cookie = cookie),
  content_type_json(),
  user_agent(user_agent)
  )
ESPNget$status_code
ESPNRaw <- rawToChar(ESPNget$content)
ESPNFromJSON <- jsonlite::fromJSON(ESPNRaw)


# Get all players
players_columns <-  c(
  "id",
  "injured",
  "firstName",
  "lastName",
  "percentChange",
  "percentOwned",
  "percentStarted",
  "proTeamId",
  "positionalRanking",
  "totalRanking",
  "totalRating",
  "status",
  "onTeamId"
)
players <- data.frame(
  id = numeric(0),
  injured = numeric(0),
  firstName = numeric(0),
  lastName = numeric(0),
  percentChange = numeric(0),
  percentOwned = numeric(0),
  percentStarted = numeric(0),
  proTeamId = numeric(0),
  positionalRanking = numeric(0),
  totalRanking = numeric(0),
  totalRating = numeric(0),
  status = numeric(0),
  onTeamId = numeric(0)
)
# Get all players
url = paste0(base, year, mid, leagueID, player_tail)

# 1
Playersget = GET (
  url,
  config = httr::config(cookie = cookie),
  content_type_json(),
  user_agent(user_agent),
  httr::add_headers(
    "Referer" = glue("https://fantasy.espn.com/football/players/add?leagueId={leagueID}"),
    "X-Fantasy-Filter" = x_fantasy_filter[1],
    "X-Fantasy-Platform" = "kona-PROD-b3dba77950fd9b6a22ba3b8228a699d8bb974072",
    "X-Fantasy-Source" = "kona"
  )
)
Playersget$status_code
ESPNPlayersRaw <- rawToChar(Playersget$content)
ESPNPlayersFromJSON <- jsonlite::fromJSON(ESPNPlayersRaw)
ESPNPlayersFromJSON$players$player$fullName %>% head()
players <- data.frame(
    ESPNPlayersFromJSON$players$player$id,
    ESPNPlayersFromJSON$players$player$injured,
    ESPNPlayersFromJSON$players$player$firstName,
    ESPNPlayersFromJSON$players$player$lastName,
    ESPNPlayersFromJSON$players$player$ownership$percentChange,
    ESPNPlayersFromJSON$players$player$ownership$percentOwned,
    ESPNPlayersFromJSON$players$player$ownership$percentStarted,
    ESPNPlayersFromJSON$players$player$proTeamId,
    ESPNPlayersFromJSON$players$ratings$'0'$positionalRanking,
    ESPNPlayersFromJSON$players$ratings$'0'$totalRanking,
    ESPNPlayersFromJSON$players$ratings$'0'$totalRating,
    ESPNPlayersFromJSON$players$status,
    ESPNPlayersFromJSON$players$onTeamId
    )

# 2
Playersget = GET (
  url,
  config = httr::config(cookie = cookie),
  content_type_json(),
  user_agent(user_agent),
  httr::add_headers(
    "Referer" = glue("https://fantasy.espn.com/football/players/add?leagueId={leagueID}"),
    "X-Fantasy-Filter" = x_fantasy_filter[2],
    "X-Fantasy-Platform" = "kona-PROD-b3dba77950fd9b6a22ba3b8228a699d8bb974072",
    "X-Fantasy-Source" = "kona"
  )
)
Playersget$status_code
ESPNPlayersRaw <- rawToChar(Playersget$content)
ESPNPlayersFromJSON <- jsonlite::fromJSON(ESPNPlayersRaw)
ESPNPlayersFromJSON$players$player$fullName %>% head()

players <- rbind(players, data.frame(
  ESPNPlayersFromJSON$players$player$id,
  ESPNPlayersFromJSON$players$player$injured,
  ESPNPlayersFromJSON$players$player$firstName,
  ESPNPlayersFromJSON$players$player$lastName,
  ESPNPlayersFromJSON$players$player$ownership$percentChange,
  ESPNPlayersFromJSON$players$player$ownership$percentOwned,
  ESPNPlayersFromJSON$players$player$ownership$percentStarted,
  ESPNPlayersFromJSON$players$player$proTeamId,
  ESPNPlayersFromJSON$players$ratings$'0'$positionalRanking,
  ESPNPlayersFromJSON$players$ratings$'0'$totalRanking,
  ESPNPlayersFromJSON$players$ratings$'0'$totalRating,
  ESPNPlayersFromJSON$players$status,
  ESPNPlayersFromJSON$players$onTeamId
))

# 3
Playersget = GET (
  url,
  config = httr::config(cookie = cookie),
  content_type_json(),
  user_agent(user_agent),
  httr::add_headers(
    "Referer" = glue("https://fantasy.espn.com/football/players/add?leagueId={leagueID}"),
    "X-Fantasy-Filter" = x_fantasy_filter[3],
    "X-Fantasy-Platform" = "kona-PROD-b3dba77950fd9b6a22ba3b8228a699d8bb974072",
    "X-Fantasy-Source" = "kona"
  )
)
Playersget$status_code
ESPNPlayersRaw <- rawToChar(Playersget$content)
ESPNPlayersFromJSON <- jsonlite::fromJSON(ESPNPlayersRaw)
ESPNPlayersFromJSON$players$player$fullName %>% head()

players <- rbind(players, data.frame(
  ESPNPlayersFromJSON$players$player$id,
  ESPNPlayersFromJSON$players$player$injured,
  ESPNPlayersFromJSON$players$player$firstName,
  ESPNPlayersFromJSON$players$player$lastName,
  ESPNPlayersFromJSON$players$player$ownership$percentChange,
  ESPNPlayersFromJSON$players$player$ownership$percentOwned,
  ESPNPlayersFromJSON$players$player$ownership$percentStarted,
  ESPNPlayersFromJSON$players$player$proTeamId,
  ESPNPlayersFromJSON$players$ratings$'0'$positionalRanking,
  ESPNPlayersFromJSON$players$ratings$'0'$totalRanking,
  ESPNPlayersFromJSON$players$ratings$'0'$totalRating,
  ESPNPlayersFromJSON$players$status,
  ESPNPlayersFromJSON$players$onTeamId
))

# 4
Playersget = GET (
  url,
  config = httr::config(cookie = cookie),
  content_type_json(),
  user_agent(user_agent),
  httr::add_headers(
    "Referer" = glue("https://fantasy.espn.com/football/players/add?leagueId={leagueID}"),
    "X-Fantasy-Filter" = x_fantasy_filter[4],
    "X-Fantasy-Platform" = "kona-PROD-b3dba77950fd9b6a22ba3b8228a699d8bb974072",
    "X-Fantasy-Source" = "kona"
  )
)
Playersget$status_code
ESPNPlayersRaw <- rawToChar(Playersget$content)
ESPNPlayersFromJSON <- jsonlite::fromJSON(ESPNPlayersRaw)
ESPNPlayersFromJSON$players$player$fullName %>% head()

players <- rbind(players, data.frame(
  ESPNPlayersFromJSON$players$player$id,
  ESPNPlayersFromJSON$players$player$injured,
  ESPNPlayersFromJSON$players$player$firstName,
  ESPNPlayersFromJSON$players$player$lastName,
  ESPNPlayersFromJSON$players$player$ownership$percentChange,
  ESPNPlayersFromJSON$players$player$ownership$percentOwned,
  ESPNPlayersFromJSON$players$player$ownership$percentStarted,
  ESPNPlayersFromJSON$players$player$proTeamId,
  ESPNPlayersFromJSON$players$ratings$'0'$positionalRanking,
  ESPNPlayersFromJSON$players$ratings$'0'$totalRanking,
  ESPNPlayersFromJSON$players$ratings$'0'$totalRating,
  ESPNPlayersFromJSON$players$status,
  ESPNPlayersFromJSON$players$onTeamId
))
colnames(players) <- players_columns

players %>% write_csv(paste0("fantasy_football/data/", leagueID, "players.csv"))



# write_json(ESPNRaw, path = glue("espnRaw{year}.json"))