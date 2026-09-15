# Free agent scrape for all three fantasy leagues

# Beep Boop ---------------------------------------------------------------

# source('init.R')

leagueID <- fantasy_key$league_id[1]
fantasy_key$league_name[1]

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
espn.players <- data.frame(
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
url = paste0(initR::base, year, initR::mid, initR::leagueID, initR::player_tail)

# 1
Playersget = GET (
  url,
  config = httr::config(cookie = initR::cookie),
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
espn.players <- data.frame(
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

Sys.sleep(5)

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

espn.players <- rbind(espn.players, data.frame(
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

Sys.sleep(5)

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

espn.players <- rbind(espn.players, data.frame(
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

Sys.sleep(5)

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

espn.players <- rbind(espn.players, data.frame(
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

Sys.sleep(5)

# 5
Playersget = GET (
  url,
  config = httr::config(cookie = cookie),
  content_type_json(),
  user_agent(user_agent),
  httr::add_headers(
    "Referer" = glue("https://fantasy.espn.com/football/players/add?leagueId={leagueID}"),
    "X-Fantasy-Filter" = x_fantasy_filter[5],
    "X-Fantasy-Platform" = "kona-PROD-b3dba77950fd9b6a22ba3b8228a699d8bb974072",
    "X-Fantasy-Source" = "kona"
  )
)
Playersget$status_code
ESPNPlayersRaw <- rawToChar(Playersget$content)
ESPNPlayersFromJSON <- jsonlite::fromJSON(ESPNPlayersRaw)
ESPNPlayersFromJSON$players$player$fullName %>% head()

espn.players <- rbind(espn.players, data.frame(
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

Sys.sleep(5)

# 6
Playersget = GET (
  url,
  config = httr::config(cookie = cookie),
  content_type_json(),
  user_agent(user_agent),
  httr::add_headers(
    "Referer" = glue("https://fantasy.espn.com/football/players/add?leagueId={leagueID}"),
    "X-Fantasy-Filter" = x_fantasy_filter[6],
    "X-Fantasy-Platform" = "kona-PROD-b3dba77950fd9b6a22ba3b8228a699d8bb974072",
    "X-Fantasy-Source" = "kona"
  )
)
Playersget$status_code
ESPNPlayersRaw <- rawToChar(Playersget$content)
ESPNPlayersFromJSON <- jsonlite::fromJSON(ESPNPlayersRaw)
ESPNPlayersFromJSON$players$player$fullName %>% head()

espn.players <- rbind(espn.players, data.frame(
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

colnames(espn.players) <- players_columns

espn.players %>% 
  group_by(id) %>% 
  filter(n()>1)

espn.players %>% write_csv(paste0("fantasy_football/data/free_agents/", leagueID, "players_", Sys.Date(), ".csv"))
espn.players %>% saveRDS(paste0("fantasy_football/data/free_agents/", leagueID, "players_", Sys.Date(), ".rds"))


# Drinker's Slushy Beer ---------------------------------------------------

leagueID <- fantasy_key$league_id[2]
fantasy_key$league_name[2]

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
espn.players <- data.frame(
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
espn.players <- data.frame(
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

espn.players <- rbind(espn.players, data.frame(
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

espn.players <- rbind(espn.players, data.frame(
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

espn.players <- rbind(espn.players, data.frame(
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
colnames(espn.players) <- players_columns

espn.players %>% write_csv(paste0("fantasy_football/data/free_agents/", leagueID, "players_", Sys.Date(), ".csv"))
espn.players %>% saveRDS(paste0("fantasy_football/data/free_agents/", leagueID, "players_", Sys.Date(), ".rds"))


# Family League 3.0 -------------------------------------------------------

leagueID <- fantasy_key$league_id[3]
fantasy_key$league_name[3]

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
espn.players <- data.frame(
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
espn.players <- data.frame(
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

espn.players <- rbind(espn.players, data.frame(
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

espn.players <- rbind(espn.players, data.frame(
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

espn.players <- rbind(espn.players, data.frame(
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
colnames(espn.players) <- players_columns

espn.players %>% write_csv(paste0("fantasy_football/data/free_agents/", leagueID, "players_", Sys.Date(), ".csv"))
espn.players %>% saveRDS(paste0("fantasy_football/data/free_agents/", leagueID, "players_", Sys.Date(), ".rds"))

