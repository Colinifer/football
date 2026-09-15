
# Create dataframe matching ESPN team IDs
team_abbr <- rosters$team.abbr %>% unique() %>% sort()
team_id <-
  c(22, 1, 33, 2, 29, 3, 4, 5, 6, 7, 8, 9, 34, 11, 30, 12, 14, 24, 13, 15, 16, 17, 18, 19, 20, 21, 23, 26, 25, 27, 10, 28)
espn_team_ids <- data.frame(team_abbr, team_id)
# confirm team IDs are correct, automate this

join_names <- avg_exp_fp_df %>%
  mutate(player_roster_join = paste0(receiver, "_", posteam)) %>%
  select(player_roster_join)
fantasy_roster <- rosters %>%
  filter(pbp_name != "NA",
         team.season == 2020) %>%
  mutate(player_roster_join = paste0(pbp_name, "_", team.abbr)) %>%
  filter(player_roster_join %in% join_names$player_roster_join)

pro_team_id_to_name <- function(x) {
  y <- espn_team_ids %>% 
    filter(team_id == x) %>% 
    select(team_abbr)
  return(y[1])
}

players %>%
  mutate(
    player_roster_join = paste0(subset(ESPNPlayersFromJSON.players.player.firstName, 1, 1), ESPNPlayersFromJSON.players.player.lastName, "_", posteam),
    proTeamName = # Need to add function here
  )

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




# write_json(ESPNRaw, path = glue("espnRaw{year}.json"))