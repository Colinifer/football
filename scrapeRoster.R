for (x in teams$nflscrapr_abbrev) {
  roster <- get_season_rosters(userYear, teams = x, positions = c(
    "QUARTERBACK",
    "RUNNING_BACK",
    "WIDE_RECEIVER",
    "TIGHT_END",
    "DEFENSIVE_LINEMAN",
    "LINEBACKER",
    "DEFENSIVE_BACK",
    "KICKOFF_KICKER",
    "KICK_RETURNER",
    "PUNTER",
    "PUNT_RETURNER",
    "FIELD_GOAL_KICKER"
  ))
  froster <- paste("data/teams/", userYear, "/", x, userYear, "roster.csv", sep = "") #ARI2019roster.csv
  write.csv(roster, froster, row.names = FALSE)
}