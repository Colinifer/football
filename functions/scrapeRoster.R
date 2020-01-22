positions <- c(
  "QUARTERBACK", "RUNNING_BACK", "WIDE_RECEIVER",
  "TIGHT_END", "DEFENSIVE_LINEMAN", "LINEBACKER",
  "DEFENSIVE_BACK", "KICKOFF_KICKER", "KICK_RETURNER",
  "PUNTER", "PUNT_RETURNER", "FIELD_GOAL_KICKER"
)

for (x in teams$nflscrapr_abbrev[19:nrow(teams)]) {
  print(paste("Scraping ", x, " ", userYear, " roster. ", "Row #", which(teams$nflscrapr_abbrev == x), sep = ""))
  for (p in positions) {
    roster <- get_season_rosters(userYear, teams = x, positions = p)
    froster <- paste("data/teams/", userYear, "/", x, userYear, "roster.csv", sep = "") #ARI2019roster.csv
    if (file.exists(froster)==TRUE) {
      roster <- bind_rows(roster, read.csv(froster))
      roster <- roster[!duplicated(roster),]
    } else  {
      
    }
    write.csv(roster, froster, row.names = FALSE)
    }
}

for (x in teams$nflscrapr_abbrev[6:14]) {
  froster <- paste("data/teams/", userYear, "/", x, userYear, "roster.csv", sep = "") #ARI2019roster.csv
  roster <- read.csv(froster)
  roster <- roster[!duplicated(roster),]
  write.csv(roster, file = froster)
}