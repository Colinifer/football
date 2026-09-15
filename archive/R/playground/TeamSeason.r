TeamGames <- filter(game_ids, away_team == 'PHI')
TeamGames <- rbind(filter(game_ids, home_team == 'PHI'), filter(game_ids, away_team == 'PHI'))

TeamGameIDs <- TeamGames$game_id

for (x in TeamGameIDs) {
  fplayers <- paste("data/players/", TeamGames$season[1], "/", x, "players.csv", sep = )
  
  TeamPlayerSeason <- read.csv(fplayers)
  
  rbind(TeamPlayerSeason, read.csv(fplayers))
}