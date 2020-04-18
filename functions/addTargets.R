addTargets <- function(x.id) {
  
  f.players <- paste("data/players/", u.year, "/", x.id, "players.csv", sep = "")
  ## create targets dataframe
  x.pbp <- read.csv(paste("data/games/", u.year, "/", x.id, ".csv", sep = ""))
  targets <- filter(x.pbp, play_type == "pass" & receiver_player_id != "NA")
  x.receivers <- unique(targets$receiver_player_id)
  
  ## add targets to stats
  ## for (z in x.receivers) {
    x.players[x.players$playerID == z, "targets"] <- sum(targets$receiver_player_id == z)
  ##}
  
  target_function <- function(z) {
    x.players[x.players$playerID == z, "targets"] <- sum(targets$receiver_player_id == z)
  }
  x.receivers %>% lapply(target_function)
  
  ## bring targets column next to the receptions
  x.players <- read.csv(paste("data/players/", u.year , "/", x.id , "players.csv", sep = ""))
  x.players <- x.players[,c(1:20,ncol(x.players),22:ncol(x.players)-1)]
  player_season$targets[is.na(player_season$targets)] <- 0
  write.csv(x.players, f.players, row.names = FALSE)
}