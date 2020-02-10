for (x in game_ids$game_id) {
  fplayers <- paste("data/players/", userYear, "/", x, "players.csv", sep = "")
  xplayers <- player_game(x)
  write.csv(xplayers, fplayers, row.names = FALSE)
  print(paste("X =", x))
  ## create targets dataframe
  fpbp = paste("data/games/", userYear, "/", x, ".csv", sep = "")
  xpbp <- read.csv(fpbp)
  targets <- filter(xpbp, play_type == "pass" & receiver_player_id != "NA")
  xreceivers <- unique(targets$Receiver_ID)
  
  ## add targets to stats
  for (z in xreceivers) {
    xplayers[xplayers$playerID == z, "targets"] <- sum(targets$Receiver_ID == z)
  }
  
  ## bring targets column next to the receptions
  xplayers <- xplayers[,c(1:20,ncol(xplayers),22:ncol(xplayers)-1)]
  xplayers$targets[is.na(xplayers$targets)] <- 0
  write.csv(xplayers, fplayers, row.names = FALSE)
}