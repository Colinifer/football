addTargets <- function(x) {
  fplayers <- paste("data/players/", userYear, "/", x, "players.csv", sep = "")
  fpbp = paste("data/games/", userYear, "/", x, ".csv", sep = "")
  xpbp <- read.csv(fpbp)
  
  xplayers <- player_game(x)
  write.csv(xplayers, fplayers, row.names = FALSE)
  print(paste("X =", x))
  
  ## create targets dataframe
  targets <- filter(xpbp, play_type == "pass" & receiver_player_id != "NA")
  xreceivers <- unique(targets$receiver_player_id)
  
  ## add targets to stats
  for (z in xreceivers) {
    xplayers[xplayers$playerID == z, "targets"] <- sum(targets$receiver_player_id == z)
  }
  
  
  ## bring targets column next to the receptions
  xplayers <- xplayers[,c(1:20,ncol(xplayers),22:ncol(xplayers)-1)]
  xplayers$targets[is.na(xplayers$targets)] <- 0
  write.csv(xplayers, fplayers, row.names = FALSE)
}