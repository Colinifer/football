for (x in game_ids$game_id) {
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









trgtsSeason <- playerSeason %>% filter(targets > 1)

for (z in unique(playerSeason$playerID) {
  
}

fplayersList <- list.files(paste("data/players/", userYear, "/", sep = ""),
                           pattern = "*.csv", full.names = TRUE)

# add rownames as a column in each data.frame and bind rows
bind_rows(df1 %>% add_rownames(), 
          df2 %>% add_rownames()) %>% 
  # evaluate following calls for each value in the rowname column
  group_by(rowname) %>% 
  # add all non-grouping variables
  summarise_all(sum)


playerSeason %>% group_by(playerID) %>% summarise_each(funs(sum))

tapply(x$Frequency, x$Category, FUN=sum)

playerSeason %>%
  group_by(playerID) %>% 
  summarise_all(funs(sum))