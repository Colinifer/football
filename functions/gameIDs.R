gameIDs <- function() {
  userYear <- 2009
  
  while (userYear <= 2020) {
    fgame_ids <- paste("data/games/", seasonState, "_season/", seasonState, "_games_", userYear, ".csv", sep ="")
    game_ids <- scrape_game_ids(userYear, type = seasonState)
    write.csv(game_ids, fgame_ids, row.names = FALSE)
    userYear <- userYear + 1
  }
}
