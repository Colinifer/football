date <- 201909

for (x in games_in_play) {
  f <- paste("data/games_data/", userYear, "/", x, ".csv", sep = "")
  
  if (file.exists(f)==TRUE) {
    y <- read.csv(f, check.names=FALSE)
    y[1] <- NULL
    write.csv(y, file = paste("data/games_data/", userYear,"/", x, ".csv", sep = ""), row.names=FALSE)
  }
}