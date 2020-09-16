seasonPBP <- function(x) {
  pbp2019 <- list.files(paste("data/games/", userYear, "/", sep = ""),
                        pattern = "*.csv", full.names = TRUE) %>%
    lapply(read_csv) %>%
    bind_rows
  pbp2019
  write.csv(pbp2019, file = paste("data/season_total/pbp", userYear,".csv", sep = ""), row.names=FALSE)
}