#Ron Yurko code
pbp <- read_csv(url("https://github.com/ryurko/nflscrapR-data/raw/master/play_by_play_data/regular_season/reg_pbp_2018.csv"))

userWeek <- 2

pg <- game_ids$state_of_game == "POST"
completeGames <- game_ids[pg,]
cg <- completeGames$game_id
 <- length(cg)

gameID <- paste("data/games_data/", , ".csv", sep = "")
pbp19 <- read_csv(gameID)


write.csv(pbp, "data/play_by_play_data/regular_season/reg_pbp_2018.csv")