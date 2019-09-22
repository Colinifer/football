userYear <- 2019
userWeek <- 3

g <- read.csv(paste("data/games_data/reg_season/reg_games_", userYear, ".csv", sep = ""))
g <- g %>% filter(state_of_game == "POST")
id <- g %>% pull(game_id)

new_plays <- NULL
for (x in id) {
  game_plays <- read.csv(paste("data/games_data/", userYear, "/", x, ".csv", sep = ""))
  new_plays <- bind_rows(new_plays,game_plays)
}

write.csv(new_plays, paste("data/games_data/", userYear, "/pbp", userYear, ".csv", sep = ""))

seasonpbp <- read.csv(paste("data/games_data/", userYear, "/pbp", userYear, ".csv", sep = ""))