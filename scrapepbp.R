##set custom variables
  userYear <- 2019 ##necessary for saved 
  userWeek <- 3 ##not necessary at the moment
  today <- Sys.Date()
  
  #test date
  ##date <- 20190922
  date <- format(today, format="%Y%m%d")
  
game_ids <- read.csv("data/games_data/reg_season/reg_games_2019.csv")
game_ids <- read.csv("data/games_data/reg_season/reg_games_2019.csv")
  
  currentGameIDs <- game_ids$game_id
  #pull games in 2019 season that match today's date
  currentGames <- grep(date, currentGameIDs)
  games_in_play <- currentGameIDs[currentGames]

##can't figure this out yet
##  
##  games_in_play <- game_ids$state_of_game[currentGames] != "POST"
##
##  nplay <- length(games_in_play)
##  nplayLoop <- 1
##

#scrape pbp of active games
for (x in games_in_play) {
  #read game csv
  if (file.exists(f)) {
    y <- read.csv(f)
    #check if y$desc contains "END GAME"
    endGame <- grepl("END GAME", y$desc)
    y <- game_ids
    #check if y$desc contains "END GAME"
    endGame <- grepl("END GAME", y$desc)
  } else {
    y <- game_ids
    #check if y$desc contains "END GAME"
    endGame <- 2 > 3
  }
  #if x has END GAME change state_of_game to POST
  if(any(endGame == TRUE)) {
    f <- paste("data/games_data/", userYear, "/", x, ".csv", sep = "")
    game_ids[game_ids$game_id == x, "state_of_game"] <- "POST"
    print(paste("Changing the state of game for ", x, " to POST", sep = ""))
    ##save changes to season game_ids
    write.csv(game_ids, "data/games_data/reg_season/reg_games_2019.csv")
  } else {
    #scrape
    print(paste("Scraping game ", x, sep = ""))
    y <- scrape_json_play_by_play(x)
    write.csv(y, file = paste("data/games_data/", userYear,"/", x, ".csv", sep = ""))
    print("Last play:")
    print(y$desc[nrow(y)])
    endGame <- grepl("END GAME", y$desc)
    #check for end game and add post status if scrape includes
    if(any(endGame == TRUE)) {
      game_ids[game_ids$game_id == x, "state_of_game"] <- "POST"
      print(paste("Changing the state of game for ", x, " to POST", sep = ""))
    }
  }
}
  
  
##  game_ids[game_ids$game_id == 2019092300, "state_of_game"] <- "PRE"
##  write.csv(game_ids, "data/games_data/reg_season/reg_games_2019.csv")
  
  
  # note: Pull out the Bears and Redskins colors:
  # note: Make this dynamic across games and add to loop
  nfl_teamcolors <- teamcolors %>% filter(league == "nfl")
  chi_color <- nfl_teamcolors %>%
    filter(name == "Chicago Bears") %>%
    pull(primary)
  was_color <- nfl_teamcolors %>%
    filter(name == "Washington Redskins") %>%
    pull(primary)
  
  # Now generate the win probability chart:
  y %>%
    filter(!is.na(home_wp),
           !is.na(away_wp)) %>%
    dplyr::select(game_seconds_remaining,
                  home_wp,
                  away_wp) %>%
    gather(team, wpa, -game_seconds_remaining) %>%
    ggplot(aes(x = game_seconds_remaining, y = wpa, color = team)) +
    geom_line(size = 2) +
    geom_hline(yintercept = 0.5, color = "gray", linetype = "dashed") +
    scale_color_manual(labels = c("CHI", "WAS"),
                       values = c(chi_color, was_color),
                       guide = FALSE) +
    scale_x_reverse(breaks = seq(0, 3600, 300)) + 
    annotate("text", x = 3000, y = .75, label = "CHI", color = chi_color, size = 8) + 
    annotate("text", x = 3000, y = .25, label = "WAS", color = was_color, size = 8) +
    geom_vline(xintercept = 900, linetype = "dashed", black) + 
    geom_vline(xintercept = 1800, linetype = "dashed", black) + 
    geom_vline(xintercept = 2700, linetype = "dashed", black) + 
    geom_vline(xintercept = 0, linetype = "dashed", black) + 
    labs(
      x = "Time Remaining (seconds)",
      y = "Win Probability",
      title = paste("Week", userWeek, "Win Probability Chart", sep = " "),
      subtitle = "Chicago Bears vs. Washington Redskins",
      caption = "Data from nflscrapR"
    ) + theme_bw()
  
  
  

##print the last 3 plays
  
  ## note: class/function this somehow??
  
  print("Last play:")
  print(paste("EPA Added:", y$epa[nrow(y)-2], ",", y$desc[nrow(y)-2], sep = " "))
  print(paste("EPA Added:", y$epa[nrow(y)-1], ",", y$desc[nrow(y)-1], sep = " "))
  print(paste("EPA Added:", y$epa[nrow(y)], ",", y$desc[nrow(y)], sep = " "))
  
## note: print winner and score
  endGame == TRUE