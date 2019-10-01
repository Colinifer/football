##set custom variables
  userYear <- 2019 ##necessary for saved 
  userWeek <- 4 ##not necessary at the moment
  today <- Sys.Date()
  
  #test date
  ##date <- 20190929
  date <- format(today, format="%Y%m%d")
  
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

#if 0 games, scrape scores
for (x in games_in_play) {
  f <- paste("data/games_data/", userYear, "/", x, ".csv", sep = "")
  
  if (file.exists(f)==TRUE) {
    
    #read game csv
    y <- read.csv(f)
    
    #check if y$desc contains "END GAME"
    #if x has END GAME change state_of_game to POST
    if(grepl("END GAME", y$desc[nrow(y)]) == TRUE) {
      print(paste("Game", x, "is over.", sep = " "))
    } else {
      #scrape
      print(paste("Scraping game ", x, sep = ""))
      y <- scrape_json_play_by_play(x)
      game_ids$X <- NULL ## annoying glitch
      if(grepl("END GAME", y$desc[nrow(y)]) == TRUE) {
        print(paste("Game", x, "is over.", sep = " "))
        game_ids[game_ids$game_id == x, "state_of_game"] <- "POST"
        print(paste("Changing the state of game for ", x, " to POST", sep = ""))
        ##save changes to season game_ids
        game_ids$X <- NULL ## annoying glitch
        write.csv(game_ids, "data/games_data/reg_season/reg_games_2019.csv")
        }
      write.csv(y, file = paste("data/games_data/", userYear,"/", x, ".csv", sep = ""))
      print(paste("Last play:", y$desc[nrow(y)], sep=""))
    }
  }
  else {
    print(paste("Scraping game", x, sep = " "))
    y <- scrape_json_play_by_play(x)
    write.csv(y, file = paste("data/games_data/", userYear,"/", x, ".csv", sep = ""))
  }
}


## graph new scrape
homeTeam_abbr <- game_ids[game_ids$game_id == x, "home_team"]
awayTeam_abbr <- game_ids[game_ids$game_id == x, "away_team"]
teamAbbr <- read.csv(paste("data/games_data/", userYear, "/team_abbr.csv", sep = ""))
homeTeamInt <- grep(homeTeam_abbr, teamAbbr$nflscrapr_abbrev)
awayTeamInt <- grep(awayTeam_abbr, teamAbbr$nflscrapr_abbrev)
homeTeam_fullname <- teamAbbr$full_name[homeTeamInt]
awayTeam_fullname <- teamAbbr$full_name[awayTeamInt]
homeTeam_logo <- nfl_teamcolors$logo[homeTeamInt]
awayTeam_logo <- nfl_teamcolors$logo[awayTeamInt]

# note: home/awayTeam currently grabs abbrev name, need to get full name.

# note: Pull out the Home and Away colors:
# note: Make this dynamic across games and add to loop
nfl_teamcolors <- teamcolors %>% filter(league == "nfl")
awayTeam_color <- nfl_teamcolors %>%
  filter(name == awayTeam_fullname) %>%
  pull(secondary)
homeTeam_color <- nfl_teamcolors %>%
  filter(name == homeTeam_fullname) %>%
  pull(primary)

# Now generate the win probability chart:
y %>%
  filter(!is.na(away_wp),
         !is.na(home_wp)) %>%
  dplyr::select(game_seconds_remaining,
                away_wp,
                home_wp) %>%
  gather(team, wpa, -game_seconds_remaining) %>%
  ggplot(aes(x = game_seconds_remaining, y = wpa, color = team)) +
  geom_line(size = 2) +
  geom_hline(yintercept = 0.5, color = "gray", linetype = "dashed") +
  scale_color_manual(labels = c(awayTeam_abbr, homeTeam_abbr),
                     values = c(awayTeam_color, homeTeam_color),
                     guide = FALSE) +
  scale_x_reverse(breaks = seq(0, 3600, 300)) + 
  annotate("text", x = 3000, y = .75, label = awayTeam_abbr, color = awayTeam_color, size = 8) + 
  annotate("text", x = 3000, y = .25, label = homeTeam_abbr, color = homeTeam_color, size = 8) +
  geom_vline(xintercept = 900, linetype = "dashed", black) + 
  geom_vline(xintercept = 1800, linetype = "dashed", black) + 
  geom_vline(xintercept = 2700, linetype = "dashed", black) + 
  geom_vline(xintercept = 0, linetype = "dashed", black) + 
  labs(
    x = "Time Remaining (seconds)",
    y = "Win Probability",
    title = paste("Week", userWeek, "Win Probability Chart", sep = " "),
    subtitle = paste(awayTeam_fullname, "vs.", homeTeam_fullname, sep = " "),
    caption = "Data from nflscrapR"
  ) + theme_bw()


##  game_ids[game_ids$game_id == 2019093000, "state_of_game"] <- "PRE"
##  write.csv(game_ids, "data/games_data/reg_season/reg_games_2019.csv")

##print the last 3 plays
  
  ## note: class/function this somehow??
print("Last play:")
print(paste("EPA Added:", y$epa[nrow(y)-2], ",", y$desc[nrow(y)-2], sep = " "))
print(paste("EPA Added:", y$epa[nrow(y)-1], ",", y$desc[nrow(y)-1], sep = " "))
print(paste("EPA Added:", y$epa[nrow(y)], ",", y$desc[nrow(y)], sep = " "))

print(paste(awayTeam_fullname, "Win Probability:", y$away_wp[nrow(y)], sep=" "))
print(paste(homeTeam_fullname, "Win Probability:", y$home_wp[nrow(y)], sep = " "))

 
## note: print winner and score
##  endGame == TRUE