# set custom variables
  userYear <- 2019 ##necessary for saved 
  userWeek <- 16 ##not necessary at the moment
  today <- Sys.Date()
  
    # test date
  date <- 201912
##  date <- format(today, format="%Y%m%d")
  
  fgame_ids <- paste("data/games/reg_season/reg_games_", userYear, ".csv", sep ="")
  

  ## read Game IDs
game_ids <- read.csv(fgame_ids, check.names = FALSE)
## save Game IDs 
# write.csv(game_ids, file = fgame_ids, row.names = FALSE)

y <- data.frame()


currentGameIDs <- game_ids$game_id
#pull games in 2019 season that match today's date
currentGames <- grep(date, currentGameIDs)
games_in_play <- currentGameIDs[currentGames]

##can't figure this out yet
#  
#  games_in_play <- game_ids$state_of_game[currentGames] != "POST"
#
#  nplay <- length(games_in_play)
#  nplayLoop <- 1
#

# scrape pbp of active games

# if 0 games, scrape scores
for (x in games_in_play)
  {
  f <- paste("data/games/", userYear, "/", x, ".csv", sep = "")
  fplayers <-  paste("data/players/", userYear, "/", x, "players", ".csv", sep = "")
  
  if (file.exists(f)==TRUE)
    {
    
    #read game csv
    y <- read.csv(f, check.names=FALSE)
    tail(y)
    
    #check if y$desc contains "END GAME"
    #if x has END GAME change state_of_game to POST
    if (grepl("END GAME", y$desc[nrow(y)]) == TRUE)
      {
      print(paste("Game", x, "is over.", sep = " "))
      game_ids[game_ids$game_id == x, "state_of_game"] <- "POST"
      game_ids[game_ids$game_id == x, "away_score"] <- y$total_away_score[nrow(y)]
      game_ids[game_ids$game_id == x, "home_score"] <- y$total_home_score[nrow(y)]
      print(paste("Changing the state of game for", x, "to POST", sep = " "))
      write.csv(game_ids, fgame_ids, row.names = FALSE)
    }
    else
      {
      #scrape
      print(paste("Scraping game ", x, sep = ""))
      print(paste(game_ids[game_ids$game_id == x, "away_team"], "vs", game_ids[game_ids$game_id == x, "home_team"], sep = " "))
      y <- scrape_json_play_by_play(x)
      tail(y)
      game_ids$X <- NULL ## annoying glitch
      if (grepl("END GAME", y$desc[nrow(y)]) == TRUE) {
        print(paste("Game", x, "is over.", sep = " "))
          print(paste(game_ids[game_ids$game_id == x, "away_team"], "vs", game_ids[game_ids$game_id == x, "home_team"], sep = " "))
        game_ids[game_ids$game_id == x, "state_of_game"] <- "POST"
        game_ids[game_ids$game_id == x, "away_score"] <- y$total_away_score[nrow(y)]
        game_ids[game_ids$game_id == x, "home_score"] <- y$total_home_score[nrow(y)]
        print(paste("Changing the state of game for ", x, " to POST", sep = ""))
        write.csv(game_ids, fgame_ids, row.names=FALSE)
        }
      write.csv(y, file = paste("data/games/", userYear,"/", x, ".csv", sep = ""), row.names=FALSE)
      }
  }
  else
    {
    print(paste("Scraping game", x, sep = " "))
    print(
      paste(
        game_ids[
        game_ids$game_id == x, "away_team"], 
        "vs", 
        game_ids[game_ids$game_id == x, "home_team"], 
        sep = " "
        )
      )
    y <- scrape_json_play_by_play(x)
    tail(y)
    write.csv(y, file = paste("data/games/", userYear,"/", x, ".csv", sep = ""), row.names=FALSE)
    }
  xawayscore <- y$total_away_score[nrow(y)]
  xawayteam <- y$away_team[1]
  xhomescore <- y$total_home_score[nrow(y)]
  xhometeam <- y$home_team[1]
  print(paste(xawayteam, ": ", xawayscore, " @ ", xhometeam, ": ", xhomescore, sep = ""))
  
  print(paste("Last play:", y$desc[nrow(y)], sep=""))
  
  ## add to the normal scrape functions
  if (file.exists(fplayers)==TRUE & grepl("END GAME", y$desc[nrow(y)]) == TRUE)
  {
    xplayers <- player_game(x)
    write.csv(xplayers, fplayers, row.names = FALSE)
    print(paste("X =", x))
    xpbp <- game_play_by_play(x)
    addTargets(x)
  } else {
    ## scrape player stats
    xplayers <- player_game(x)
    print(paste("X =", x))
    xpbp <- game_play_by_play(x)
    addTargets(x)
  }
  
  addTargets(x)
}


## xpbp <- game_play_by_play(2019122300)
xreceivers <- unique(targets$Receiver_ID)
for (x in xreceivers) {
  xplayers[xplayers$playerID == x, "targets"] <- sum(x)
}

for (x in xreceivers) {
  xplayers[xplayers$playerID == x, "targets"] <- sum(targets$Receiver_ID == x)
}
## bring targets column next to the receptions
xplayers <- xplayers[,c(1:20,ncol(xplayers),22:ncol(xplayers)-1)]



## start season merge

## PBP Merge

pbp2019 <- list.files(paste("data/games/", userYear, "/", sep = ""),
                        pattern = "*.csv", full.names = TRUE) %>%
  lapply(read_csv) %>%
  bind_rows
pbp2019
write.csv(pbp2019, file = paste("data/season_total/pbp", userYear,".csv", sep = ""), row.names=FALSE)

## Players Merge

players2019 <- list.files(paste("data/players/", userYear, "/", sep = ""),
                      pattern = "*.csv", full.names = TRUE) %>%
  lapply(read_csv) %>%
  bind_rows
players2019
write.csv(players2019, file = paste("data/season_total/players", userYear,".csv", sep = ""), row.names=FALSE)

for (x in teamAbbr$nflscrapr_abbrev) {
  ## doesn't work yet
  ##  print(paste("Getting roster for the", userYear, teamAbbr$full_name == x))
  xroster <- season_rosters(userYear, teams = x)
  froster <- paste("data/teams/", userYear, "/", x, userYear, "roster", ".csv", sep = "")
  write.csv(xroster, file = froster, row.names = FALSE)
}

teams <- list.files(paste("data/players/", userYear, "/", sep = ""),
                          pattern = "*.csv", full.names = TRUE) %>%
  lapply(read_csv) %>%
  bind_rows
players2019
write.csv(teams, file = paste("data/season_total/players", userYear,".csv", sep = ""), row.names=FALSE)


##end season merge

week_game_ids <- fiter(game_ids, week == 8)
games_in_play <- week_game_ids$game_id
## currentGames <- 

## graph new scrape
## x <- 2019102100
y <- scrape_json_play_by_play(x)
write.csv(y, f, row.names = FALSE)

homeTeam_abbr <- game_ids[game_ids$game_id == x, "home_team"]
awayTeam_abbr <- game_ids[game_ids$game_id == x, "away_team"]

homeTeam_abbr <- y$home_team[1]
awayTeam_abbr <- y$away_team[1]

teamAbbr <- read.csv(paste("data/season_total/team_abbr.csv", sep = ""))
homeTeamInt <- grep(homeTeam_abbr, teamAbbr$nflscrapr_abbrev)
awayTeamInt <- grep(awayTeam_abbr, teamAbbr$nflscrapr_abbrev)
# awayTeamInt <- 24 # if Rams
homeTeam_fullname <- teamAbbr$full_name[homeTeamInt]
awayTeam_fullname <- teamAbbr$full_name[awayTeamInt]
homeTeam_logo <- teamAbbr[teamAbbr$nflscrapr_abbrev == homeTeam_abbr, "logo"]
awayTeam_logo <- teamAbbr[teamAbbr$nflscrapr_abbrev == awayTeam_abbr, "logo"]

# awayImage <- download.file(paste("", awayTeam_logo, "", sep = ""), destfile = "tmp.png")
# homeImage <- download.file(paste("", homeTeam_logo, "", sep = ""), destfile = "tmp1.png")

game_ids[game_ids$game_id == x, "state_of_game"]

# note: home/awayTeam currently grabs abbrev name, need to get full name.

# note: Pull out the Home and Away colors:
# note: Make this dynamic across games and add to loop
nfl_teamcolors <- teamcolors %>% filter(league == "nfl")
awayTeam_color <- nfl_teamcolors %>%
  filter(name == awayTeam_fullname) %>%
  pull(primary)
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
#  annotation_custom(awayImage ) + 
  annotate("text", x = 3000, y = .25, label = homeTeam_abbr, color = homeTeam_color, size = 8) +
#  annotation_custom(homeImage ) + 
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

xawayscore <- y$total_away_score[nrow(y)]
xawayteam <- y$away_team[1]
xhomescore <- y$total_home_score[nrow(y)]
xhometeam <- y$home_team[1]

print("Last play:")
print(paste("EPA Added:", y$epa[nrow(y)-2], ",", y$desc[nrow(y)-2], sep = " "))
print(paste("EPA Added:", y$epa[nrow(y)-1], ",", y$desc[nrow(y)-1], sep = " "))
print(paste("EPA Added:", y$epa[nrow(y)], ",", y$desc[nrow(y)], sep = " "))
tail(y$desc, 3)
print("Score:")
print(paste(awayTeam_fullname, ": ", xawayscore, sep = ""))
print(paste(homeTeam_fullname, ": ", xhomescore, sep = ""))

print(paste(awayTeam_fullname, "Win Probability:", y$away_wp[nrow(y)], sep=" "))
print(paste(homeTeam_fullname, "Win Probability:", y$home_wp[nrow(y)], sep = " "))

 
## note: print winner and score
##  endGame == TRUE

yawaypen <- filter(y, penalty_team == xawayteam)
sum(yawaypen$penalty_yards)
yhomepen <- filter(y, penalty_team == xhometeam)
sum(yhomepen$penalty_yards)
sum(y$penalty_yards)
sum(y$penalty_yards[!is.na(y$penalty_yards)])