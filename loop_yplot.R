x2 <- 2019101400
y2 <- scrape_json_play_by_play(x2)
f2 <- paste("data/games_data/", userYear, "/", x2, ".csv", sep = "")
write.csv(y2, f2)
#read game csv
y2 <- read.csv(f2)
y2colnames <- colnames(y2)

y3 <- select(y2, home_team, away_team, desc, play_type, game_seconds_remaining, wp, wpa)

## graph new scrape
homeTeam_abbr <- game_ids[game_ids$game_id == x2, "home_team"]
awayTeam_abbr <- game_ids[game_ids$game_id == x2, "away_team"]
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
awayTeam_secondarycolor <- nfl_teamcolors %>%
  filter(name == awayTeam_fullname) %>%
  pull(primary)

homeTeam_color <- nfl_teamcolors %>%
  filter(name == homeTeam_fullname) %>%
  pull(primary)
homeTeam_secondarycolor <- nfl_teamcolors %>%
  filter(name == homeTeam_fullname) %>%
  pull(secondary)

    ##
    ##
  
y2 %>%
  filter(!is.na(away_wp),
         !is.na(home_wp)) %>%
  dplyr::select(game_seconds_remaining,
                away_wp,
                home_wp) %>%
  gather(team, wpa, -game_seconds_remaining) %>%
  ggplot(aes(x = game_seconds_remaining, y = wpa, color = team)) +
  geom_line(size = 3) +
  geom_hline(yintercept = 0.5, color = "gray", linetype = "dashed") +
  scale_color_manual(labels = c(awayTeam_abbr, homeTeam_abbr),
                     values = c(awayTeam_color, homeTeam_color),
                     guide = FALSE) +
  scale_x_reverse(breaks = seq(0, 3600, 300)) + 
  annotate("text", x = 3000, y = .75, label = awayTeam_abbr, color = awayTeam_secondarycolor, size = 16) + 
  annotate("text", x = 3000, y = .25, label = homeTeam_abbr, color = homeTeam_secondarycolor, size = 16) +
  geom_vline(xintercept = 900, linetype = "dashed", black) + 
  geom_vline(xintercept = 1800, linetype = "dashed", black) + 
  geom_vline(xintercept = 2700, linetype = "dashed", black) + 
  geom_vline(xintercept = 0, linetype = "dashed", black) + 
#  geom_vline(xintercept = , linetype = "solid", black)
  labs(
    x = "Time Remaining (seconds)",
    y = "Win Probability",
    title = paste("Week", userWeek, "Win Probability Chart", sep = " "),
    subtitle = paste(awayTeam_fullname, "vs.", homeTeam_fullname, sep = " "),
    caption = "Data from nflscrapR"
  ) + theme_bw()

print("Last play:")
print(paste("EPA Added:", y2$epa[nrow(y2)-2], ",", y2$desc[nrow(y2)-2], sep = " "))
print(paste("EPA Added:", y2$epa[nrow(y2)-1], ",", y2$desc[nrow(y2)-1], sep = " "))
print(paste("EPA Added:", y2$epa[nrow(y2)], ",", y2$desc[nrow(y2)], sep = " "))

print(paste(awayTeam_fullname, "Win Probability:", y2$away_wp[nrow(y2)], sep=" "))
print(paste(homeTeam_fullname, "Win Probability:", y2$home_wp[nrow(y2)], sep = " "))
