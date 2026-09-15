# note: Pull out the Home and Away colors:
# note: Make this dynamic across games and add to loop
nfl_teamcolors <- teamcolors %>% filter(league == "nfl")
homeTeam_color <- nfl_teamcolors %>%
  filter(name == homeTeam_fullname) %>%
  pull(primary)
awayTeam_color <- nfl_teamcolors %>%
  filter(name == awayTeam_fullname) %>%
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
  scale_color_manual(labels = c(homeTeam_abbr, awayTeam_abbr),
                     values = c(awayTeam_color, homeTeam_color),
                     guide = FALSE) +
  scale_x_reverse(breaks = seq(0, 3600, 300)) + 
  annotate("text", x = 3000, y = .75, label = homeTeam_abbr, color = homeTeam_color, size = 8) + 
  annotate("text", x = 3000, y = .25, label = awayTeam_abbr, color = awayTeam_color, size = 8) +
  geom_vline(xintercept = 900, linetype = "dashed", black) + 
  geom_vline(xintercept = 1800, linetype = "dashed", black) + 
  geom_vline(xintercept = 2700, linetype = "dashed", black) + 
  geom_vline(xintercept = 0, linetype = "dashed", black) + 
  labs(
    x = "Time Remaining (seconds)",
    y = "Win Probability",
    title = paste("Week", userWeek, "Win Probability Chart", sep = " "),
    subtitle = paste(homeTeam_fullname, "vs.", awayTeam_fullname, sep = " "),
    caption = "Data from nflscrapR"
  ) + theme_bw()