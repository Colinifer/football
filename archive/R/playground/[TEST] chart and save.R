game_ids <- read.csv("data/games_data/reg_season/reg_games_2019.csv")

currentGameIDs <- game_ids$game_id
#pull games in 2019 season that match today's date
currentGames <- grep(date, currentGameIDs)
games_in_play <- currentGameIDs[currentGames]

x2 <- 2019102701
f2 <- paste("data/games_data/", userYear, "/", x2, ".csv", sep = "")
#read game csv
y2 <- read_csv(f2)
write.csv(y2, f2)
y2colnames <- colnames(y2)

y3 <- select(y2, home_team, away_team, desc, play_type, game_seconds_remaining, wp, wpa)
chart_data <- pbp_rp %>%
  group_by(posteam) %>%
  filter(down<=2) %>%
  summarise(
    n_dropbacks = sum(pass),
    n_rush = sum(rush),
    epa_per_db = sum(epa * pass) / n_dropbacks,
    epa_per_rush = sum(epa * rush) / n_rush,
    success_per_db = sum(success * pass) / n_dropbacks,
    success_per_rush = sum(success * rush) / n_rush
  )

chart <- chart_data %>% left_join(nfl_logos_df, by = c("posteam" = "team_code"))

chart %>%
  ggplot(aes(x = epa_per_rush, y = epa_per_db)) +
  geom_image(aes(image = url), size = 0.05) +
  labs(x = "Rush EPA/play",
       y = "Pass EPA/play",
       caption = "Data from nflscrapR",
       title = "Early-down rush and pass EPA/play",
       subtitle = "2018") +
  theme_bw() +
  geom_abline(slope=1, intercept=0, alpha=.2) +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 12))

ggsave('FILENAME.png', dpi=1000)