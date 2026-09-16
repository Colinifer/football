library(ggimage)

x = 1
u.year <- 2010:2019

pbp <- readRDS(url(paste("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_", u.year[x], ".rds", sep = ""))) %>%
  filter(game_type == 'REG') %>% clean_pbp() %>% filter(!is.na(posteam) & (rush == 1 | pass == 1))
qb_epa_play <- pbp %>% group_by(passer_player_name) %>% summarise(off_epa = mean(epa, na.rm = TRUE))
cpoe <- pbp %>% group_by(passer_player_name) %>% summarise(cpoe = mean(cpoe, na.rm = TRUE))
logos <- teams_colors_logos %>% select(team_abbr, team_logo_espn)

plot <- qb_epa_play %>%
  inner_join(cpoe, by = c("passer_player_name" = "passer_player_name")) %>%
  ## inner_join(logos, by = c("posteam" = "team_abbr")) %>%
  ggplot(aes(x = cpoe, y = qb_epa_play)) +
  geom_point() +
  geom_abline(slope = -1.5, intercept = c(.4, .3, .2, .1, 0, -.1, -.2, -.3), alpha = .2) +
  geom_hline(aes(yintercept = mean(off_epa)), color = "red", linetype = "dashed") +
  geom_vline(aes(xintercept = mean(cpoe)), color = "red", linetype = "dashed") +
  ## geom_image(aes(image = team_logo_espn), size = 0.05, asp = 16 / 9) +
  labs(
    x = "QB EPA/play",
    y = "QB CPOE",
    caption = "Data: @nflfastR | EPA model: @nflscrapR",
    title = paste(u.year[x], " NFL QB EPA and CPOE per Play", sep = "")
  ) +
  theme_bw() +
  theme(
    aspect.ratio = 9 / 16,
    plot.title = element_text(size = 12, hjust = 0.5, face = "bold")
  ) +
  scale_y_reverse()

paste("plots/desktop/team_epa_", u.year[x], "_1080desktop.png", sep = "") %>% ggsave(plot, device = "png", width = 12, height = 8, dpi = 100, limitsize = FALSE)

x = x + 1