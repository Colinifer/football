library(ggimage)

x = 1
u.year <- 2010:2019

pbp <- readRDS(url(paste("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_", u.year[x], ".rds", sep = ""))) %>%
  filter(game_type == 'REG') %>% clean_pbp() %>% filter(!is.na(posteam) & (rush == 1 | pass == 1))
offense <- pbp %>% group_by(posteam) %>% summarise(off_epa = mean(epa, na.rm = TRUE))
defense <- pbp %>% group_by(defteam) %>% summarise(def_epa = mean(epa, na.rm = TRUE))
logos <- teams_colors_logos %>% select(team_abbr, team_logo_espn)

plot <- offense %>%
  inner_join(defense, by = c("posteam" = "defteam")) %>%
  inner_join(logos, by = c("posteam" = "team_abbr")) %>%
  ggplot(aes(x = off_epa, y = def_epa)) +
  geom_abline(slope = -1.5, intercept = c(.4, .3, .2, .1, 0, -.1, -.2, -.3), alpha = .2) +
  geom_hline(aes(yintercept = mean(off_epa)), color = "red", linetype = "dashed") +
  geom_vline(aes(xintercept = mean(def_epa)), color = "red", linetype = "dashed") +
  geom_image(aes(image = team_logo_espn), size = 0.05, asp = 16 / 9) +
  labs(
    x = "Offense EPA/play",
    y = "Defense EPA/play",
    caption = "Data: @nflfastR | EPA model: @nflscrapR",
    title = paste(u.year[x], " NFL Offensive and Defensive EPA per Play", sep = "")
  ) +
  theme_bw() +
  theme(
    aspect.ratio = 9 / 16,
    plot.title = element_text(size = 12, hjust = 0.5, face = "bold")
  ) +
  scale_y_reverse()

paste("plots/desktop/team_epa_", u.year[x], "_1080desktop.png", sep = "") %>% ggsave(plot, device = "png", width = 12, height = 8, dpi = 100, limitsize = FALSE)

x = x + 1