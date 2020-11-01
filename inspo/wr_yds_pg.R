library(tidyverse)
library(ggtext)
library(ggimage)
res = 800

games <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2020.rds'))
roster <- nflfastR::fast_scraper_roster(2020)

logos <- nflfastR::teams_colors_logos

games <- games %>%
  nflfastR::decode_player_ids()

joined <- games %>% 
  filter(!is.na(receiver_id), complete_pass == 1) %>%
  select(posteam, defteam, week, desc, receiver, receiver_id, epa, yards_gained) %>%
  left_join(roster, by = c('receiver_id' = 'gsis_id'))

chart <- joined %>%
  filter(position == "WR") %>%
  group_by(defteam) %>%
  summarize(
    yards = sum(yards_gained),
    games = length(unique(week)),
    yards_per_game = yards / games
  ) %>%
  ungroup() %>%
  arrange(yards_per_game) %>%
  left_join(logos, by = c("defteam" = "team_abbr")) %>%
  mutate(index_rank = 1 : n())

chart

my_title <- glue::glue("Yards allowed to <span style='color:red'>**wide receivers**</span> per game")
chart %>% 
  ggplot(aes(x = reorder(defteam, yards_per_game), y = yards_per_game)) +
  geom_col(aes(fill = ifelse(defteam=="SEA",team_color2, team_color)),
           width = 0.45, alpha = .8
  ) +
  geom_image(aes(image = team_logo_espn), size = 0.04, asp = 16/9, nudge_y=.02) +
  scale_fill_identity(aesthetics = c("fill", "colour")) +
  theme_bw() +
  scale_y_continuous(breaks = scales::breaks_pretty(n = 10)) +
  labs(
    x = "",
    y = "Yards per game",
    title = my_title,
    caption = paste("Figure: @benbbaldwin | Data: @nflfastR")
  ) +
  theme(
    panel.grid.major.x = element_blank(),
    plot.title = element_markdown(size = 24, hjust = 0.5),
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank()
  )   +
  geom_text(data = chart, aes(x = index_rank, y = -3, size=.04, label = index_rank), show.legend = FALSE)

ggsave('999_def_wrs.png', dpi=res, height=9*.8, width=16*.8)