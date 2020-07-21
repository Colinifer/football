``` r
nfls <- read_csv(url('https://github.com/ryurko/nflscrapR-data/blob/master/play_by_play_data/regular_season/reg_pbp_2015.csv?raw=true')) %>%
  select(game_id, play_id, epa)
path <- "../nflfastR-raw/raw"

y = 2015

# get reg and post games with scraper
nflf <- fast_scraper_schedules(y) %>%
  pull(game_id) %>%
  fast_scraper(pp = TRUE, dir = path) %>%
  clean_pbp() %>%
  add_qb_epa()

j <- nfls %>%
  mutate(game_id = as.character(game_id)) %>% 
  left_join(nflf, by = c('game_id' = 'old_game_id', 'play_id')) %>%
  filter(!is.na(down), !is.na(epa.x), !is.na(epa.y), rush == 1 | pass == 1) %>%
  select(game_id, play_id, yardline_100, down, ydstogo, qtr, epa.x, epa.y)

cor(j$epa.x, j$epa.y)  

p <- ggplot(j, aes(x=epa.x, y=epa.y)) + 
  geom_point(alpha = .3) + 
  facet_wrap(~ down, ncol = 4) +
  geom_smooth(aes(x = epa.x, y = epa.y), method = "loess") +
  geom_abline(slope = 1, intercept = 0, color = "black", lty = 2) +
  coord_equal() +   
  theme_bw() +
  labs(title = 'EPA in nflscrapR vs nflfastR, 2015, rush & pass plays, by down',
       x = "nflscrapR EPA",
       y = "nflfastR EPA") +
  theme(plot.title = element_text(hjust = 0.5),
        strip.background = element_blank(),
        strip.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 10, angle = 90))

p
  
```