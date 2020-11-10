library(zoo)
library(viridis)
source('plots/assets/plot_theme.R')
load(url('https://github.com/guga31bb/metrics/blob/master/dakota_model.rda?raw=true'))


# roster_df <- readRDS(url('https://github.com/guga31bb/nflfastR-data/blob/master/roster-data/roster.rds?raw=true')) %>% 
#   left_join(readRDS(url('https://github.com/ajreinhard/NFL/blob/master/nflfastR%20ID%20mapping/gsis_map.rds?raw=true')), by = c('teamPlayers.gsisId' = 'gsis')) %>% 
#   mutate(ID = ifelse(is.na(ID), teamPlayers.gsisId, ID))

roster_df <-
  readRDS(url('https://github.com/guga31bb/nflfastR-data/blob/master/roster-data/roster.rds?raw=true')
  ) %>% 
  decode_player_ids(fast = T)

print("Scraping 2006:2020 PBP for career results")
pbp_df <- do.call(rbind, lapply(2006:year, function(yr) {
  readRDS(url(glue('https://github.com/guga31bb/nflfastR-data/blob/master/data/play_by_play_{yr}.rds?raw=true')))
})) %>% decode_player_ids(fast = T)

all_qb_id <- pbp_df %>% 
  pull(passer_id) %>% table %>% 
  .[which(. >= 50)] %>% 
  names

qb_2020_id <- pbp_df %>% 
  filter(season==2020) %>% 
  pull(passer_id) %>% 
  table %>% 
  .[which(. >= 3)] %>%
  names

qb_top_bottom <- pbp_df %>% 
  mutate(qb_id = ifelse(is.na(passer_id), rusher_id, passer_id)) %>%
  filter(qb_id %in% all_qb_id) %>% 
  group_by(qb_id) %>%
  rename(play_cpoe = cpoe) %>% 
  mutate(
    play_num = row_number(),
    week_desc = paste0(season, ' Week ', week),
    qb_epa_cap = ifelse(qb_epa <= -4.5, -4.5, qb_epa),
    epa_per_play = rollapply(qb_epa_cap, 200, mean, align='right', fill=NA, na.rm = T),
    cpoe = rollapply(play_cpoe, 200, mean, align='right', fill=NA, na.rm = T),
    gm_list = rollapply(game_id, 200, function(x) paste(rev(unique(x)),collapse = '\n'), align='right', fill=NA),
    end_game = ifelse(game_id != lead(game_id) | is.na(lead(game_id)), 1, 0),
    game_num = cumsum(end_game),
    last_throw = ifelse(play_num == max(play_num), 1, NA)
  ) %>% 
  ungroup %>% 
  mutate(dakota = mgcv::predict.gam(dakota_model, .)) %>%
  group_by(qb_id) %>%
  summarise(
    car_plays = n(),
    curr_dakota = max(last_throw * dakota, na.rm = T),
    high_dakota = max(dakota, na.rm = T),
    low_dakota = min(dakota, na.rm = T),
    epa_per_play =  mean(epa_per_play, na.rm = T),
    cpoe = mean(cpoe, na.rm = T)
  ) %>% 
  mutate(car_dakota = mgcv::predict.gam(dakota_model, .))

qb_top_bottom <- qb_top_bottom %>% 
  left_join(sleep.players %>% select(gsis_id, full_name, headshot_url),
            by = c("qb_id" = "gsis_id"))

min_plays <- 200

p <- qb_top_bottom %>% 
  # left_join(roster_df %>% filter(team.season >= (as.integer(year) - 1)), by = c('qb_id' = 'teamPlayers.gsisId')) %>%
  # filter(!is.na(team.season) & car_plays>=min_plays & qb_id %in% qb_2020_id) %>% 
  filter(car_plays>=min_plays & qb_id %in% qb_2020_id) %>% 
  arrange(-car_dakota) %>% 
  mutate(rank = row_number()) %>% 
  ggplot(aes(x = curr_dakota, xend = high_dakota, y = rank, yend = rank)) +
  geom_segment(aes(x = low_dakota), color = color_cw[5], size = 0.7) +
  geom_point(aes(x = car_dakota), color = color_cw[5], shape = 5) +
  geom_point(aes(fill = curr_dakota), size = 3, color = color_cw[2], shape = 21, stroke = 0.7) +
  geom_shadowtext(aes(x = low_dakota - 0.005, label = full_name, family = "Montserrat"), hjust = 1, color = color_cw[5], bg.color = color_cw[2], size = 2.2, bg.r = 0.2) +
  # geom_image(aes(x = low_dakota - 0.005, image = headshot_url), size = .025, na.rm = T) +
  scale_y_reverse(expand = expansion(mult = c(0.02, 0.04))) +
  scale_x_continuous(limits = c(-0.15,0.4), expand = expansion(mult = 0)) +
  scale_fill_viridis(option = "A") +
  labs(title = 'Range of Best & Worst EPA+CPOE Composite Index' ,
       subtitle = glue('Diamond = Career average, Dot = Last 200 Plays | Min. {min_plays} QB plays'),
       fill = "Index score",
       x = 'EPA+CPOE Composite Index',
       y = NULL) +
  theme_cw +
  theme(
    plot.title = element_text(size = 12),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = c(.85, .22)
  )

# p

brand_plot(p, asp = 1/1.25, save_name = 'plots/desktop/dakota_career.png', data_home = 'EPA+CPOE courtesy of @benbbaldwin | Data: @nflfastR', fade_borders = '')
