library(tidyverse)

season <- year

con <- fx.db_con(x.host = 'localhost')

pbp_df <-
  # purrr::map_df(season, function(x) {
  #   readRDS(
  #     # url(glue::glue("https://github.com/guga31bb/nflfastR-data/blob/master/data/play_by_play_{x}.rds?raw=true"))
  #     glue::glue('data/pbp/play_by_play_{x}.rds')
  #     )
  # }) %>%
  tbl(con, 'nflfastR_pbp') %>% 
  filter(season >= current_season & 
           game_id != '2020_12_NO_DEN') %>% # THis game is pointless
  collect() %>% 
  decode_player_ids(fast = TRUE) 

prem_epa_df <- pbp_df %>%
  filter(down <= 2 & wp <= .8 & wp >= .2 & half_seconds_remaining >= 120) %>%
  group_by(posteam) %>%
  summarize(
    pass_freq = mean(pass),
    pass_epa = mean(ifelse(pass == 1, epa, NA), na.rm = T),
    run_epa = mean(ifelse(rush == 1, epa, NA), na.rm = T),
    pass_epa_prem = pass_epa - run_epa
  ) %>% left_join(teams_colors_logos,
                  by = c('posteam' = 'team_abbr'))

label_df <- data.frame(
  pass_freq = c(.675, .425, .675, .425),
  pass_epa_prem = c(.42, .42, -.02, -.02),
  label = c(
    "Better at Passing\nPass A Lot",
    "Better at Passing\nRun A Lot",
    "Better at Running\nPass A Lot",
    "Better at Running\nRun A Lot"
  )
)

pbp_df %>%
  filter(down <= 2 & wp <= .8 & wp >= .2 & half_seconds_remaining >= 120) %>%
  group_by(season) %>%
  summarize(
    pass_freq = mean(pass),
    pass_epa = mean(ifelse(pass == 1, epa, NA), na.rm = T),
    run_epa = mean(ifelse(rush == 1, epa, NA), na.rm = T),
    pass_epa_prem = pass_epa - run_epa
  )


library(scales) # fixes labels
library(shadowtext) # geom_shadowtext adds a thin border around your text, making it more readable
library(ggpmisc) # geom_grob is my preferred function for placing images on plots
library(grid) # convert an image to a raster object
library(magick) # read our image in so we can make manipulations, if needed

plus_lab <- function(x, accuracy = NULL, suffix = "") paste0(ifelse(x > 0, "+", ""), number(x, accuracy = accuracy, suffix = suffix, scale = ifelse(suffix == "%", 100, 1)))
plus_lab_format <- function(accuracy = NULL, suffix = "") function(x) plus_lab(x, accuracy = accuracy, suffix = suffix)

grob_img_adj <- function(img_url, alpha = 1, whitewash = 0) {
  return(lapply(img_url, function(x) {
    if (is.na(x)) {
      return(NULL)
    } else {
      img <- magick::image_read(x)[[1]]
      img[1, , ] <- as.raw(255 - (255 - as.integer(img[1, , ])) * (1 - whitewash))
      img[2, , ] <- as.raw(255 - (255 - as.integer(img[2, , ])) * (1 - whitewash))
      img[3, , ] <- as.raw(255 - (255 - as.integer(img[3, , ])) * (1 - whitewash))
      img[4, , ] <- as.raw(as.integer(img[4, , ]) * alpha)
      return(grid::rasterGrob(image = magick::image_read(img)))
    }
  }))
}

p <- ggplot(data = prem_epa_df, aes(x = pass_epa_prem, y = pass_freq)) +
  geom_hline(
    yintercept = 0.525,
    color = "red",
    linetype = "52",
    size = 0.2
  ) +
  geom_vline(
    xintercept = 0.198,
    color = "red",
    linetype = "52",
    size = 0.2
  ) +
  geom_shadowtext(
    data = label_df,
    aes(label = label),
    family = 'Montserrat',
    color = color_cw[5],
    size = 1.75,
    bg.color = color_cw[2],
    bg.r = 0.2
  ) +
  geom_grob(aes(
    x = pass_epa_prem,
    y = pass_freq,
    label = grob_img_adj(team_logo_espn, alpha = 0.8),
    vp.height = 0.15
  )) +
  scale_x_continuous(
    labels = plus_lab_format(accuracy = .01),
    breaks = seq(-1, 1, .1),
    limits = c(-0.1, 0.5),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    breaks = seq(0, 1, .05),
    limits = c(.4, .7),
    expand = expansion(mult = c(0.03, 0.03))
  ) +
  labs(
    title = "Dropback Premium vs Dropback Frequency",
    subtitle = "Early Downs with Win Probability Between 20% and 80% Outside of Last Two\nMinutes of The Half",
    y = "Dropback\nFreq",
    x = "(EPA/Play on Dropbacks) - (EPA/Play on Designed Runs)"
  ) +
  theme_cw_dark

brand_plot(p, asp = 16/10, save_name = glue('plots/desktop/team_tiers/team_dropback_{season}.png'), data_home = 'Data: @nflfastR', fade_borders = '')

rm(prem_epa_df, label_df, plus_lab, plus_lab_format, grob_img_adj, p)