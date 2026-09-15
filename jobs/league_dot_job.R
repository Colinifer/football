proj_name <- "football"
print(proj_name)

pkgs <- c(
  "devtools",
  "tidyverse",
  "DBI",
  "odbc",
  "RMariaDB",
  "readr",
  "pander",
  "furrr",
  "na.tools",
  "ggimage",
  "teamcolors",
  "glue",
  "dplyr",
  "jsonlite",
  "RJSONIO",
  "tictoc",
  "animation",
  "gt",
  "png",
  "DT",
  "ggthemes",
  "ggforce",
  "ggridges",
  "ggrepel",
  "ggbeeswarm",
  "cowplot",
  "gridExtra",
  "grid",
  "extrafont",
  "shadowtext",
  "tidytext",
  "RCurl"
)
installed_packages <- pkgs %in%
  rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(pkgs[!installed_packages])
}
lapply(pkgs, library, character.only = TRUE)
# library("nflscrapR") # doesn't work anymore
library("nflfastR")
# library("ffscrapr")

##reset
setwd("~/")
gid <- paste(getwd())
gid
device <- ""

if (gid == "/Volumes/HDD/Users/colinwelsh") {
  ## Maverick - MBP
  setwd("~/Documents/dev/football")
  device <- "Maverick (MBP)"
} else if (gid == "/Users/ColinWelsh") {
  ## Goose - iMac
  setwd("~/Documents/dev/football")
  device <- "Goose (iMac)"
} else if (gid == "/home/rstudio-user") {
  ## RStudio Cloud
  setwd("/cloud/project")
  device <- "RStudio Cloud"
}
print(paste(device, "is ready for some football", sep = " "))
rm(gid)


# Create standard objects -------------------------------------------------
source("../initR/con.R") 

# Clean data --------------------------------------------------------------

tic()
yr <- 2018
team_df <- read.csv('plots/assets/nfl_logo.csv', stringsAsFactors = F)
pff_id <- read.csv('data/players/all_players.csv', stringsAsFactors = F)

team_df$team_code[which(team_df$team_code=='OAK')] <- 'LV'
team_df$posteam[which(team_df$posteam=='OAK')] <- 'LV'
pff_id$team.abbreviation[which(pff_id$team.abbreviation=='OAK')] <- 'LV'
pff_id$team.city[which(pff_id$team.city=='Oakland')] <- 'Las Vegas'
pff_id$team.slug[which(pff_id$team.slug=='okland-raiders')] <- 'las-vegas-raiders'


# pbp_df <- readRDS(url('https://github.com/guga31bb/nflfastR-data/blob/master/data/play_by_play_2020.rds?raw=true'))
# yr_pbp <- lapply(yr, function(x) {
#   dbGetQuery(con, paste0("SELECT * FROM `football`.`pbp` WHERE `game_id` LIKE '%",x, "%'"), stringsAsFactors=F)
# })
# dbDisconnect(con)
# pbp_NFL <- yr_pbp[[1]]


pbp_df <- readRDS(url('https://github.com/guga31bb/nflfastR-data/blob/master/data/play_by_play_2020.rds?raw=true'))
pbp_NFL <- pbp_df
# pbp_NFL <- do.call(rbind, yr_pbp)
pbp_NFL$posteam[which(pbp_NFL$posteam=='SD')] <- 'LAC'
pbp_NFL$posteam[which(pbp_NFL$posteam=='JAC')] <- 'JAX'
pbp_NFL$posteam[which(pbp_NFL$posteam=='STL')] <- 'LA'
pbp_NFL$posteam[which(pbp_NFL$posteam=='OAK')] <- 'LV'

pbp_NFL$team_player <- paste0(pbp_NFL$receiver_player_id,'_',pbp_NFL$posteam)

# need to map correct IDs
adot_agg_df <- aggregate(cbind(air_yards,pass_attempt) ~ receiver_player_id + posteam + team_player + receiver_player_name, data = pbp_NFL, FUN = sum)
#adot_agg_df <- adot_agg_df[which(adot_agg_df$pass_attempt>=40),]
adot_agg_df <- adot_agg_df[order(-adot_agg_df$pass_attempt),]
adot_agg_df$tm_targ_order <- sapply(1:nrow(adot_agg_df), function(x) length(which(adot_agg_df$posteam[x]==adot_agg_df$posteam[1:x])))
adot_agg_df <- adot_agg_df[which(adot_agg_df$tm_targ_order <= 5),]
adot_agg_df$ADOT <- adot_agg_df$air_yards / adot_agg_df$pass_attempt
# Order by ADOT
adot_agg_df <- adot_agg_df[order(adot_agg_df$ADOT),]
adot_agg_df$tm_ADOT_order <- sapply(1:nrow(adot_agg_df), function(x) length(which(adot_agg_df$posteam[x]==adot_agg_df$posteam[1:x])))
# Order by Pass Attempt
adot_agg_df <- adot_agg_df[order(adot_agg_df$pass_attempt),]
adot_agg_df$tm_targ_order <- sapply(1:nrow(adot_agg_df), function(x) length(which(adot_agg_df$posteam[x]==adot_agg_df$posteam[1:x])))

freq_passers <- pbp_NFL[which(pbp_NFL$team_player %in% adot_agg_df$team_player),]
freq_passers$full_name <- pff_id$full_name[match(freq_passers$receiver_player_id, pff_id$nflfastR_id)]
freq_passers <- merge(freq_passers, adot_agg_df, by = 'team_player', suffixes = c('','_agg'))
freq_passers$tm_ADOT_order <- factor(freq_passers$tm_ADOT_order,1:5)
freq_passers$tm_targ_order <- factor(freq_passers$tm_targ_order,1:5)
freq_passers$posteam <- factor(freq_passers$posteam, team_df$team_code[order(team_df$division)])

fill_col <- team_df$primary
outline_col <- team_df$secondary
names(fill_col) <- team_df$team_code
names(outline_col) <- team_df$team_code

headshot_df <-
  data.frame(
    'full_name' = adot_agg_df$receiver_player_name,
    pff_url = paste0(
      'http://media.pff.com/player-photos/nfl/',
      pff_id$id[match(adot_agg_df$receiver_player_id, pff_id$nflfastR_id)],
      '.png'
    ),
    'posteam' = adot_agg_df$posteam,
    'tm_targ_order' = factor(adot_agg_df$tm_targ_order, 1:5),
    'tm_ADOT_order' = factor(adot_agg_df$tm_ADOT_order, 1:5),
    stringsAsFactors = F
  )
headshot_df$pff_url[which(headshot_df$pff_url == 'http://media.pff.com/player-photos/nfl/25578.png')] <-
  'https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/3916945.png&w=600&h=380'
headshot_df$pff_url[which(headshot_df$pff_url == 'http://media.pff.com/player-photos/nfl/48229.png')] <-
  'https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/3121422.png&w=600&h=380'
# headshot_df$pff_url[which(headshot_df$pff_url=='http://media.pff.com/player-photos/nfl/47931.png')] <- 'https://imagecomposer.nfl.com/?l=http://static.nfl.com/static/content/static/img/combine/2019/headshots/1400x1000/2562577.png&f=png&w=308&c=71'
headshot_df$pff_url[which(headshot_df$pff_url == 'http://media.pff.com/player-photos/nfl/47931.png')] <-
  'https://a.espncdn.com/combiner/i?img=/i/headshots/college-football/players/full/4036163.png&w=600&h=380'
headshot_df$posteam <-
  factor(headshot_df$posteam, team_df$team_code[order(team_df$division)])

unmatched <- adot_agg_df$receiver_player_id[is.na(match(adot_agg_df$receiver_player_id, pff_id$GSIS_ID))]
pbp_NFL$receiver_player_name[match(unmatched, pbp_NFL$receiver_player_id)]


team_df$posteam <- factor(team_df$team_code, team_df$team_code[order(team_df$division)])
#pff_id[which(pff_id$id==48229),]
toc()
img_size_adj <- 100/(8*4)

tic()
p_new <- ggplot(freq_passers, aes(y = tm_targ_order, x = air_yards)) +
  # Team Logo
  geom_image(data = team_df, aes(image = logo, x = 17.5, y = 3.5), size = 0.80) +
  # Team logo washed
  geom_image(data = team_df, aes(image = logo, x = 17.5, y = 3.5), size = 0.80, color = 'white', alpha = 0.8) +
  # Player headshot
  geom_image(data = headshot_df, aes(image = pff_url, x = 38, y = tm_targ_order), size = 0.25, nudge_y = 0.5) +
  geom_density_ridges2(aes(y = tm_targ_order, fill = posteam, color = posteam), scale = 1.7, size = 0.2, na.rm = T, show.legend = F, bandwidth = 2, panel_scaling = F) +
  # ---------
  # geom_text(aes(label = full_name, x = 35), nudge_y = .75, hjust = 1, size = 0.8 * img_size_adj, color = 'darkblue') +
  # geom_text(data = team_df, aes(label = posteam, x = -1, y = 4.8), angle = 45, size = 1.5 * img_size_adj, color = 'darkblue') +
  # geom_text(aes(label = 0, x = 0, y = 1), nudge_y = -0.15, size = 0.7 * img_size_adj, color = 'grey70') +
  # geom_text(aes(label = 10, x = 10, y = 1), nudge_y = -0.15, size = 0.7 * img_size_adj, color = 'grey70') +
  # geom_text(aes(label = 20, x = 20, y = 1), nudge_y = -0.15, size = 0.7 * img_size_adj, color = 'grey70') +
  # geom_text(aes(label = 30, x = 30, y = 1), nudge_y = -0.15, size = 0.7 * img_size_adj, color = 'grey70') +
  # ---------
  # Player full name
  geom_shadowtext(data = headshot_df, aes(label = full_name, x = 35), nudge_y = .75, hjust = 1, size = 1.1 * img_size_adj, color = 'black', bg.color = 'white', bg.r = 0.15, na.rm = TRUE) +
  # Team name
  # geom_shadowtext(data = team_df, aes(label = posteam, x = -1, y = 5.7), angle = 45, size = 1.5 * img_size_adj, color = 'black', bg.color = 'white', bg.r = 0.1, na.rm = TRUE) +
  # X Axis Labels
  geom_shadowtext(aes(label = 0, x = 0, y = 1), nudge_y = -0.15, size = 0.7 * img_size_adj, color = 'grey70', bg.color = 'white', bg.r = 0.15, na.rm = TRUE) +
  geom_shadowtext(aes(label = 10, x = 10, y = 1), nudge_y = -0.15, size = 0.7 * img_size_adj, color = 'grey70', bg.color = 'white', bg.r = 0.15, na.rm = TRUE) +
  geom_shadowtext(aes(label = 20, x = 20, y = 1), nudge_y = -0.15, size = 0.7 * img_size_adj, color = 'grey70', bg.color = 'white', bg.r = 0.15, na.rm = TRUE) +
  geom_shadowtext(aes(label = 30, x = 30, y = 1), nudge_y = -0.15, size = 0.7 * img_size_adj, color = 'grey70', bg.color = 'white', bg.r = 0.15, na.rm = TRUE) +
  # Y Axis Labels
  # geom_shadowtext(aes(label = 0, x = 0, y = 1), nudge_y = -0.15, size = 0.7 * img_size_adj, color = 'grey70', bg.color = 'white', bg.r = 0.15, na.rm = TRUE) +
  # geom_shadowtext(aes(label = 10, x = 10, y = 1), nudge_y = -0.15, size = 0.7 * img_size_adj, color = 'grey70', bg.color = 'white', bg.r = 0.15, na.rm = TRUE) +
  # geom_shadowtext(aes(label = 20, x = 20, y = 1), nudge_y = -0.15, size = 0.7 * img_size_adj, color = 'grey70', bg.color = 'white', bg.r = 0.15, na.rm = TRUE) +
  # geom_shadowtext(aes(label = 30, x = 30, y = 1), nudge_y = -0.15, size = 1 * img_size_adj, color = 'grey70', bg.color = 'white', bg.r = 0.15, na.rm = TRUE) +
  scale_color_manual(values = outline_col) + 
  scale_fill_manual(values = fill_col)
toc()

tic()
ggsave(
  paste0("plots/desktop/", yr, "_team_ridge_DOT_no_images.png"),
  plot = p_new +
    facet_wrap(~ posteam, ncol = 8, shrink = FALSE) +
    labs(
      title = 'Receiver Depth of Target Distribution',
      subtitle = paste0(yr, " NFL Regular Season"),
      x = 'Air Yards'
    ) +
    scale_x_continuous(limits = c(-5, 40), expand = c(0, 0)) +
    scale_y_discrete(expand = expansion(add = c(0.3, 1))) +
    theme_bw() +
    theme(
      text = element_text(color = 'black'),
      plot.background = element_rect(fill = 'grey95', color = 'grey95'),
      panel.border = element_rect(color = 'black'),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      axis.text = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      plot.title = element_text(size = 18),
      plot.subtitle = element_text(size = 12),
      plot.caption = element_text(size = 7),
      legend.background = element_rect(fill = 'grey90', color = 'black'),
      legend.key = element_blank(),
      strip.background = element_blank(),
      # strip.text.x = element_blank(),
      panel.spacing = unit(0.1, "lines")
    ),
  width = 25,
  height = 14,
  dpi = 100,
  limitsize = FALSE
)
toc()

tic()
ggsave(
  paste0("plots/mobile/", yr, "_team_ridge_DOT_no_images.png"),
  plot = p_new +
    facet_wrap(~ posteam, ncol = 4, shrink = FALSE) +
    labs(
      title = 'Receiver Depth of Target Distribution',
      subtitle = paste0(yr, " NFL Regular Season"),
      x = 'Air Yards'
    ) +
    scale_x_continuous(limits = c(-5, 40), expand = c(0, 0)) +
    scale_y_discrete(expand = expansion(add = c(0.3, 1))) +
    theme_bw() +
    theme(
      text = element_text(color = 'black'),
      plot.background = element_rect(fill = 'grey95', color = 'grey95'),
      panel.border = element_rect(color = 'black'),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      axis.text = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      plot.title = element_text(size = 18),
      plot.subtitle = element_text(size = 12),
      plot.caption = element_text(size = 7),
      legend.background = element_rect(fill = 'grey90', color = 'black'),
      legend.key = element_blank(),
      strip.background = element_blank(),
      # strip.text.x = element_blank(),
      panel.spacing = unit(0.1, "lines")
    ),
  width = 14,
  height = 25,
  dpi = 100,
  limitsize = FALSE
)
toc()