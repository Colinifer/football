library(ggplot2)
library(extrafont)
library(ggridges)
library(shadowtext)
library(teamcolors)
library(ggimage)
source('C:/Users/Owner/Documents/data-viz/fancy footer.R')

team_df <- read.csv('plots/assets/nfl_logo.csv', stringsAsFactors = F)
pff_id <- read.csv('data/players/All_Player_Info.csv', stringsAsFactors = F)

yr_pbp <- lapply(2019, function(yr) {
  read.csv(paste0('pbp/reg/reg__pbp_',yr,'.csv'), stringsAsFactors=F)
})
pbp_NFL <- do.call(rbind, yr_pbp)
pbp_NFL$posteam[which(pbp_NFL$posteam=='SD')] <- 'LAC'
pbp_NFL$posteam[which(pbp_NFL$posteam=='JAC')] <- 'JAX'
pbp_NFL$posteam[which(pbp_NFL$posteam=='STL')] <- 'LA'

pbp_NFL$team_player <- paste0(pbp_NFL$receiver_player_id,'_',pbp_NFL$posteam)

adot_agg_df <- aggregate(cbind(air_yards,pass_attempt) ~ receiver_player_id + posteam + team_player, data = pbp_NFL, FUN = sum)
#adot_agg_df <- adot_agg_df[which(adot_agg_df$pass_attempt>=40),]
adot_agg_df <- adot_agg_df[order(-adot_agg_df$pass_attempt),]
adot_agg_df$tm_targ_order <- sapply(1:nrow(adot_agg_df), function(x) length(which(adot_agg_df$posteam[x]==adot_agg_df$posteam[1:x])))
adot_agg_df <- adot_agg_df[which(adot_agg_df$tm_targ_order < 5),]
adot_agg_df$ADOT <- adot_agg_df$air_yards / adot_agg_df$pass_attempt
adot_agg_df <- adot_agg_df[order(adot_agg_df$ADOT),]
adot_agg_df$tm_ADOT_order <- sapply(1:nrow(adot_agg_df), function(x) length(which(adot_agg_df$posteam[x]==adot_agg_df$posteam[1:x])))

freq_passers <- pbp_NFL[which(pbp_NFL$team_player %in% adot_agg_df$team_player),]
freq_passers$full_name <- pff_id$full_name[match(freq_passers$receiver_player_id, pff_id$GSIS_ID)]
freq_passers <- merge(freq_passers, adot_agg_df, by = 'team_player', suffixes = c('','_agg'))
freq_passers$tm_ADOT_order <- factor(freq_passers$tm_ADOT_order,1:4)
freq_passers$posteam <- factor(freq_passers$posteam, team_df$team_code[order(team_df$division)])

fill_col <- team_df$primary
outline_col <- team_df$secondary
names(fill_col) <- team_df$team_code
names(outline_col) <- team_df$team_code

headshot_df <- data.frame(pff_url=paste0('http://media.pff.com/player-photos/nfl/',pff_id$id[match(adot_agg_df$receiver_player_id, pff_id$GSIS_ID)],'.png'), 'posteam'=adot_agg_df$posteam, 'tm_ADOT_order'=factor(adot_agg_df$tm_ADOT_order,1:4), stringsAsFactors=F)
headshot_df$pff_url[which(headshot_df$pff_url=='http://media.pff.com/player-photos/nfl/25578.png')] <- 'https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/3916945.png&w=600&h=380'
headshot_df$pff_url[which(headshot_df$pff_url=='http://media.pff.com/player-photos/nfl/48229.png')] <- 'https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/3121422.png&w=600&h=380'
headshot_df$posteam <- factor(headshot_df$posteam, team_df$team_code[order(team_df$division)])

unmatched <- adot_agg_df$receiver_player_id[is.na(match(adot_agg_df$receiver_player_id, pff_id$GSIS_ID))]
#pbp_NFL$receiver_player_name[match(unmatched, pbp_NFL$receiver_player_id)]


team_df$posteam <- factor(team_df$team_code, team_df$team_code[order(team_df$division)])
#pff_id[which(pff_id$id==48229),]

img_size_adj <- 50/(8*4)

p <- ggplot(data = freq_passers, aes(y = tm_ADOT_order, x = air_yards)) +
  geom_image(data = team_df, aes(image = logo, x = 17.5, y = 2.8), size = 0.80) +
  geom_image(data = team_df, aes(image = logo, x = 17.5, y = 2.8), size = 0.80, color = 'white', alpha = 0.8) +
  geom_image(data = headshot_df, aes(image = pff_url, x = 38, y = tm_ADOT_order), size = 0.22 * img_size_adj, nudge_y = 0.5) +
  geom_density_ridges2(aes(y = tm_ADOT_order, fill = posteam, color = posteam), scale = 1.7, size = 0.2, na.rm = T, show.legend = F, bandwidth = 2, panel_scaling = F) +
  geom_shadowtext(aes(label = full_name, x = 35), nudge_y = .75, hjust = 1, family='HP Simplified', size = 0.8 * img_size_adj, color = 'darkblue', bg.color = 'white', bg.r = 0.15) +
  geom_shadowtext(data = team_df, aes(label = posteam, x = -1, y = 4.8), angle = 45, family='HP Simplified', size = 1.5 * img_size_adj, color = 'darkblue', bg.color = 'white', bg.r = 0.1) +
  geom_shadowtext(aes(label = 0, x = 0, y = 1), nudge_y = -0.15, family='HP Simplified', size = 0.7 * img_size_adj, color = 'grey70', bg.color = 'white', bg.r = 0.15) +
  geom_shadowtext(aes(label = 10, x = 10, y = 1), nudge_y = -0.15, family='HP Simplified', size = 0.7 * img_size_adj, color = 'grey70', bg.color = 'white', bg.r = 0.15) +
  geom_shadowtext(aes(label = 20, x = 20, y = 1), nudge_y = -0.15, family='HP Simplified', size = 0.7 * img_size_adj, color = 'grey70', bg.color = 'white', bg.r = 0.15) +
  geom_shadowtext(aes(label = 30, x = 30, y = 1), nudge_y = -0.15, family='HP Simplified', size = 0.7 * img_size_adj, color = 'grey70', bg.color = 'white', bg.r = 0.15) +
  scale_color_manual(values = outline_col) + 
  scale_fill_manual(values = fill_col) + 
  facet_wrap(~ posteam, nrow = 8, shrink= F) +
  labs(title = 'Receiver Depth of Target Distribution', 
       subtitle = '2015 to 2019 NFL Regular Season', 
       x = 'Air Yards') +
  scale_x_continuous(limits = c(-5,40), expand = c(0,0)) +
  scale_y_discrete(expand = expansion(add = c(0.3, 1))) +
  theme_bw() +
  theme(
    text = element_text(family='HP Simplified', color='darkblue'),
    plot.background = element_rect(fill = 'grey95', color = 'grey95'),
    panel.border = element_rect(color = 'darkblue'),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.title = element_text(size = 14),
    plot.subtitle = element_text(size = 8),
    plot.caption = element_text(size = 5),
    legend.background = element_rect(fill = 'grey90',color = 'darkblue'),
    legend.key = element_blank(),
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    panel.spacing = unit(0.1, "lines")
  )


brand_plot(p, asp = 16/9, save_name = 'Team_WR_ridge_DOT_2015_2019.png', data_home = 'Data: @nflscrapR')


#table(freq_passers$full_name[which(freq_passers$posteam=='KC')])
#tm_air <- aggregate(pass_attempt ~ air_yards + full_name, data = freq_passers, subset = posteam=='MIA', FUN = sum) 
#tm_all <- aggregate(pass_attempt ~ full_name, data = freq_passers, subset = posteam=='MIA', FUN = sum) 
#full_tm <- merge(tm_air, tm_all, by = 'full_name', suffixes = c('','_all'))
#full_tm$den <- full_tm$pass_attempt/ full_tm$pass_attempt_all
#max(full_tm$den)
