x.season_passers <- pbpSeason$passer_player_name
x.season_passers <- x.season_passers[!is.na(x.season_passers)] %>% unique()

for (x in x.season_passers) {
  x.pbp <- pbpSeason %>% filter(passer_player_name == x)
  mean(x.pbp$epa)
  xplayers[xplayers$playerID == z, "epa_per_play"]
}

x.season_passers

x.qb_epa <- pbpSeason %>% filter(passer_player_name == "R. Wilson")
mean(x.qb_epa$epa)




targets <- filter(xpbp, PlayType == "Pass" & Receiver != "NA")
xreceivers <- unique(targets$Receiver_ID)

## add targets to stats
for (z in xreceivers) {
  xplayers[xplayers$playerID == z, "targets"] <- sum(targets$Receiver_ID == z)
}


schotty <- pbp_rp %>%
  filter(wp > .20 & wp < .80 & down <= 2 & qtr <= 2 & half_seconds_remaining > 120 & !posteam %in% c("APR", "NPR")) %>%
  group_by(posteam) %>%
  summarise(mean_pass = mean(pass), 
            plays = n()) %>%
  arrange(mean_pass)