snaps_df <- player_stats_weekly %>% 
  select(player_id,
         player_name,
         recent_team,
         week,
         offense_snaps,
         offense_pct) %>% 
  # filter(week < 4) %>% 
  group_by(player_id) %>% 
  mutate(snap_pct_diff = offense_pct - lag(offense_pct, 1)) %>% 
  left_join(roster_df %>% 
              select(player_id = gsis_id, position),
            by = c('player_id')) %>% 
  filter(position %in% c('RB', 'WR', 'TE') 
         # & offense_pct >= 0.43
         ) %>% 
  arrange(-snap_pct_diff)
  
p <- snaps_df %>% 
  ggplot(aes(x = week, y = offense_pct)) + 
  geom_line(aes(group = player_id)) + 
  geom_text_repel(data = snaps_df %>% filter(week == max(snaps_df$week) & offense_pct > .2), aes(label = player_name)) +
  facet_wrap(~ recent_team, ncol = 8) + 
  theme_cw_light

p
