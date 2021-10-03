player_stats %>% 
  left_join(roster_df %>% 
              select(gsis_id, position, headshot_url),
            by = c('player_id' = 'gsis_id')) %>% 
  filter(position %in% c('QB', 'RB', 'WR', 'TE')) %>% 
  filter(position %in% c('WR', 'TE')) %>% 
  group_by(recent_team) %>% 
  arrange(-offense_snaps) %>% 
  mutate(rank = rank(-offense_snaps) %>% round()) %>% 
  select(rank, headshot_url, position, player_id:offense_pct) %>% 
  filter(rank <= 5) %>% 
  ggplot(aes(y = offense_pct, x = rank)) + 
  geom_bar(stat='identity') + 
  coord_flip() + 
  scale_x_reverse() +
  facet_wrap(~recent_team, ncol = 8)

headshots <- player_stats %>% 
  left_join(roster_df %>% 
              select(gsis_id, position, headshot_url, season),
            by = c('player_id' = 'gsis_id')) %>% 
  filter(position %in% c('QB', 'RB', 'WR', 'TE')) %>% 
  filter(position %in% c('WR', 'TE')) %>% 
  # group_by(recent_team) %>% 
  arrange(-offense_snaps) %>% 
  mutate(rank = rank(-offense_snaps) %>% round()) %>% 
  select(rank, season, headshot_url, position, player_id:offense_pct) %>% 
  filter(!is.na(headshot_url)) %>% 
  select(season, headshot_url, player_id)

year <- 2021

existing_headshots <- list.files(path = paste0('data/headshots/', year, '/')) %>% 
  gsub(pattern = '.png', replacement = '')

headshots <- roster_df %>%
  filter(season == year & position %in% c('QB', 'WR', 'RB', 'TE') &
           !is.na(gsis_id) & 
           !(gsis_id %in% existing_headshots)) %>%
  unique() %>%
  pull(headshot_url)

map(headshots, function(x){
  print(x)
  join_data <- roster_df %>% 
    filter(headshot_url == x & season == year)
  try(download.file(url = join_data$headshot_url, destfile = paste0('data/headshots/', join_data$season, '/', join_data$gsis_id, '.png'), method='curl'))
  print(join_data$headshot_url)
})

