

year

my_key <- sportradar_con %>% 
  filter(sportradar_sports == "NFL Official") %>% 
  select(sportradar_keys) %>% 
  as.character()

szn_json <- RJSONIO::fromJSON(glue('http://api.sportradar.us/nfl/official/trial/v6/en/games/{year}/REG/schedule.json?api_key={my_key}'))
write(RJSONIO::toJSON(szn_json), glue('data/schedules/{year}.json'))

yr_sched <- RJSONIO::fromJSON(glue('data/schedules/{year}.json'))

all_id <- unlist(sapply(yr_sched$weeks, function(wk) {
  sapply(wk$games, function(gm) {
    gm$id
  })
}))

gm_status <- unlist(sapply(yr_sched$weeks, function(wk) {
  sapply(wk$games, function(gm) {
    gm$status
  })
}))

gm_df <- data.frame(all_id, gm_status, stringsAsFactors = F)

# play by play
gm_done <- gsub('.json','',dir(glue('/data/schedules/{year}')))
loop_id <- gm_df %>% 
  filter(gm_status=='closed' & !(all_id %in% gm_done)) %>% 
  pull(all_id)

for (j in loop_id) {
  gm_json <- RJSONIO::fromJSON(glue('http://api.sportradar.us/nfl/official/trial/v5/en/games/{j}/pbp.json?api_key={my_key}'))
  write(RJSONIO::toJSON(gm_json), glue('/data/schedules/{year}/{j}.json'))
}

# participation
gm_done <- gsub('.json','',dir(paste0(paste0('data/part/',year))))
loop_id <- gm_df %>% 
  filter(gm_status=='closed' & !(all_id %in% gm_done)) %>% 
  pull(all_id)

for (j in loop_id) {
  gm_json <- fromJSON(paste0('http://api.sportradar.us/nfl/official/trial/v5/en/plays/',j,'/participation.json?api_key=',my_key))
  write(toJSON(gm_json), paste0('data/part/',year,'/',j,'.json'))
}


# # generate sched
# games_df <- readRDS(url('http://nflgamedata.com/games.rds'))
# sched_json <- fromJSON(paste0('sched/',year,'.json'))
# 
# gms_list <- lapply(sched_json$weeks, function(wk) {
#   t(sapply(wk$games, function(gm) {
#     c(
#       'season'=year,
#       'week'=wk$title,
#       'home_tm'=gm$home$alias,
#       'away_tm'=gm$away$alias,
#       'game_id'=gm$id
#     )}))
# })
# sport_gm_df <- data.frame(do.call(rbind, gms_list), stringsAsFactors=F)
# 
# sport_gm_df %>%
#   filter(!is.na(week)) %>%
#   mutate(
#     away_tm = ifelse(away_tm=='JAC','JAX',away_tm),
#     home_tm = ifelse(home_tm=='JAC','JAX',home_tm)
#   ) %>%
#   mutate(game_id_fstR = paste(season,ifelse(as.numeric(week)<10,paste0('0',week),week),away_tm,home_tm,sep = '_')) %>%
#   select(game_id = game_id_fstR, game_id_SR = game_id) %>%
#   left_join(games_df) %>%
#   saveRDS('games_2020.rds')
