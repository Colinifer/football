
year

sr_key <- sportradar_con %>% 
  filter(sportradar_sports == "NFL Official") %>% 
  select(sportradar_keys) %>% 
  as.character()

# Automate to make calls only on Tuesdays
season_json <- jsonlite::fromJSON(url(glue('http://api.sportradar.us/nfl/official/trial/v6/en/games/{year}/REG/schedule.json?api_key={sr_key}')))
jsonlite::write_json(season_json, glue('data/schedules/{year}.json'))
season_json <- read_json(glue('data/schedules/{year}.json'))

yr_sched <- read_json(glue('data/schedules/{year}.json'))

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

gm_scheduled <- unlist(sapply(yr_sched$weeks, function(wk) {
  sapply(wk$games, function(gm) {
    gm$status
  })
}))

gm_df <- data.frame(all_id, gm_status, stringsAsFactors = F)

# Check missing play by play files
gm_done <- gsub('.json','',dir(glue('data/{year}')))
loop_id <- gm_df %>% 
  filter(gm_status=='closed' & !(all_id %in% gm_done)) %>% 
  pull(all_id)

loop_id
# Download missing play by play files
lapply(loop_id, function(x){
  gm_json <- jsonlite::fromJSON(url(glue('https://api.sportradar.us/nfl/official/trial/v5/en/games/{x}/pbp.json?api_key={sr_key}')))
  jsonlite::write_json(gm_json, glue('data/{year}/{x}.json'))
  Sys.sleep(3)
  })

# Check missing participation files
# Participation is updated weekly on Fridays
gm_done <- gsub('.json','',dir(glue('data/part/{year}')))
loop_id <- gm_df %>% 
  filter(gm_status=='closed' & !(all_id %in% gm_done) & ) %>% 
  pull(all_id)

loop_id
# Download missing participation files
lapply(loop_id, function(x){
  gm_json <- jsonlite::fromJSON(url(glue('https://api.sportradar.us/nfl/official/trial/v6/en/plays/{x}/participation.json?api_key={sr_key}')))
  jsonlite::write_json(gm_json, glue('data/part/{year}/{x}.json'))
  Sys.sleep(3)
  # on.exit(close(gm_json))
  })


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
