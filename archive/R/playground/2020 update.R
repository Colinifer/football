
year

sr_key <- sportradar_con %>% 
  filter(sportradar_sports == "NFL Official") %>% 
  select(sportradar_keys) %>% 
  as.character()

# Automate to make calls only on Tuesdays
season_json <- jsonlite::fromJSON(url(glue('http://api.sportradar.us/nfl/official/trial/v6/en/games/{year}/REG/schedule.json?api_key={sr_key}')))
jsonlite::write_json(season_json, glue('data/schedules/{year}.json'))
sr_games_json <- read_json(glue('data/schedules/{year}.json'))

all_id <- unlist(sapply(sr_games_json$weeks, function(wk) {
  sapply(wk$games, function(gm) {
    gm$id
  })
}))
gm_status <- unlist(sapply(sr_games_json$weeks, function(wk) {
  sapply(wk$games, function(gm) {
    gm$status
  })
}))
gm_scheduled <- unlist(sapply(sr_games_json$weeks, function(wk) {
  sapply(wk$games, function(gm) {
    gm$scheduled
  })
}))
gm_home <- unlist(sapply(sr_games_json$weeks, function(wk) {
  sapply(wk$games, function(gm) {
    gm$home$alias
  })
}))
gm_away <- unlist(sapply(sr_games_json$weeks, function(wk) {
  sapply(wk$games, function(gm) {
    gm$away$alias
  })
}))
gm_venue <- unlist(sapply(sr_games_json$weeks, function(wk) {
  sapply(wk$games, function(gm) {
    gm$venue$name
  })
}))
gm_venue_roof <- unlist(sapply(sr_games_json$weeks, function(wk) {
  sapply(wk$games, function(gm) {
    gm$venue$roof_type
  })
}))
gm_venue_surface <- unlist(sapply(sr_games_json$weeks, function(wk) {
  sapply(wk$games, function(gm) {
    gm$venue$surface
  })
}))
gm_venue_long <- unlist(sapply(sr_games_json$weeks, function(wk) {
  sapply(wk$games, function(gm) {
    gm$venue$location$lng
  })
}))
gm_venue_lat <- unlist(sapply(sr_games_json$weeks, function(wk) {
  sapply(wk$games, function(gm) {
    gm$venue$location$lat
  })
}))
# Need to conditionally add weather based on roof type == outdoor
gm_weather <- unlist(sapply(sr_games_json$weeks, function(wk) {
  sapply(wk$games, function(gm) {
    if_else(gm$weather %>% is.null() == TRUE,'None',gm$weather)
  })
}))

sr_games_json_df <- tibble(
  all_id,
  gm_status,
  gm_scheduled,
  gm_home,
  gm_away,
  gm_venue,
  gm_venue_roof,
  gm_venue_surface,
  # gm_venue_long,
  # gm_venue_lat,
  gm_weather,
  NULL
) %>% 
  mutate(
    gm_home = ifelse(gm_home == 'JAC', 'JAX', gm_home),
    gm_away = ifelse(gm_away  == 'JAC', 'JAX', gm_away),
    gm_scheduled = gsub('T', ' ', substr(gm_scheduled, 1, 19)) %>%
      lubridate::ymd_hms(tz = 'GMT') %>%
      lubridate::with_tz(tzone = 'America/New_York'),
    gm_date = gm_scheduled %>%
      as.character() %>%
      substr(1, 10) %>%
      as.Date()
  ) %>% 
  # Join nflfastR game_id
  left_join(
    schedule_df %>%
      select(game_id, gameday, home_team) %>%
      mutate(gameday = gameday %>% as.Date()),
    by = c('gm_date' = 'gameday', 'gm_home' = 'home_team')
  ) %>% 
  relocate(game_id) %>% 
  select(
    game_id,
    sr_game_id = all_id,
    gm_status,
    gm_scheduled,
    gm_date,
    gm_home,
    gm_away,
    gm_venue,
    gm_venue_roof,
    gm_venue_surface,
    # gm_venue_long,
    # gm_venue_lat,
    gm_weather,
    NULL
    )

sr_games_json_df %>% 
  saveRDS(glue('data/schedules/sportradar/games_{year}.rds'))

sr_games_json_df <- readRDS(glue('data/schedules/sportradar/games_{year}.rds'))

# Check missing play by play files
gm_done <- gsub('.json','',dir(glue('data/pbp/sportradar/{year}')))
loop_id <- sr_games_json_df %>% 
  filter(gm_status=='closed' & !(sr_game_id %in% gm_done)) %>% 
  pull(sr_game_id)

loop_id
# Download missing play by play files
lapply(loop_id, function(x){
  gm_json <- jsonlite::fromJSON(url(glue('https://api.sportradar.us/nfl/official/trial/v5/en/games/{x}/pbp.json?api_key={sr_key}')))
  jsonlite::write_json(gm_json, glue('data/pbp/sportradar/{year}/{x}.json'))
  Sys.sleep(3)
  })

# Check missing participation files
# Participation is updated weekly on Fridays
gm_done <- gsub('.json','',dir(glue('data/part/{year}')))
loop_id <- sr_games_json_df %>% 
  filter(gm_status=='closed' & !(sr_game_id %in% gm_done) & (Sys.Date() - gm_date >= 5)) %>% 
  pull(sr_game_id)

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
