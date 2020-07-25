games_df <- read.csv('http://nflgamedata.com/games.csv', stringsAsFactors = F)
#rosters.rds to check match rate
roster_df <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/roster-data/roster.rds'))

#get all the column names with player_id & player_name
sample_df <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/legacy-data/play_by_play_2019.rds'))
player_id_cols <- names(sample_df)[grep('_player_id',names(sample_df))]
player_name_cols <- names(sample_df)[grep('_player_name',names(sample_df))]

#go through 2011 to 2019 pbp data
full_player_map <- do.call(rbind, lapply(2011:2019, function(yr) {
  #join legacy pbp with new pbp on game_id, play_id and every player_name field at once (actually seems to work!)
  #a player could be missed if their name abbreviation changed from 1.0 to 2.0
  year_joined_df <- readRDS(url(paste0('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_',yr,'.rds'))) %>% 
    left_join(games_df, by = c('game_id')) %>% 
    inner_join(readRDS(url(paste0('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/legacy-data/play_by_play_',yr,'.rds'))), by = c('gsis'='game_key', 'play_id', player_name_cols), suffix = c('_new','_legacy'))
  
  #look at every ID col and take each IDs and gsisID combination
  do.call(rbind, lapply(player_id_cols, function(x) {
    year_joined_df %>%
      select(
        'ID' = paste0(x, '_new'),
        'gsis' = paste0(x, '_legacy')
      )
  })) %>%
    filter(!is.na(ID) & !is.na(gsis)) %>%
    distinct() %>% 
    return()
})) %>% 
  distinct()

saveRDS(full_player_map, 'init_gsis_map.rds')

#check for gsis or IDs that were matched more than once
table(table(full_player_map$ID))
table(table(full_player_map$gsis))

#join to roster file for players from this era to check match rate
roster_check <- roster_df %>% 
  filter(team.season >= 2011) %>% 
  left_join(full_player_map, by = c('teamPlayers.gsisId' = 'gsis'))

#see how many players we were able to match, I got 75%
table(is.na(roster_check$ID))/nrow(roster_check)