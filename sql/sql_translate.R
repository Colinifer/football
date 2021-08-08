# lazy_frame() %>% 
tbl(con, 'nflfastR_pbp') %>%
  filter(season == current_season & 
           pass_attempt==1 & 
           season_type=='REG' 
         & two_point_attempt==0 
         & !is.na(receiver_id)
  ) %>% 
  select(season, 
         game_id, 
         play_id, 
         posteam,
         defteam,
         receiver, 
         receiver_player_id, 
         receiver_id, 
         air_yards, 
         yards_gained, 
         complete_pass,
         pass_location,
         cp,
         pass_touchdown) %>% 
  remote_query()
  