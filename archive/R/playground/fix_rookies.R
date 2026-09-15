# https://pastebin.com/bndRdW8s

fix_rookies <- function(pbp) {
  roster_fix <- nflreadr::load_rosters(seasons = 2022) %>% 
    mutate(player_abbr = paste0(substr(first_name, 1,1), ".",last_name)) %>% 
    filter(!is.na(gsis_id)) %>%
    filter(position %in% c("QB", "WR", "TE", "RB")) %>% 
    select(full_name, team, gsis_id, player_abbr) %>% 
    unique()
  
  old_season <- pbp_df |> filter(season < 2022)
  
  fixed_recent_season <- pbp |> filter(season == 2022) %>% 
    mutate(rec_first_initial = substr(sub("\\..*", "", receiver_player_name), 1, 1),
           rec_last_name = sub('.*\\.', '', receiver_player_name),
           rush_first_initial = substr(sub("\\..*", "", rusher_player_name), 1, 1),
           rush_last_name = sub('.*\\.', '', rusher_player_name),
           fantasy_first_initial = substr(sub("\\..*", "", fantasy_player_name), 1, 1),
           fantasy_last_name = sub('.*\\.', '', fantasy_player_name),
           receiver_player_name = paste0(rec_first_initial, ".", rec_last_name),
           rusher_player_name = paste0(rush_first_initial, ".", rush_last_name),
           fantasy_player_name = paste0(fantasy_first_initial, ".", fantasy_last_name)) %>% 
    left_join(roster_fix,
              by = c("receiver_player_name" = "player_abbr",
                     "posteam" = "team")) %>% 
    mutate(receiver_player_id = if_else(is.na(receiver_player_id), gsis_id, receiver_player_id)) %>% 
    select(-c(rec_first_initial, rec_last_name, full_name, gsis_id)) %>% 
    left_join(roster_fix,
              by = c("rusher_player_name" = "player_abbr",
                     "posteam" = "team")) %>% 
    mutate(rusher_player_id = if_else(is.na(rusher_player_id), gsis_id, rusher_player_id)) %>% 
    select(-c(rush_first_initial, rush_last_name, full_name, gsis_id)) %>% 
    left_join(roster_fix,
              by = c("fantasy_player_name" = "player_abbr",
                     "posteam" = "team")) %>% 
    mutate(fantasy_player_id = if_else(is.na(fantasy_player_id), gsis_id, fantasy_player_id)) %>% 
    select(-c(fantasy_first_initial, fantasy_last_name, full_name, gsis_id))
  
  return(
    rbind(old_season, fixed_recent_season) |> 
      arrange(old_game_id, play_id)
    )
}
