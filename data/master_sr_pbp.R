library(RJSONIO)
library(tidyverse)

sr_games_df <- readRDS(glue('data/games_{year}.rds')) %>% 
  select(game_id, game_id_SR)

year <- 2020

### I store all my json files in the folder structure below. I save every file name to this vector and iterate through them
all_json <- c(dir(glue('data/{year}'), full=T),dir(glue('data/post/{year}'), full=T)) %>% .[which(grepl('.json',.))] 

### find every single "statistic" that is referenced in the SR pbp data
### this is just for reference
all_stat_names <- sort(unique(unlist(lapply(all_json, function(gm_file) {
  gm_json <- RJSONIO::fromJSON(gm_file)
  lapply(gm_json$periods, function(qtr) {
    lapply(qtr$pbp, function(drv) {
      lapply(drv$events, function(ply) {
        sapply(ply$statistics, function(x) {
          paste0(x$stat_type,'..',names(unlist(x)))
        })
      })
    })
  })
}))))

### find every play field that is referenced in the SR pbp data
### this is just for reference
play_level_vars <- sort(unique(unlist(lapply(all_json, function(gm_file) {
  gm_json <- RJSONIO::fromJSON(gm_file)
  lapply(gm_json$periods, function(qtr) {
    lapply(qtr$pbp, function(drv) {
      lapply(drv$events, function(ply) {
        names(unlist(ply))
      })
    })
  })
}))))

### these are the defensive stats that I'd like to pull
SR_cnt_stat <- c('player.reference','stat_type','category','def_target','missed_tackles','blitz','hurry','knockdown',
                  'onside_success','onside_attempt','squib_kick','batted_pass','incompletion_type','on_target_throw',
                  'pocket_time','hang_time','broken_tackles','catchable','yards_after_contact')

SR_stats <- data.frame(do.call(rbind, unlist(unlist(unlist(
  lapply(all_json, function(gm_file) {
    gm_json <- RJSONIO::fromJSON(gm_file)
    lapply(gm_json$periods, function(qtr) {
      lapply(qtr$pbp, function(drv) {
        lapply(drv$events, function(ply) {
          if(length(ply$statistics)==0) {
            return(NULL)
          } else {
            ply_stats <- t(sapply(ply$statistics, function(x) {
              stat_list <- unlist(x)[SR_cnt_stat]
              if(length(stat_list)==0) {stat_list <- rep(NA, length(stat_list))}
              names(stat_list) <- SR_cnt_stat
              return(stat_list)
            }))
            return(cbind('play_id_SR'=ply$id, ply_stats))
          }
        })
      })
    })
  })
, recursive = F), recursive = F), recursive = F)), stringsAsFactors = F)

### df with all the player-level stats that I want
single_stat_df <- SR_stats %>% 
  pivot_longer(-c(play_id_SR, player.reference, stat_type, category), names_to = 'stat_name', values_to = 'stat_value') %>% 
  mutate(full_stat_name = paste0(stat_type,'.',stat_name)) %>% 
  filter(!is.na(stat_value) & stat_type != 'defense') %>% 
  group_by(play_id_SR, full_stat_name) %>% 
  mutate(order = row_number()) %>% 
  ungroup %>% 
  filter(order == 1) %>% 
  pivot_wider(play_id_SR, names_from = full_stat_name, values_from = stat_value) %>% 
  select(order(names(.)))


### df with comma sep defensive players for each stat
def_player_df <- SR_stats %>% 
  pivot_longer(-c(play_id_SR, player.reference, stat_type, category), names_to = 'stat_name', values_to = 'stat_value') %>% 
  filter(stat_type == 'defense' & stat_value > 0) %>% 
  group_by(play_id_SR, stat_name) %>% 
  mutate(
    players = paste(player.reference, collapse = ','),
    player.reference = NULL,
    stat_value = NULL
  ) %>% 
  distinct() %>% 
  pivot_wider(play_id_SR, names_from = stat_name, values_from = players, names_prefix = 'defPlayers.') %>% 
  select(order(names(.)))

SR_play_stats <- c('sequence','reference','wall_clock','hash_mark','fake_field_goal','fake_punt','left_tightends','right_tightends',
                   'play_direction','running_lane','run_pass_option','screen_pass','men_in_box','blitz','pass_route',
                   'play_action','players_rushed','pocket_location')

play_info_df <- data.frame(do.call(rbind, unlist(unlist(unlist(
  lapply(all_json, function(gm_file) {
    gm_json <- RJSONIO::fromJSON(gm_file)
    lapply(gm_json$periods, function(qtr) {
      lapply(qtr$pbp, function(drv) {
        lapply(drv$events, function(ply) {
          play_info_vec <- unlist(ply)[SR_play_stats]
          names(play_info_vec) <- SR_play_stats
          return(c('game_id_SR'=gm_json$id, 'play_id_SR'=ply$id, play_info_vec))
        })
      })
    })
  })
, recursive = F), recursive = F), recursive = F)), stringsAsFactors = F)

play_info_df$reference <- as.numeric(play_info_df$reference)
play_info_df <- play_info_df %>% select(order(names(.)))

#play_info_df %>% 
#  filter(game_id_SR == '75dce1db-65cb-42a7-a8ed-7bf5e9142f69') %>% 
#  view

pbp_df <-
  purrr::map_df(season, function(x) {
    readRDS(url(glue::glue("https://github.com/guga31bb/nflfastR-data/blob/master/data/play_by_play_{x}.rds?raw=true")))
  }) %>% decode_player_ids(fast = TRUE)

full_pbp_df <- pbp_df %>% 
  left_join(sr_games_df, by = 'game_id') %>% 
  left_join(play_info_df, by = c('game_id_SR','play_id' = 'reference')) %>% 
  left_join(single_stat_df, by = 'play_id_SR') %>% 
  left_join(def_player_df, by = 'play_id_SR')

to_logical_cols <- c('fake_field_goal','fake_punt','run_pass_option','screen_pass','play_action', 'blitz')
for (i in to_logical_cols) full_pbp_df[,paste0(i)] <- as.logical(unlist(full_pbp_df[,paste0(i)]))

to_numeric_cols <- c(to_logical_cols, 'left_tightends','right_tightends','running_lane','men_in_box','players_rushed',
                     'kick.onside_success','kick.onside_attempt','kick.squib_kick','rush.broken_tackles','rush.yards_after_contact',
                     'pass.blitz','pass.hurry','pass.knockdown','pass.batted_pass','pass.on_target_throw','pass.pocket_time',
                     'receive.broken_tackles','receive.catchable','receive.yards_after_contact','punt.hang_time')
for (i in to_numeric_cols) full_pbp_df[,paste0(i)] <- as.numeric(unlist(full_pbp_df[,paste0(i)]))

saveRDS(full_pbp_df, glue('data/pbp/play_by_play_{year}.rds'))


# 
# full_pbp_df %>% 
#   pull(play_id_SR) %>% 
#   is.na %>% 
#   table
# 
# full_pbp_df %>%
#  filter(is.na(play_id_SR)) %>%
#  group_by(game_id) %>%
#  summarise(n = n()) %>%
#  arrange(-n)
#   



### see what all is available
#sapply(full_pbp_df[,329:364], function(x) table(factor(is.na(x), levels = c('TRUE','FALSE'))))

### try to get at correct sequence, reference doesn't always work

# table(is.na(full_pbp_df$play_id_SR))
# 
# full_pbp_df %>% 
#   filter(is.na(play_id_SR) & timeout==0 & extra_point_attempt==0 & two_point_attempt==0) %>%  
#   view
# 
# full_pbp_df %>% 
#   filter(is.na(play_id_SR)) %>% 
#   pull(game_id) %>% 
#   table %>% 
#   sort
# 
# SR_games_df %>% 
#   filter(game_id == '2019_01_BUF_NYJ')


# def_stats %>% 
#   filter(player.reference == '00-0026280') %>% 
#   mutate(play_id_SR = NULL) %>% 
#   group_by(player.reference) %>% 
#   summarise_all(sum, na.rm = T)
#   
# roster_df %>% 
#   filter(teamPlayers.lastName == 'Carr' & teamPlayers.position == 'CB' & team.season == 2019)

  
# 
# def_stats %>% 
#   group_by(id) %>% 
#   mutate(pl_list = paste(player.reference, collapse = ', ')) %>% 
#   group_by(id, pl_list) %>% 
#   summarize(
#     stat = sum(missed_tackles, na.rm = T)
#   ) %>% 
#   arrange(-stat)
# 



## browns v bucs
# missing run from scrim, seq & ref do not match
# maybe do a second join where I try to pick these up
# 2010_01_CLE_TB
#48a8cbaa-d785-47ee-95e5-2a63f0a3352f
#play id 1497

## jets v bills
#missing xp from pbp json parse
#xp appears in drive list for some reason
#2019_01_BUF_NYJ
#75dce1db-65cb-42a7-a8ed-7bf5e9142f69


# grep('75dce1db-65cb-42a7-a8ed-7bf5e9142f69',all_json)
# gm_json <- fromJSON(all_json[124])
# sapply(gm_json$periods[[1]]$pbp, function(x) x$type)
# 
# gm_json$periods[[1]]$pbp[[1]]$alt_description
