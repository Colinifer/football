library(RJSONIO)

yr <- 2020
part_json_file_names <- dir(glue('data/part/{year}'), full.names = T)

player_infos <- c('id','name','jersey','reference','position','sr_id')

part_szn_df <- data.frame(do.call(rbind, unlist(lapply(part_json_file_names, function(file_name) {
  part_json <- RJSONIO::fromJSON(file_name)
  #part_json <- RJSONIO::fromJSON(part_json_file_names[67])
  #each_play <-
  lapply(part_json$plays, function(ply) {
    if(length(ply$away$players)==0) {
      return(NULL)
    } else {
      cbind(
        'game_id_SR' = part_json$id,
        'play_id' = ply$reference,
        'play_id_SR' = ply$id,
        rbind(
            cbind('team' = ply$away$alias, t(sapply(ply$away$players, function(x) unlist(x)[player_infos]))),
            cbind('team' = ply$home$alias, t(sapply(ply$home$players, function(x) unlist(x)[player_infos])))
        )
      )
    }
  })
}), recursive = F)), stringsAsFactors = F)

saveRDS(part_szn_df, glue('data/part/sportradar_part_{year}.rds'))




# 
# lapply(part_json$plays
# 
# rev(sort(table(part_szn_df$position)))
# pos_ord <- c('K', 'P', 'LS', 'QB', 'RB', 'FB', 'TE', 'WR', 'T', 'G', 'C', 'OL', 'DT', 'DL', 'DE', 'OLB', 'LB', 'ILB', 'LB/S', 'DB', 'S', 'CB')
# 
# part_szn_df %>% 
#   group_by(id) %>% 
#   mutate(tot_n = n()) %>% 
#   group_by(id, position) %>% 
#   summarise(
#     port = n()/mean(tot_n),
#     all_snaps = mean(tot_n)
#   ) %>% 
#   filter(port < 1)
# 
# part_szn_df %>% 
#   filter(id == '010806af-e6ba-409a-b7fb-a119714238f6')
# 
# part_szn_df %>% 
#   mutate(position = factor(position, pos_ord)) %>% 
#   group_by(team, play_id, position) %>% 
#   summarize(n = n()) %>% 
#   mutate(pos_cnt = paste0(n, ' ', position)) %>% 
#   group_by(team, play_id) %>% 
#   mutate(per_grp = paste(pos_cnt, collapse = ' | ')) %>% 
#   select(team, play_id, per_grp) %>% 
#   distinct %>% 
#   group_by(team, per_grp) %>% 
#   summarize(n = n()) %>%
#   arrange(-n)
# 

