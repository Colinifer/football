library(RJSONIO)

current_season <- year

# lapply(2017:2020, function(current_season){
part_json_file_names <- dir(glue('data/part/{current_season}'), full.names = T)
# part_json_file_names <- "data/part/2017/46b96b1c-1df5-4c18-8999-36b6d39be24f.json"

player_infos <- c('id','name','jersey','reference','position','sr_id')

part_df <- as_tibble(do.call(rbind, unlist(lapply(part_json_file_names, function(file_name) {
  # "data/part/2017/470ae953-4a2e-4814-8a18-1b50f9236a26.json" broken
  print(file_name)
  # print(file_name)
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
        'play_type_SR'= ply$type,
        rbind(
          cbind('team' = ply$away$alias, t(sapply(ply$away$players, function(x) unlist(x)[player_infos]))),
          cbind('team' = ply$home$alias, t(sapply(ply$home$players, function(x) unlist(x)[player_infos])))
        )
      )
    }
  })
}), recursive = F)))

part_df %>% 
  saveRDS(glue('data/part/sportradar_part_{current_season}.rds'))
part_df %>% 
  write_parquet(glue('data/part/sportradar/{current_season}/sportradar_part_{current_season}.parquet'))
# })

rm(
  part_json_file_names,
  part_szn_df
)


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

