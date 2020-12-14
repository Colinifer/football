new_scrape <- schedule_df %>% 
  filter(gameday == Sys.Date()) %>% 
  pull(game_id) %>% 
  fast_scraper(pp = TRUE) %>% 
  clean_pbp() %>% 
  add_qb_epa() %>% 
  add_xyac()

new_game_ids <- new_scrape %>% 
  pull(game_id) %>% 
  unique()

pbp_df %>% 
  select_if(!names(.) %in% names(new_scrape)) %>% 
  names()

pbp_df <- rbind(pbp_df %>% filter(!(game_id %in% new_game_ids)), new_scrape)

pbp_df %>% 
  saveRDS(glue('data/pbp/play_by_play_{year}.rds'))

# rm(new_scrape, new_game_ids)