library(tidyverse)

# source('init.R')

source('https://raw.githubusercontent.com/mrcaseb/nflfastR/master/R/utils.R')
source('https://github.com/mrcaseb/nflfastR/raw/master/R/helper_add_xyac.R')
source('https://github.com/mrcaseb/nflfastR/raw/master/R/helper_add_nflscrapr_mutations.R')
source('fantasy_football/xyac/add_xyac_old.R')

# YAC Distribution Function -----------------------------------------------

# duplicate the add_xyac() function that we sourced above
add_xyac_dist <- add_xyac

# separate each block of code in the add_xyac_dist() function into blocks
add_xyac_blocks <- body(add_xyac_dist) %>% as.list

# we want to remove lines 51 to 62 from the 5th item in the list
add_xyac_blocks[[2]] <- add_xyac_blocks[[2]] %>% 
  format %>% 
  .[-(61:72)] %>% 
  paste(collapse = '\n') %>% 
  str2lang

# replace the body of add_xyac_dist() with our new edited function
body(add_xyac_dist) <- add_xyac_blocks %>% as.call

season <- year

# lapply(2006:2020, function(season){
# Load xyac dataframe
xyac_pbp_df <- readRDS(glue('data/pbp/xyac_play_by_play_{season}.rds'))

# Take existing game IDs from xyac dataframe
existing_xyac_ids <- xyac_pbp_df %>% 
  arrange(game_date) %>% 
  pull(game_id) %>% 
  unique()

# Add xyac to new game IDs and bind with existing xyac dataframe
xyac_pbp_df <-
  readRDS(glue('data/pbp/play_by_play_{season}.rds')) %>% 
  decode_player_ids(fast = T) %>% 
  filter(!(game_id %in% existing_xyac_ids)) %>% 
  add_xyac_dist %>% 
  rbind(xyac_pbp_df)

# Save new xyac dataframe
saveRDS(xyac_pbp_df, glue('data/pbp/xyac_play_by_play_{season}.rds'))

# })