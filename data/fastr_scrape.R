library(tidyverse)
library(parallel)
library(viridis)

current_season <- year

con <- fx.db_con(x.host = 'localhost')

players_df <- nflreadr::load_players()
dbWriteTable(conn = con, 'nflfastR_players', players_df, overwrite = T)

roster_df <-  nflfastR::fast_scraper_roster(1999:year)
dbWriteTable(conn = con, 'nflfastR_rosters', roster_df, overwrite = T)

contracts_df <- nflreadr::load_contracts()
# dbWriteTable(conn = con, 'nflfastR_contracts', contracts_df)

officials_df <- nflreadr::load_officials()
dbWriteTable(conn = con, 'nflfastR_officials', officials_df, overwrite = T)

participation_df <- load_participation(seasons = 2016:current_season)
dbWriteTable(conn = con, 'nflfastR_participation', participation_df, overwrite = T)

schedule_df <- nflfastR::fast_scraper_schedules(1999:year)
dbWriteTable(con, 'nflfastR_schedule', schedule_df, overwrite = T)

trades_df <- nflreadr::load_trades()
dbWriteTable(con, 'nflfastR_trades', trades_df, overwrite = T)

draft_df <- nflreadr::load_draft_picks()
dbWriteTable(con, 'nflfastR_draft', draft_df, overwrite = T)

pbp <- tbl(con, 'nflfastR_pbp') %>% 
  filter(season >= 2006) %>% 
  collect()

team_stats_df <- pbp %>% 
  calculate_team_stats_mod(weekly = TRUE)

dbWriteTable(con, 'nflfastR_team_stats', team_stats_df)

player_stats_df <- pbp %>% 
  calculate_player_stats_mod(weekly = TRUE)

dbWriteTable(con, 'nflfastR_player_stats', player_stats_df)

existing_game_ids <- tbl(con, 'nflfastR_pbp') %>% 
  filter(season == current_season) %>% 
  select(game_id) %>% 
  collect() %>% 
  unique() %>% 
  pull(game_id)

# Scrape new games
new_scrape_df <- schedule_df %>% 
  filter(!(game_id %in% (tbl(con, 'nflfastR_pbp') %>% 
             filter(season == current_season) %>% 
             select(game_id) %>% 
             collect() %>% 
             unique() %>% 
             pull(game_id))) &
           gameday <= Sys.Date() &
           game_id != '2020_12_NO_DEN') %>% 
  pull(game_id) %>% 
  fast_scraper() %>% 
  clean_pbp() %>% 
  add_qb_epa() %>% 
  add_xyac() %>% 
  mutate(xyac_median_yardage = xyac_median_yardage %>% as.double())

new_scrape_ids <- new_scrape_df %>% 
  pull(game_id) %>% 
  unique()

new_scrape_ids

# Make sure the existing pbp dataframe doesn't have Sportradar columns
pbp_df %>% 
  select_if(!names(.) %in% names(new_scrape_df)) %>% 
  names()

# Bind the new PBP scrape
pbp_df <- rbind(tbl(con, 'nflfastR_pbp') %>% 
                  filter(season == current_season) %>% 
                  collect() %>% 
                  select(-year) %>%
                  filter(!(game_id %in% new_scrape_ids)), 
                new_scrape_df
                )

pbp_df %>% 
  saveRDS(glue('data/pbp/play_by_play_{current_season}.rds'))
# pbp_df %>% 
#   write_parquet(glue('data/pbp/fastr/{year}/pbp_{current_season}.parquet'))


# Add XYAC ----------------------------------------------------------------
# source('https://github.com/mrcaseb/nflfastR/blob/master/R/utils.R?raw=true')
# source('https://github.com/mrcaseb/nflfastR/raw/master/R/helper_add_xyac.R')
# source('https://github.com/mrcaseb/nflfastR/raw/master/R/helper_add_nflscrapr_mutations.R')
# source('fantasy_football/xyac/add_xyac_old.R')

# YAC Distribution Function -----------------------------------------------

# Duplicate the add_xyac() function that we sourced above
add_xyac_dist <- add_xyac


# Separate each block of code in the add_xyac_dist() function into blocks
add_xyac_blocks <- body(add_xyac_dist) %>% as.list

# Remove lines 51 to 62 from the 5th item in the list
add_xyac_blocks[[2]] <- add_xyac_blocks[[2]] %>% 
  format %>% 
  .[-(61:72)] %>% 
  paste(collapse = '\n') %>% 
  str2lang

# Replace the body of add_xyac_dist() with the new edited function
body(add_xyac_dist) <- add_xyac_blocks %>% as.call

# Get all PBP game IDs
pbp_game_ids <- pbp_df %>% 
  pull(game_id) %>% 
  unique()

# Read XYAC dataframe
pbp_xyac_df <- readRDS(glue('data/pbp/xyac_play_by_play_{current_season}.rds'))

# Get all XYAC game IDs
xyac_game_ids <- pbp_xyac_df %>% 
  pull(game_id) %>% 
  unique()

# List IDs that haven't been added to XYAC yet
new_pbp_game_ids <- pbp_game_ids[which(!pbp_game_ids %in% xyac_game_ids)]

new_pbp_game_ids

# Add XYAC to new PBP and bind the XYAC w/o new IDs
pbp_xyac_df <- rbind(pbp_xyac_df %>% 
                       filter(!(game_id %in% new_pbp_game_ids)), 
                     pbp_df %>% 
                       filter(game_id %in% new_pbp_game_ids) %>% 
                       add_xyac_dist)

pbp_xyac_df %>% 
  saveRDS(glue('data/pbp/xyac_play_by_play_{current_season}.rds'))
pbp_xyac_df %>% 
  write_parquet(glue('data/pbp/xyac/{year}/xyac_pbp_{current_season}.parquet'))

rm(
  new_scrape_df,
  new_scrape_ids,
  add_xyac_dist,
  add_xyac_blocks,
  pbp_game_ids,
  xyac_game_ids,
  new_pbp_game_ids,
  pbp_xyac_df
)

# -------------------------------------------------------------------------

# to remove sportradar columns
# pbp_xyac_df_19 <- readRDS(glue('data/pbp/xyac_play_by_play_2019.rds'))
# pbp_xyac_df <- pbp_xyac_df %>% 
#   select(pbp_xyac_df_19 %>% 
#            names()
#          )
