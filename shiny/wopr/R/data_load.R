# Create variables & dataframes -------------------------------------------

# nflfastR data
con <- fx.db_con(x.host = 'localhost')
# update_roster_db(season = year, db_connection = fx.db_con(x.host = 'localhost'))
roster_df <- tbl(con, 'nflfastR_rosters') %>% 
  filter(season == year) %>%
  collect()
# update_schedule_db(season = year, db_connection = fx.db_con(x.host = 'localhost'))
schedule_df <- tbl(con, 'nflfastR_schedule') %>% 
  filter(season == year) %>% 
  collect()
# update_trades_db(season = year, db_connection = fx.db_con(x.host = 'localhost'))
trades_df <- tbl(con, 'nflfastR_trades') %>% 
  filter(season == year) %>% 
  collect()
# update_draft_db(season = year, db_connection = fx.db_con(x.host = 'localhost'))
draft_df <- tbl(con, 'nflfastR_draft') %>% 
  filter(season == year) %>% 
  collect()

pbp_df <- tbl(con, 'nflfastR_pbp') %>% 
  filter(season == year) %>% 
  collect()
dbDisconnect(con)


source('init_ff.R')
# Rosters
fantasy_rosters <- ff_rosters(ff_conn_beep_boop) %>%
  mutate(on_roster = TRUE,
         league = 'Beep Boop') %>%
  rbind(ff_rosters(ff_conn_drinkers) %>%
          mutate(on_roster = TRUE,
                 league = 'Drinkers')) %>%
  rbind(ff_rosters(ff_conn_kepler) %>%
          mutate(on_roster = TRUE,
                 league = 'Kepler')) %>%
  rbind(ff_rosters(ff_conn_family) %>%
          mutate(on_roster = TRUE,
                 league = 'Family')) %>% 
  left_join(roster_df %>% 
              select(
                gsis_id,
                espn_id),
            by = c('player_id' = 'espn_id'))

player_stats <- pbp_df %>% 
  calculate_player_stats_mod() 

ff_free_agents <- fx.ff_free_agents(player_stats, 'Beep Boop')

player_stats_weekly <- pbp_df %>% 
  calculate_player_stats_mod(weekly = TRUE)

u.players <- player_stats_weekly %>% 
  select(player_id, player_name, recent_team) %>% 
  unique() %>% 
  mutate(
    full_player_info = paste(recent_team, player_name)
  ) %>% 
  arrange(full_player_info) %>% 
  pull(full_player_info)