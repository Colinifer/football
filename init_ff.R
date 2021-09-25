# https://ffscrapr.ffverse.com/reference/index.html
year

ff_conn_beep_boop <- ffscrapr::espn_connect(season = year,
                       league_id = initR::fantasy_key %>% pull(league_id) %>% nth(1),
                       swid = swid, 
                       espn_s2 = espn_s2)

ff_conn_drinkers <- ffscrapr::espn_connect(season = year,
                                  league_id = initR::fantasy_key %>% pull(league_id) %>% nth(2),
                                  swid = swid, 
                                  espn_s2 = espn_s2)

ff_conn_family <- ffscrapr::espn_connect(season = year,
                                         league_id = initR::fantasy_key %>% pull(league_id) %>% nth(3),
                                         swid = swid, 
                                         espn_s2 = espn_s2)

ff_conn_kepler <- ffscrapr::espn_connect(season = year,
                       league_id = fantasy_key %>% pull(league_id) %>% nth(4),
                       swid = swid, 
                       espn_s2 = espn_s2)

# ff_draft(ff_conn_beep_boop) %>% 
#   saveRDS(glue('fantasy_football/data/draft/{year}/beep_boop_draft.rds'))
# ff_draft(ff_conn_drinkers) %>% 
#   saveRDS(glue('fantasy_football/data/draft/{year}/drinkers_draft.rds'))
# ff_draft(ff_conn_family) %>% 
#   saveRDS(glue('fantasy_football/data/draft/{year}/family_draft.rds'))
# ff_draft(ff_conn_kepler) %>% 
#   saveRDS(glue('fantasy_football/data/draft/{year}/kepler_draft.rds'))

ff_franchises(ff_conn_beep_boop)

ff_league(ff_conn_beep_boop)
ff_playerscores(ff_conn_beep_boop)

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
                 league = 'Family'))
  

# Schedules
ff_schedule_beep_boop <- ff_schedule(ff_conn_beep_boop)
ff_schedule_drinkers <- ff_schedule(ff_conn_drinkers)
ff_schedule_family <- ff_schedule(ff_conn_family)
ff_schedule_kepler <- ff_schedule(ff_conn_kepler)

ff_scoring(ff_conn)
ff_scoringhistory(ff_conn)
ff_standings(ff_conn)
ff_starter_positions(ff_conn)
ff_starters(ff_conn)
ff_transactions(ff_conn)

nflfastR::fast_scraper_roster(year)
espn_players(ff_conn)
# )
