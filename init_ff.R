# https://ffscrapr.ffverse.com/reference/index.html
year

ff_conn_beep_boop <- ffscrapr::espn_connect(season = year,
                       league_id = initR::fantasy_key |> pull(league_id) |> nth(1),
                       swid = swid, 
                       espn_s2 = espn_s2)

ff_conn_drinkers <- ffscrapr::espn_connect(season = year,
                                  league_id = initR::fantasy_key |> pull(league_id) |> nth(2),
                                  swid = swid, 
                                  espn_s2 = espn_s2)

ff_conn_family <- ffscrapr::espn_connect(season = year,
                                         league_id = initR::fantasy_key |> pull(league_id) |> nth(3),
                                         swid = swid, 
                                         espn_s2 = espn_s2)

ff_conn_kepler <- ffscrapr::espn_connect(season = year,
                       league_id = fantasy_key |> pull(league_id) |> nth(4),
                       swid = swid, 
                       espn_s2 = espn_s2)


fx.ff_free_agents <- function(player_stats_df = player_stats, league_name = 'Drinkers'){
  ff_team_name <- tibble(
    league = c('Beep Boop', 'Drinkers', 'Kepler', 'Family'),
    team = c('Love Daktually', 'Finding Deebo', 'Team Welsh', 'Hop Skip and a W')
  ) |> 
    filter(league == league_name) |> 
    pull(team)
  
  df <- player_stats_df |>
    left_join(
      fantasy_rosters |> 
        filter(league == league_name & 
                 pos %in% c('RB', 'WR', 'TE')) |> 
        select(on_roster, franchise_name, gsis_id),
      by = c('player_id' = 'gsis_id')
    ) |> 
    left_join(
      roster_df |> 
        select(
          season,
          position,
          gsis_id
        ),
      by = c('player_id' = 'gsis_id')
    ) |> 
    left_join(
      teams_colors_logos |> 
        select(
          team_abbr,
          team_logo_wikipedia,
          team_color,
          team_color2
        ),
      by = c('recent_team' = 'team_abbr')
    ) |> 
    filter(position %in% c('RB', 'WR', 'TE')) |> 
    mutate(
      on_roster = case_when(is.na(on_roster) ~ FALSE,
                            TRUE ~ on_roster)
    ) |> 
    filter(
      on_roster == FALSE # | franchise_name == ff_team_name
    )
  
  return(df)
}

# ff_draft(ff_conn_beep_boop) %>% 
#   saveRDS(glue('fantasy_football/data/draft/{year}/beep_boop_draft.rds'))
# ff_draft(ff_conn_drinkers) %>% 
#   saveRDS(glue('fantasy_football/data/draft/{year}/drinkers_draft.rds'))
# ff_draft(ff_conn_family) %>% 
#   saveRDS(glue('fantasy_football/data/draft/{year}/family_draft.rds'))
# ff_draft(ff_conn_kepler) %>% 
#   saveRDS(glue('fantasy_football/data/draft/{year}/kepler_draft.rds'))

# ff_franchises(ff_conn_beep_boop)
# ff_league(ff_conn_beep_boop)
# ff_playerscores(ff_conn_beep_boop)

# 
# # Schedules
# ff_schedule_beep_boop <- ff_schedule(ff_conn_beep_boop)
# ff_schedule_drinkers <- ff_schedule(ff_conn_drinkers)
# ff_schedule_family <- ff_schedule(ff_conn_family)
# ff_schedule_kepler <- ff_schedule(ff_conn_kepler)
# 
# ff_scoring(ff_conn)
# ff_scoringhistory(ff_conn)
# ff_standings(ff_conn)
# ff_starter_positions(ff_conn)
# ff_starters(ff_conn)
# ff_transactions(ff_conn)
# 
# nflfastR::fast_scraper_roster(year)
# espn_players(ff_conn)
# )
