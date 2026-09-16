
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

player_stats %>%
  left_join(
    fantasy_rosters %>% 
      filter(league == 'Drinkers' & 
               pos %in% c('RB', 'WR', 'TE')) %>% 
      select(on_roster, franchise_name, gsis_id),
    by = c('player_id' = 'gsis_id')
  ) %>% 
  left_join(
    roster_df %>% 
      select(
        season,
        position,
        gsis_id
      ),
    by = c('player_id' = 'gsis_id', 'season', 'position')
  ) %>% 
  filter(position %in% c('RB', 'WR', 'TE')) %>% 
  select(
    season, on_roster, franchise_name, recent_team:player_name, position, offense_snaps:offense_pct, contains('rushing'), hvt, receptions, targets, contains('receiving'), racr, target_share, air_yards_share, wopr, contains('fantasy')
  ) %>% 
  filter(is.na(on_roster) | (franchise_name %in% c('Matt Rhules', 'Golden Rhule', 'Rhule Tide', 'Team Welsh'))) %>% 
  arrange(-hvt)