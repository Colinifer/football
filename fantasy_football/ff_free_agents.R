player_df <- roster_df %>% 
  filter(
    season == year & 
      position %in% c('QB', 'RB', 'WR', 'TE', 'K')
  ) %>% 
  select(
    team,
    position,
    full_name,
    first_name,
    last_name,
    years_exp,
    gsis_id,
    espn_id,
    pff_id,
    sportradar_id,
    headshot_url
  ) %>% 
  left_join(
    ff_rosters_beep_boop %>% 
      select(
        player_id,
        on_roster
      ),
    by = c('espn_id' = 'player_id')
  ) %>% 
  mutate(
    on_roster = case_when(is.na(on_roster) ~ FALSE,
                          TRUE ~ on_roster)
  ) %>% 
  left_join(
    player_stats %>% 
      select()
  )
