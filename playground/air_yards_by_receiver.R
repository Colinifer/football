defteam_rec <- pbp_df %>% 
  filter(pass_attempt==1 & season_type=='REG' & two_point_attempt==0 & !is.na(receiver_id)) %>% 
  select(season, 
         game_id, 
         play_id, 
         posteam,
         defteam,
         receiver, 
         receiver_player_id, 
         receiver_id, 
         air_yards, 
         yards_gained, 
         complete_pass,
         pass_location,
         cp,
         pass_touchdown) %>% 
  # filter(!is.na(receiver)) %>% 
  filter(defteam == 'LV') %>%
  group_by(receiver_player_id) %>% 
  mutate(
    target = 0,
    game_played = 0
  )  %>% 
  group_by(game_id, receiver) %>% 
  mutate(game_played = ifelse(row_number()==1,1,0)) %>% 
  ungroup %>% 
  group_by(game_id, play_id, receiver) %>% 
  mutate(target = ifelse(row_number()==1,1,0),
         pass_left = ifelse(pass_location=='left',1,0),
         pass_middle = ifelse(pass_location =='middle',1,0),
         pass_right = ifelse(pass_location =='right',1,0),
         complete_left = ifelse(complete_pass == 1 & pass_location=='left',1,0),
         complete_middle = ifelse(complete_pass == 1 & pass_location =='middle',1,0),
         complete_right = ifelse(complete_pass == 1 & pass_location =='right',1,0)
           ) %>% 
  ungroup %>% 
  group_by(posteam, receiver) %>% 
  summarize(
    receiver_id = unique(receiver_id),
    games = sum(game_played, na.rm = T),
    targets = sum(target, na.rm = T),
    targets_pg = targets / games,
    tot_rec_yards = sum(yards_gained, na.rm =TRUE),
    tot_air_yards = sum(air_yards, ra.rm = TRUE),
    air_yards_pg = tot_air_yards / games,
    racr = tot_rec_yards / tot_air_yards,
    receptions = sum(complete_pass),
    adot = tot_air_yards / targets,
    td = sum(pass_touchdown, ra.rm = TRUE),
    td_pg = td / games,
    target_l = sum(pass_left),
    target_m = sum(pass_middle),
    target_r = sum(pass_right),
    target_l_perct = target_l / targets,
    target_m_perct = target_m / targets,
    target_r_perct = target_r / targets,
    rec_l = sum(complete_left),
    rec_m = sum(complete_middle),
    rec_r = sum(complete_right),
    rec_l_perct = rec_l / targets,
    rec_m_perct = rec_m / targets,
    rec_r_perct = rec_r / targets
  ) %>% 
  left_join(sleeper_players_df %>% 
              select(gsis_id, height, headshot_url),
            by = c('receiver_id' = 'gsis_id')
  ) %>% 
  ungroup() %>% 
  arrange(air_yards_pg %>% 
            desc()
  )

defteam_rec %>% 
  filter(tot_air_yards > 149) %>% 
  summarize(
    rec_l_perct = mean(rec_l_perct),
    rec_m_perct = mean(rec_m_perct),
    rec_r_perct = mean(rec_r_perct),
    height = mean(height)
  )

slice(1:20) %>% 
  ggplot(
    aes(x = receiver, y = tot_air_yards)) +
  geom_bar(
    aes(x = reorder(receiver, -tot_air_yards), 
        y = tot_air_yards), 
    stat='identity') +
  geom_image(
    aes(image = headshot_url), 
    asp = 16/9,
    nudge_y = 1
  ) +
  theme_cw +
  theme(
    axis.text.x = element_text(angle = 45)
  )


posteam_rec <- pbp_df %>% 
  filter(pass_attempt==1 & season_type=='REG' & two_point_attempt==0 & !is.na(receiver_id)) %>% 
  select(season, 
         game_id, 
         play_id, 
         posteam,
         defteam,
         receiver, 
         receiver_player_id, 
         receiver_id, 
         air_yards, 
         yards_gained, 
         complete_pass,
         pass_location,
         cp,
         pass_touchdown) %>% 
  # filter(!is.na(receiver)) %>% 
  filter(posteam == 'LAC') %>%
  group_by(receiver_player_id) %>% 
  mutate(
    target = 0,
    game_played = 0
  )  %>% 
  group_by(game_id, receiver) %>% 
  mutate(game_played = ifelse(row_number()==1,1,0)) %>% 
  ungroup %>% 
  group_by(game_id, play_id, receiver) %>% 
  mutate(target = ifelse(row_number()==1,1,0),
         pass_left = ifelse(pass_location=='left',1,0),
         pass_middle = ifelse(pass_location =='middle',1,0),
         pass_right = ifelse(pass_location =='right',1,0),
         complete_left = ifelse(complete_pass == 1 & pass_location=='left',1,0),
         complete_middle = ifelse(complete_pass == 1 & pass_location =='middle',1,0),
         complete_right = ifelse(complete_pass == 1 & pass_location =='right',1,0)
  ) %>% 
  ungroup %>% 
  group_by(posteam, receiver) %>% 
  summarize(
    receiver_id = unique(receiver_id),
    games = sum(game_played, na.rm = T),
    targets = sum(target, na.rm = T),
    targets_pg = targets / games,
    tot_rec_yards = sum(yards_gained, na.rm =TRUE),
    tot_air_yards = sum(air_yards, ra.rm = TRUE),
    air_yards_pg = tot_air_yards / games,
    racr = tot_rec_yards / tot_air_yards,
    receptions = sum(complete_pass),
    adot = tot_air_yards / targets,
    td = sum(pass_touchdown, ra.rm = TRUE),
    td_pg = td / games,
    target_l = sum(pass_left),
    target_m = sum(pass_middle),
    target_r = sum(pass_right),
    target_l_perct = target_l / targets,
    target_m_perct = target_m / targets,
    target_r_perct = target_r / targets,
    rec_l = sum(complete_left),
    rec_m = sum(complete_middle),
    rec_r = sum(complete_right),
    rec_l_perct = rec_l / targets,
    rec_m_perct = rec_m / targets,
    rec_r_perct = rec_r / targets
  ) %>% 
  left_join(sleeper_players_df %>% 
              select(gsis_id, height, headshot_url),
            by = c('receiver_id' = 'gsis_id')
  ) %>% 
  ungroup() %>% 
  arrange(air_yards_pg %>% 
            desc()
  )
  slice(1:20) %>% 
  ggplot(
    aes(x = receiver, y = tot_air_yards)) +
  geom_bar(
    aes(x = reorder(receiver, -tot_air_yards), 
        y = tot_air_yards), 
    stat='identity') +
  geom_image(
    aes(image = headshot_url), 
    asp = 16/9,
    nudge_y = 1
  ) +
  theme_cw +
  theme(
    axis.text.x = element_text(angle = 45)
  )



team_passing <- pbp_df %>% 
  filter(pass_attempt==1 & season_type=='REG' & two_point_attempt==0 & !is.na(receiver_id)) %>% 
  select(season, 
         game_id, 
         play_id, 
         posteam,
         defteam,
         air_yards, 
         yards_gained, 
         complete_pass,
         pass_location,
         cp,
         pass_touchdown) %>% 
  group_by(posteam) %>% 
  mutate(
    target = 0,
    game_played = 0
  )  %>% 
  group_by(game_id, posteam) %>% 
  mutate(game_played = ifelse(row_number()==1,1,0)) %>% 
  ungroup %>% 
  group_by(game_id, play_id, posteam) %>% 
  mutate(target = ifelse(row_number()==1,1,0),
         pass_left = ifelse(pass_location=='left',1,0),
         pass_middle = ifelse(pass_location =='middle',1,0),
         pass_right = ifelse(pass_location =='right',1,0),
         complete_left = ifelse(complete_pass == 1 & pass_location=='left',1,0),
         complete_middle = ifelse(complete_pass == 1 & pass_location =='middle',1,0),
         complete_right = ifelse(complete_pass == 1 & pass_location =='right',1,0)
  ) %>% 
  ungroup %>% 
  group_by(posteam) %>% 
  summarize(
    games = sum(game_played, na.rm = T),
    targets = sum(target, na.rm = T),
    targets_pg = targets / games,
    tot_rec_yards = sum(yards_gained, na.rm =TRUE),
    tot_air_yards = sum(air_yards, ra.rm = TRUE),
    air_yards_pg = tot_air_yards / games,
    racr = tot_rec_yards / tot_air_yards,
    receptions = sum(complete_pass),
    adot = tot_air_yards / targets,
    td = sum(pass_touchdown, ra.rm = TRUE),
    td_pg = td / games,
    target_l = sum(pass_left),
    target_m = sum(pass_middle),
    target_r = sum(pass_right),
    target_l_perct = target_l / targets,
    target_m_perct = target_m / targets,
    target_r_perct = target_r / targets,
    rec_l = sum(complete_left),
    rec_m = sum(complete_middle),
    rec_r = sum(complete_right),
    rec_l_perct = rec_l / targets,
    rec_m_perct = rec_m / targets,
    rec_r_perct = rec_r / targets
  ) %>% 
  ungroup() %>% 
  arrange(air_yards_pg %>% 
            desc()
  ) %>% 
  left_join(
    pbp_df %>% 
      filter(pass_attempt==1 & season_type=='REG' & two_point_attempt==0 & !is.na(receiver_id)) %>% 
      select(season, 
             game_id, 
             play_id, 
             posteam,
             defteam,
             air_yards, 
             yards_gained, 
             complete_pass,
             pass_location,
             cp,
             pass_touchdown) %>% 
      group_by(defteam) %>% 
      mutate(
        def_target = 0,
        game_played = 0
      )  %>% 
      group_by(game_id, defteam) %>% 
      mutate(game_played = ifelse(row_number()==1,1,0)) %>% 
      ungroup %>% 
      group_by(game_id, play_id, defteam) %>% 
      mutate(def_target = ifelse(row_number()==1,1,0),
             def_pass_left = ifelse(pass_location=='left',1,0),
             def_pass_middle = ifelse(pass_location =='middle',1,0),
             def_pass_right = ifelse(pass_location =='right',1,0),
             def_complete_left = ifelse(complete_pass == 1 & pass_location=='left',1,0),
             def_complete_middle = ifelse(complete_pass == 1 & pass_location =='middle',1,0),
             def_complete_right = ifelse(complete_pass == 1 & pass_location =='right',1,0)
      ) %>% 
      ungroup %>% 
      group_by(defteam) %>% 
      summarize(
        def_games = sum(game_played, na.rm = T),
        def_targets = sum(def_target, na.rm = T),
        def_targets_pg = def_targets / def_games,
        def_tot_rec_yards = sum(yards_gained, na.rm =TRUE),
        def_tot_air_yards = sum(air_yards, ra.rm = TRUE),
        def_air_yards_pg = def_tot_air_yards / def_games,
        def_racr = def_tot_rec_yards / def_tot_air_yards,
        def_receptions = sum(complete_pass),
        def_adot = def_tot_air_yards / def_targets,
        def_td = sum(pass_touchdown, ra.rm = TRUE),
        def_td_pg = def_td / def_games,
        def_target_l = sum(def_pass_left),
        def_target_m = sum(def_pass_middle),
        def_target_r = sum(def_pass_right),
        def_target_l_perct = def_target_l / def_targets,
        def_target_m_perct = def_target_m / def_targets,
        def_target_r_perct = def_target_r / def_targets,
        def_rec_l = sum(def_complete_left),
        def_rec_m = sum(def_complete_middle),
        def_rec_r = sum(def_complete_right),
        def_rec_l_perct = def_rec_l / def_targets,
        def_rec_m_perct = def_rec_m / def_targets,
        def_rec_r_perct = def_rec_r / def_targets
      ) %>% 
      ungroup() %>% 
      arrange(def_air_yards_pg %>% 
                desc()
      ),
    by = c('posteam' = 'defteam')
  )
  
schedule_df %>% 
  filter(week == 9)
