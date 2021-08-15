con <- fx.db_con()

current_season <- 2020

defteam_rec <- tbl(con, 'nflfastR_pbp') %>% 
  filter(season == current_season & 
           pass_attempt==1 & 
           season_type=='REG' & 
           two_point_attempt==0 & 
           !is.na(receiver_id)
         ) %>% 
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
  collect() %>% 
  # filter(!is.na(receiver)) %>% 
  # filter(defteam == 'LV') %>%
  group_by(receiver_player_id) %>% 
  mutate(
    target = 0,
    game_played = 0
  )  %>% 
  group_by(game_id, receiver, defteam) %>% 
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
  group_by(game_id, posteam, receiver, defteam) %>% 
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

# slice(1:20) %>% 
#   ggplot(
#     aes(x = receiver, y = tot_air_yards)) +
#   geom_bar(
#     aes(x = reorder(receiver, -tot_air_yards), 
#         y = tot_air_yards), 
#     stat='identity') +
#   geom_image(
#     aes(image = headshot_url), 
#     asp = 16/9,
#     nudge_y = 1
#   ) +
#   theme_cw +
#   theme(
#     axis.text.x = element_text(angle = 45)
#   )


# posteam_rec <- 
team_pass_totals <- tbl(con, 'nflfastR_pbp') %>% 
  filter(season == current_season & 
           pass_attempt==1 & 
           season_type=='REG' 
         & two_point_attempt==0 
         & !is.na(receiver_id)
         ) %>% 
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
  collect() %>% 
  # filter(!is.na(receiver)) %>% 
  # filter(posteam == 'LAC') %>%
  group_by(posteam) %>% 
  mutate(
    target = 0,
    game_played = 0
  )  %>% 
  group_by(game_id, posteam) %>% 
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
  group_by(posteam) %>% 
  summarize(
    games = sum(game_played, na.rm = T),
    targets = sum(target, na.rm = T),
    targets_pg = targets / games,
    tot_rec_yards = sum(yards_gained, na.rm =TRUE),
    rec_yards_pg = tot_rec_yards / games,
    tot_air_yards = sum(air_yards, na.rm = TRUE),
    air_yards_pg = tot_air_yards / games,
    racr = tot_rec_yards / tot_air_yards,
    receptions = sum(complete_pass),
    adot = tot_air_yards / targets,
    td = sum(pass_touchdown, na.rm = TRUE),
    td_pg = td / games,
    target_l = sum(pass_left, na.rm = TRUE),
    target_m = sum(pass_middle, na.rm = TRUE),
    target_r = sum(pass_right, na.rm = TRUE),
    target_l_perct = target_l / targets,
    target_m_perct = target_m / targets,
    target_r_perct = target_r / targets,
    rec_l = sum(complete_left, na.rm = TRUE),
    rec_m = sum(complete_middle, na.rm = TRUE),
    rec_r = sum(complete_right, na.rm = TRUE),
    rec_l_perct = rec_l / targets,
    rec_m_perct = rec_m / targets,
    rec_r_perct = rec_r / targets
  ) %>% rename(team = posteam)


rec_totals <- tbl(con, 'nflfastR_pbp') %>% 
  filter(season == current_season & 
           pass_attempt==1 & 
           season_type=='REG' & 
           two_point_attempt==0 & 
           !is.na(receiver_id)
         ) %>% 
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
  collect() %>% 
  # filter(!is.na(receiver)) %>% 
  # filter(posteam == 'LAC') %>%
  group_by(receiver_player_id) %>% 
  mutate(
    target = 0,
    game_played = 0
  )  %>% 
  group_by(game_id, receiver_id) %>% 
  mutate(game_played = ifelse(row_number()==1,1,0)) %>% 
  ungroup %>% 
  group_by(game_id, play_id, receiver_id) %>% 
  mutate(target = ifelse(row_number()==1,1,0),
         pass_left = ifelse(pass_location=='left',1,0),
         pass_middle = ifelse(pass_location =='middle',1,0),
         pass_right = ifelse(pass_location =='right',1,0),
         complete_left = ifelse(complete_pass == 1 & pass_location=='left',1,0),
         complete_middle = ifelse(complete_pass == 1 & pass_location =='middle',1,0),
         complete_right = ifelse(complete_pass == 1 & pass_location =='right',1,0)
  ) %>% 
  ungroup %>% 
  group_by(posteam, receiver_id) %>% 
  summarize(
    receiver = unique(receiver),
    games = sum(game_played, na.rm = T),
    tot_targets = sum(target, na.rm = T),
    targets_pg = tot_targets / games,
    tot_rec_yards = sum(yards_gained, na.rm =T),
    rec_yards_pg = tot_rec_yards / games,
    tot_air_yards = sum(air_yards, na.rm = T),
    air_yards_pg = tot_air_yards / games,
    racr = tot_rec_yards / tot_air_yards,
    receptions = sum(complete_pass, na.rm = T),
    adot = tot_air_yards / tot_targets,
    td = sum(pass_touchdown, na.rm = T),
    td_pg = td / games,
    target_l = sum(pass_left, na.rm = T),
    target_m = sum(pass_middle, na.rm = T),
    target_r = sum(pass_right, na.rm = T),
    target_l_perct = target_l / tot_targets,
    target_m_perct = target_m / tot_targets,
    target_r_perct = target_r / tot_targets,
    rec_l = sum(complete_left, na.rm = T),
    rec_m = sum(complete_middle, na.rm = T),
    rec_r = sum(complete_right, na.rm = T),
    rec_l_perct = rec_l / tot_targets,
    rec_m_perct = rec_m / tot_targets,
    rec_r_perct = rec_r / tot_targets
  ) %>% 
  mutate(
    targets_market_cap = team_pass_totals %>% filter(team == posteam) %>% select(targets) %>% as.integer(),
    targets_market_share = tot_targets / targets_market_cap %>% as.integer(),
    targets_pg_market_cap = team_pass_totals %>% filter(team == posteam) %>% select(targets_pg) %>% as.integer(),
    targets_pg_market_share = targets_pg / targets_pg_market_cap %>% as.integer(),
    air_yards_market_cap = team_pass_totals %>% filter(team == posteam) %>% select(tot_air_yards) %>% as.integer(),
    air_yards_market_share = tot_air_yards / air_yards_market_cap %>% as.integer(),
    air_yards_pg_market_cap = team_pass_totals %>% filter(team == posteam) %>% select(air_yards_pg) %>% as.integer(),
    air_yards_pg_market_share = air_yards_pg / air_yards_pg_market_cap %>% as.integer(),
    wopr = (1.5 * targets_market_share) + (0.7 * air_yards_market_share) %>% as.integer(),
    sum_shares = targets_market_share + air_yards_market_share
  ) %>% 
  left_join(sr_pbp_df %>% 
              # filter(season == current_season) %>% 
              select(-game_id_SR) %>%
              # collect() %>% 
              left_join(tbl(con, 'part') %>% 
                          filter(year == current_season) %>% 
                          collect() %>% 
                          left_join(sr_games_df) %>% 
                          mutate(play_id = as.integer(play_id),
                                 team = ifelse(team == 'JAC', 'JAX', team)), 
                        by = c('game_id' = 'game_id', 'play_id' = 'play_id', 'play_id_SR' = 'play_id_SR'), suffix = c('_fastR', '_SR')) %>% 
              mutate(route_run = ifelse(pass_attempt == 1 & posteam == team & (position == "TE" | position == "WR"), 1, 0)) %>% 
              filter(position == c('WR', 'TE', 'RB')) %>% 
              group_by(name_SR, reference) %>% 
              summarize(routes_run = sum(route_run, na.rm = T)),
            by = c('receiver_id' = 'reference')
            ) %>% 
  mutate(yprr = tot_rec_yards / routes_run) %>% 
  select(-name_SR) %>% 
  # filter(routes_run > 50) %>% 
  # arrange(-yprr) %>% 
  # view()
  left_join(sleeper_players_df %>% 
              select(gsis_id, position, height, espn_id, headshot_url),
            by = c('receiver_id' = 'gsis_id')
  ) %>% 
  # Add ESPN free agent info
  left_join(espn_players_df %>%
              select(id,
                     status,
                     team_id = onTeamId,
                     tot_fpoints = totalRating
                     ),
            by = c('espn_id' = 'id')) %>%
  # filter(status != "ONTEAM") %>%
  ungroup() %>% 
  arrange(air_yards_pg %>% 
            desc() 
  ) %>% filter(status != 'ONTEAM' | team_id == 8, # Filter available players and compare to current team
               games > 1
               ) %>% 
  # filter(routes_run > 20) %>% 
  arrange(-wopr) %>% 
  as_tibble()

rec_totals %>% saveRDS(file = glue('data/{year}/receiver_totals.rds'))

rec_totals %>% as_tibble() %>% 
  # select(receiver, tot_fpoints, wopr) %>% 
  ggplot(aes(x = tot_fpoints, y = wopr)) + 
  geom_text_repel(aes(label = ifelse(wopr > .2 & tot_fpoints > 50 & team_id != 8 & status != 'ONTEAM', receiver, ''), color = position), show.legend = FALSE) +
  geom_point(aes(color = position)) + 
  labs(
    x = 'Total Fantasy Points',
    y = 'WOPR'
  ) + 
  theme_cw

rec_totals %>% as_tibble() %>% 
  # select(receiver, tot_fpoints, air_yards_pg) %>% 
  ggplot(aes(x = tot_fpoints, y = tot_air_yards)) + 
  geom_text_repel(aes(label = ifelse(tot_air_yards > 400 & tot_fpoints > 50 & team_id != 8 & status != 'ONTEAM', receiver, ''), color = position), show.legend = FALSE) +
  geom_point(aes(color = position)) + 
  labs(
    x = 'Total Fantasy Points',
    y = 'Total Air Yards'
  ) +
  theme_cw

rec_totals %>% 
  as_tibble() %>% 
  mutate(fpoints_pg = tot_fpoints / games) %>% 
  # select(receiver, tot_fpoints, air_yards_pg) %>% 
  ggplot(aes(x = fpoints_pg, y = air_yards_pg)) + 
  geom_text_repel(aes(label = ifelse(air_yards_pg > 40 & fpoints_pg > 8, receiver, ''), color = position), show.legend = FALSE) +
  geom_point(aes(color = position)) + 
  labs(
    x = 'Fantasy Points per Game',
    y = 'Air Yards per Game'
  ) +
  theme_cw

  arrange(-targets_pg) %>% 
  slice(1:20) %>%
  arrange(-rec_yards_pg)

posteam_rec %>% 
  left_join(sleeper_players_df %>% select(gsis_id, position), 
            by = c("receiver_id" = "gsis_id")) %>% 
  filter((position == 'TE' & 
           targets_pg >= quantile(posteam_rec$targets_pg)[4] & 
           td_pg >= quantile(posteam_rec$td_pg)[3]) | (position == 'TE' & receiver == "J.Smith")) %>% 
  arrange(-adot)


posteam_rec %>% 
  left_join(sleeper_players_df %>% select(gsis_id, position), 
            by = c("receiver_id" = "gsis_id")) %>% 
  filter(position == 'WR' & 
           targets_pg >= quantile(posteam_rec$targets_pg)[4] & 
           td_pg >= quantile(posteam_rec$td_pg)[4]) %>% 
  arrange(-adot)


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



# QB Pass Yds vs Opp Def. Pass Yds ----------------------------------------

source('https://raw.githubusercontent.com/mrcaseb/nflfastR/master/R/utils.R')
source('https://github.com/mrcaseb/nflfastR/raw/master/R/helper_add_xyac.R')
source('https://github.com/mrcaseb/nflfastR/raw/master/R/helper_add_nflscrapr_mutations.R')
source('fantasy_football/xyac/add_xyac_old.R')


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

qb_passing <- pbp_df %>%
  filter(pass_attempt == 1 &
           season_type == 'REG' &
           two_point_attempt == 0 & !is.na(receiver_id) &
           # wp > .2 &
           # wp < .8 &
           air_yards > 0) %>%
  add_xyac_dist %>% 
  select(
    season = season.x,
    game_id,
    play_id,
    posteam = posteam.x,
    passer,
    passer_player_id,
    passer_id,
    receiver,
    receiver_player_id,
    receiver_id,
    yardline_100 = yardline_100.x,
    air_yards = air_yards.x,
    actual_yards_gained = yards_gained,
    complete_pass,
    cp,
    cpoe,
    yac_prob = prob,
    gain,
    pass_attempt
  ) %>%
  mutate(
    gain = ifelse(yardline_100 == air_yards, yardline_100, gain),
    yac_prob = ifelse(yardline_100 == air_yards, 1, yac_prob),
    # PPR_points = 1 + gain / 10 + ifelse(gain == yardline_100, 6, 0),
    # half_PPR_points = .5 + gain / 10 + ifelse(gain == yardline_100, 6, 0),
    catch_run_prob = cp * yac_prob,
    # exp_PPR_points = PPR_points * catch_run_prob,
    # exp_half_PPR_points = half_PPR_points * catch_run_prob,
    exp_yards = gain * catch_run_prob,
    actual_outcome = ifelse(actual_yards_gained == gain &
                              complete_pass == 1, 1, 0),
    # actual_PPR_points = ifelse(actual_outcome == 1, PPR_points, 0),
    # actual_half_PPR_points = ifelse(actual_outcome == 1, half_PPR_points, 0),
    # completion = 0,
    attempt = 0,
    game_played = 0,
    cayoe = cpoe * air_yards,
    sum_cayoe = 0
  ) %>%
  group_by(game_id, passer_player_id) %>%
  mutate(game_played = ifelse(row_number() == 1, 1, 0)) %>%
  ungroup %>%
  # group_by(game_id, play_id, passer) %>%
  # mutate(completion = ifelse(row_number() == 1, 1, 0)) %>%
  # ungroup %>%
  group_by(game_id, play_id, receiver) %>% 
  mutate(attempt = ifelse(row_number()==1,1,0)) %>% 
  ungroup %>% 
  group_by(posteam, passer) %>%
  # filter()
  summarize(
    season = unique(season),
    games = sum(game_played, na.rm = T),
    pass_attempts = sum(attempt, na.rm = T),
    completions = sum(actual_outcome, na.rm = T),
    yards = sum(ifelse(actual_outcome == 1, gain, 0), na.rm = T),
    td = sum(ifelse(gain == yardline_100, actual_outcome, 0), na.rm = T),
    # PPR_pts = sum(actual_PPR_points, na.rm = T),
    # half_PPR_pts = sum(actual_half_PPR_points, na.rm = T),
    exp_completions = sum(ifelse(attempt == 1, cp, NA), na.rm = T),
    exp_yards = sum(exp_yards, na.rm = T),
    exp_td = sum(ifelse(gain == yardline_100, catch_run_prob, 0), na.rm = T),
    # exp_PPR_pts = sum(exp_PPR_points, na.rm = T),
    # exp_half_PPR_pts = sum(exp_half_PPR_points, na.rm = T),
    sum_cayoe = sum(yards-exp_yards, na.rm = T),
  ) %>%
  mutate(
    # half_ppr_pts_diff = half_PPR_pts - exp_half_PPR_pts,
    # ppr_pts_diff = PPR_pts - exp_PPR_pts,
    cayoe_a = sum_cayoe / pass_attempts
  ) %>%
  ungroup

qb_passing %>% 
  mutate(
    yards_pg = yards / games
  ) %>% 
  select(
    posteam,
    passer,
    yards_pg
  ) %>% 
  left_join(
    matchup_df %>% 
      filter(week == pbp_df %>% select(week) %>% max() + 1) %>% 
      select(weekday, gametime, posteam, oppteam) %>% 
      left_join(
        team_passing %>% setNames(paste0('opp_', names(.))),
        by = c('oppteam' = 'opp_posteam')
        ),
    by = c('posteam' = 'posteam')
  ) %>% 
  mutate(
    tot_py_pg = yards_pg + (opp_def_tot_rec_yards / opp_games)
  ) %>% 
  select(
    passer,
    oppteam,
    tot_py_pg
  ) %>% 
  arrange(
    -tot_py_pg
  )
