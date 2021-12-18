library(pracma)

# lapply(1999:2019, function(x){

start_time <- Sys.time()
current_season <- year

# pbp_df <- purrr::map_df(current_season, function(x) {
#   readRDS(url(
#     glue::glue("https://github.com/guga31bb/nflfastR-data/blob/master/data/play_by_play_{x}.rds?raw=true")
#   ))
# # }) %>% filter(week < 9)
# }) %>% filter(season_type == 'REG') %>% filter(!is.na(posteam) & (rush == 1 | pass == 1))
# print(current_season)

con <- fx.db_con(x.host = 'localhost')
pbp <- tbl(con, 'nflfastR_pbp') %>% 
  filter(season == current_season & 
           season_type == 'REG' &
           !is.na(posteam) & 
           (rush == 1 | pass == 1)) %>% 
  select(
    season,
    game_id,
    week,
    play_id,
    posteam,
    defteam,
    rush,
    pass,
    epa
  ) %>% 
  collect()
dbDisconnect(con)
print(current_season)

n_week <- fx.n_week(pbp_df)

# Rolling EPA ---------------------------------------------------------------
teams <- pbp %>% 
  pull(posteam) %>% 
  unique()

map(teams, function(x.team){
  ma_plays <- 100
  
  x.team_info <- nflfastR::teams_colors_logos %>% 
    filter(team_abbr == x.team)
  
  x.team_name <- x.team_info %>% pull(team_name)
  
  print(glue("Plotting {x.team_name}'s net EPA"))
  
  # net_epa <- 
  
  net_roll_df <- pbp %>%
    filter(!is.na(posteam) & (rush == 1 | pass == 1)) %>% 
    mutate(team = posteam,
           opp_team = defteam) %>% 
    rbind(pbp %>%
            filter(!is.na(posteam) & (rush == 1 | pass == 1)) %>% 
            mutate(team = defteam,
                   opp_team = posteam)) %>% 
    arrange(team, game_id, play_id) %>% 
    group_by(team, season) %>%
    mutate(
      play_count = row_number()
    ) %>% 
    select(
      season,
      play_count,
      game_id,
      week,
      team,
      posteam,
      defteam,
      epa
    ) %>% 
    mutate(
      off_epa = ifelse(team == posteam, epa, NA),
      def_epa = ifelse(team == defteam, epa, NA),
      cu_epa = cummean(epa),  #this field is not used in this vignette but it could be substituted later to graph the cumulative EPA
      ma_epa = zoo::rollapply(epa, ma_plays, mean, na.rm = TRUE, fill = NA),
      ma_off_epa = zoo::rollapply(off_epa, ma_plays, mean, na.rm = TRUE, fill = NA),
      ma_def_epa = zoo::rollapply(def_epa, ma_plays, mean, na.rm = TRUE, fill = NA),
      ma_net_epa = ma_off_epa - ma_def_epa
    ) %>% 
    filter(team == x.team) # Selected team

  p <- net_roll_df %>% 
    # select(play_count, ma_off_epa, ma_def_epa) %>% 
    ggplot(aes()) + 
    geom_abline(slope=0, intercept=0, alpha=.8, color = 'red') +
    geom_line(aes(x = play_count, y = ma_off_epa), size = 1, color = color_cw[7], linetype = 'dotted', na.rm = T) +
    geom_line(aes(x = play_count, y = ma_def_epa), size = 1, color = color_cw[8], linetype = 'dotted', na.rm = T) + 
    # geom_line(aes(x = play_count, y = ma_epa), color = 'black') + 
    # geom_smooth(aes(x = play_count, y = ma_net_epa), method = 'loess', color = 'black')
    geom_line(aes(x = play_count, y = ma_net_epa), size = 1.5, color = color_cw[5]) + 
    geom_grob(data = x.team_info,
              aes(
                x = net_roll_df %>% filter(!is.na(ma_net_epa)) %>% pull(play_count) %>% max(),
                y = net_roll_df %>% filter(!is.na(ma_net_epa)) %>% pull(ma_net_epa) %>% min() + .1,
                label = grob_img_adj(team_logo_espn),
                vp.height = 0.08
              )) +
    labs(title = glue('{x.team_name} Rolling Net EPA'),
         subtitle = glue(''),
         color = "Line Color",
         x = 'Play Count',
         y = 'EPA') +
    theme_cw_dark + 
    theme()
  
  brand_plot(p, asp = 8/5, save_name = glue('plots/desktop/team_epa/{x.team}_rolling_net_epa.png'),  data_home = 'Data: @nflfastR', fade_borders = '')
  
  end_time <- Sys.time()
  end_time - start_time
})
