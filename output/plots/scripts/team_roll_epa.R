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

net_roll_df <- pbp %>%
  filter(!is.na(posteam) & (rush == 1 | pass == 1)) %>% 
  mutate(team = posteam,
         opp_team = defteam,
         poss = ifelse(team == posteam, 'off', 'def')) %>% 
  rbind(pbp %>%
          filter(!is.na(posteam) & (rush == 1 | pass == 1)) %>% 
          mutate(team = defteam,
                 opp_team = posteam,
                 poss = ifelse(team == posteam, 'off', 'def'))) %>% 
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
  )

map(teams, function(x.team){
  ma_plays <- 250
  
  x.team_info <- nflfastR::teams_colors_logos %>% 
    filter(team_abbr == x.team)
  
  x.team_name <- x.team_info %>% pull(team_name)
  
  print(glue("Plotting {x.team_name} net EPA"))
  
  net_epa_df <- net_roll_df %>% 
    filter(team == x.team & play_count %% 20 == 0) # Selected team
  
  max_axis <- {
    net_roll_df$ma_net_epa |> 
      na.omit() |> 
      max() +.1
    } |> 
    round(2)
  
  min_axis <- {
    net_roll_df$ma_net_epa |> 
      na.omit() |> 
      min() -.1
    } |> 
    round(2)
  
  p <- net_epa_df %>% 
    # select(play_count, ma_off_epa, ma_def_epa) %>% 
    ggplot(aes()) + 
    geom_abline(slope=0, intercept=0, alpha=.5, color = 'red') +
    geom_line(aes(x = play_count, y = ma_off_epa, color = 'Offense EPA'), size = .5, alpha = .33, na.rm = T) +
    geom_line(aes(x = play_count, y = ma_def_epa, color = 'Defense EPA'), size = .5, alpha = .33, na.rm = T) + 
    # geom_line(aes(x = play_count, y = ma_epa), color = 'black') + 
    # geom_smooth(aes(x = play_count, y = ma_net_epa), method = 'loess', color = 'black')
    geom_line(aes(x = play_count, y = ma_net_epa), size = .66, color = color_cw[5]) +
    scale_color_manual(values = c('Net EPA' = unname(color_cw[5]), 
                                  'Offense EPA' = unname(color_cw[7]),
                                  'Defense EPA' = unname(color_cw[8]))
                       ) + 
    # geom_grob(data = x.team_info,
    #           aes(
    #             x = net_roll_df %>% filter(!is.na(ma_net_epa)) %>% pull(play_count) %>% max(),
    #             y = net_roll_df %>% filter(!is.na(ma_net_epa)) %>% pull(ma_net_epa) %>% min() + .1,
    #             label = grob_img_adj(team_logo_espn),
    #             vp.height = 0.08
    #           )) +
    ylim(-1, 1) + 
    labs(title = glue('{current_season} {x.team_name} Rolling Net EPA'),
         subtitle = glue(''),
         color = "Line Color",
         x = 'Play Count',
         y = 'EPA') +
    theme_cw_dark + 
    theme(legend.position = 'top',
          legend.title = element_blank(),
          legend.text = element_text(size = 8))
  
  brand_plot(p, asp = 8/5, save_name = glue('plots/desktop/team_epa/{x.team}_rolling_net_epa_{current_season}.png'),  data_home = 'Data: @nflfastR', fade_borders = '')
  
  end_time <- Sys.time()
  end_time - start_time
})
