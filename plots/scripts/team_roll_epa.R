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

pbp_df <- pbp_ds %>% 
  filter(season == current_season & 
           season_type == 'REG' &
           !is.na(posteam) & 
           (rush == 1 | pass == 1)) %>% 
  collect()
print(current_season)

n_week <- fx.n_week(pbp_df)

# Tiers -------------------------------------------------------------------

ma_plays <- 250

# net_epa <- 
  
pbp_df %>%
  filter(!is.na(posteam) & (rush == 1 | pass == 1)) %>% 
  mutate(team = posteam,
         opp_team = defteam) %>% 
  rbind(pbp_df %>%
           filter(!is.na(posteam) & (rush == 1 | pass == 1)) %>% 
           mutate(team = defteam,
                  opp_team = posteam)) %>% 
  arrange(team, game_id, play_id) %>% 
  group_by(team, season) %>%
  mutate(
    play_count = row_number()
  ) %>% 
  select(
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
  filter(team == 'NO') %>% 
  # select(play_count, ma_off_epa, ma_def_epa) %>% 
  ggplot(aes()) +
  geom_line(aes(x = play_count, y = ma_off_epa), size = 1, color = 'black', linetype = 'dotted', na.rm = T) +
  geom_line(aes(x = play_count, y = ma_def_epa), size = 1, color = 'red', linetype = 'dotted', na.rm = T) + 
  # geom_line(aes(x = play_count, y = ma_epa), color = 'black') + 
  # geom_smooth(aes(x = play_count, y = ma_net_epa), method = 'loess', color = 'black')
  geom_line(aes(x = play_count, y = ma_net_epa), size = 2, color = 'black')

offense <- pbp_df %>%
  filter(!is.na(posteam) & (rush == 1 | pass == 1)) %>% 
  group_by(posteam, season)%>%
  mutate(
    cu_epa = cummean(epa),  #this field is not used in this vignette but it could be substituted later to graph the cumulative EPA
    ma_epa = rollapply(epa,ma_plays,mean,align='right',fill=NA),
    ma_epa_offense = rollapply(epa,ma_plays,mean,align='right',fill=NA),
    ma_epa_defense = 
    play_count = row_number(),
    week_team = paste0("WK", ifelse(week > 9, week, paste0(0,week)), " ", defteam)
  )

defense <- pbp_df %>%
  filter(!is.na(posteam) & (rush == 1 | pass == 1)) %>% 
  group_by(defteam, season) %>%
  group_by(posteam, season)%>%
  mutate(
    cu_epa = cummean(epa),  #this field is not used in this vignette but it could be substituted later to graph the cumulative EPA
    ma_epa_defense = rollapply(epa,ma_plays,mean,align='right',fill=NA),
    play_count = row_number(),
    week_team = paste0("WK", ifelse(week > 9, week, paste0(0,week)), " ", defteam)
  )

chart_all <- offense %>% 
  inner_join(defense, by=c("season", "posteam" = "defteam")) %>%
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr"))

p <- chart_all %>% 
  ggplot(aes(x = off_epa, y = def_epa)) +
  geom_image(aes(image = team_logo_espn), size = 0.05, asp = 16/9) +
  geom_hline(yintercept = mean(chart_all$def_epa, na.rm = T), color = "red", linetype = "dashed") +
  geom_vline(xintercept =  mean(chart_all$off_epa, na.rm = T), color = "red", linetype = "dashed") +
  labs(x = "Offense EPA/play",
       y = "Defense EPA/play",
       # caption = "Data: @nflscrapR",
       title = glue("{current_season} NFL Team Tiers"),
       subtitle = glue("Offense and defense EPA per play through week {n_week}")) +
  geom_abline(slope=slope, intercept=.4, alpha=.2) +
  geom_abline(slope=slope, intercept=.3, alpha=.2) +
  geom_abline(slope=slope, intercept=0, alpha=.2) +
  geom_abline(slope=slope, intercept=.1, alpha=.2) +
  geom_abline(slope=slope, intercept=.2, alpha=.2) +
  geom_abline(slope=slope, intercept=-.1, alpha=.2) +
  geom_abline(slope=slope, intercept=-.2, alpha=.2) +
  geom_abline(slope=slope, intercept=-.3, alpha=.2) +
  scale_y_reverse() +
  theme_cw +
  theme(
    axis.title.y = element_text(angle = 90),
    legend.position = c(0.99, 0.99),
    legend.justification = c(1, 1) ,
    plot.title = element_text(size = 16),
    #panel.grid.minor = element_blank()
  )

brand_plot(p, asp = 16/10, save_name = glue('plots/desktop/team_tiers/team_rolling_epa_{current_season}.png'), data_home = 'Data: @nflfastR', fade_borders = '')


end_time <- Sys.time()
end_time - start_time

rm(time_series, res, slope, qb_min, epa_data, offense, opponent_data, chart_all, matchup_chart_all, p, start_time, end_time)
# })
