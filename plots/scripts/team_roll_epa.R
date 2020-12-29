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
  select(-xyac_median_yardage) %>% 
  collect()
print(current_season)

n_week <- fx.n_week(pbp_df)

# Tiers -------------------------------------------------------------------

offense <- pbp_df %>%
  filter(!is.na(posteam) & (rush == 1 | pass == 1)) %>% 
  group_by(posteam, season)%>%
  mutate(
    cu_epa=cummean(EPA),  #this field is not used in this vignette but it could be substituted later to graph the cumulative EPA
    ma_epa=rollapply(EPA,ma_plays,mean,align='right',fill=NA)
  )
  summarize(
    n_pass=sum(pass, na.rm = T),
    n_rush=sum(rush, na.rm = T),
    epa_per_pass=sum(epa*pass, na.rm = T)/n_pass,
    epa_per_rush=sum(epa*rush, na.rm = T)/n_rush,
    success_per_pass=sum(pass*epa>0, na.rm = T)/n_pass,
    success_per_rush=sum(rush*epa>0, na.rm = T)/n_rush,
    off_epa=mean(epa, na.rm = T),
    off_success=mean(success, na.rm = T)
  )

defense <- pbp_df %>%
  filter(!is.na(posteam) & (rush == 1 | pass == 1)) %>% 
  group_by(defteam, season) %>%
  summarize(
    def_n_pass=sum(pass, na.rm = T),
    def_n_rush=sum(rush, na.rm = T),
    def_epa_per_pass=sum(epa*pass, na.rm = T)/def_n_pass,
    def_epa_per_rush=sum(epa*rush, na.rm = T)/def_n_rush,
    def_success_per_pass=sum(pass*epa>0, na.rm = T)/def_n_pass,
    def_success_per_rush=sum(rush*epa>0, na.rm = T)/def_n_rush,
    def_epa=mean(epa, na.rm = T),
    def_success=mean(success, na.rm = T)
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
