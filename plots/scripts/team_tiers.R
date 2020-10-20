if (exists("pbp_df") == FALSE) {
  pbp_df <- readRDS(url(glue('https://github.com/guga31bb/nflfastR-data/blob/master/data/play_by_play_{year}.rds?raw=true')))
}
print(year)

offense <- pbp_df %>%
  filter(!is.na(epa) & !is.na(posteam)) %>% 
  group_by(posteam, season)%>%
  summarize(
    n_pass=sum(pass),
    n_rush=sum(rush),
    epa_per_pass=sum(epa*pass)/n_pass,
    epa_per_rush=sum(epa*rush)/n_rush,
    success_per_pass=sum(pass*epa>0)/n_pass,
    success_per_rush=sum(rush*epa>0)/n_rush,
    off_epa=mean(epa),
    off_success=mean(success)
  )

defense <- pbp_df %>%
  filter(!is.na(epa) & !is.na(defteam)) %>% 
  group_by(defteam, season)%>%
  summarize(
    def_n_pass=sum(pass),
    def_n_rush=sum(rush),
    def_epa_per_pass=sum(epa*pass)/def_n_pass,
    def_epa_per_rush=sum(epa*rush)/def_n_rush,
    def_success_per_pass=sum(pass*epa>0)/def_n_pass,
    def_success_per_rush=sum(rush*epa>0)/def_n_rush,
    def_epa=mean(epa),
    def_success=mean(success)
  )

chart_all <- offense %>% 
  inner_join(defense, by=c("season", "posteam" = "defteam")) %>%
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr"))

res <- 800 #size of exported plots
slope = -1.5 #for the tiers stuff
qb_min <- 320 #min # of qb plays

p <- chart_all %>% 
  ggplot(aes(x = off_epa, y = def_epa)) +
  geom_image(aes(image = team_logo_espn), size = 0.05, asp = 16/9) +
  geom_hline(yintercept = mean(chart_all$off_epa), color = "red", linetype = "dashed") +
  geom_vline(xintercept =  mean(chart_all$def_epa), color = "red", linetype = "dashed") +
  labs(x = "Offense EPA/play",
       y = "Defense EPA/play",
       # caption = "Data: @nflscrapR",
       title = paste(year, "NFL team tiers")) +
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
    plot.title = element_text(size = 16, hjust = 0.5),
    #panel.grid.minor = element_blank()
  )

brand_plot(p, asp = 16/10, save_name = glue('plots/desktop/team_tiers_{year}.png'), data_home = 'Data: @nflfastR', fade_borders = 'tr')