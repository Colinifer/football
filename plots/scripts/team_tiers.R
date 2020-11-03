library(pracma)

pbp_df <- purrr::map_df(2020, function(x) {
  readRDS(url(
    glue::glue("https://github.com/guga31bb/nflfastR-data/blob/master/data/play_by_play_{x}.rds?raw=true")
  ))
})
print(year)

# If week < 10, us current weeks in season
time_series <- dplyr::if_else(pbp_df %>%
  filter(season == Sys.Date() %>%
           format(format = "%Y") %>%
           as.integer()) %>%
  select(week) %>%
  max() - 1 < 10,
  pbp_df %>%
    filter(season == Sys.Date() %>%
             format(format = "%Y") %>%
             as.integer()) %>%
    select(week) %>%
    max() - 2,
  10)

res <- 800 #size of exported plots
slope = -1.5 #for the tiers stuff
qb_min <- 320 #min # of qb plays

# Adjust EPA --------------------------------------------------------------
# https://www.opensourcefootball.com/posts/2020-08-20-adjusting-epa-for-strenght-of-opponent/

epa_data <- pbp_df %>%
  # dplyr::filter(!is.na(epa), !is.na(ep), !is.na(posteam), play_type == "pass" | play_type == "run") %>%
  dplyr::filter(!is.na(epa), !is.na(ep), !is.na(posteam)) %>%
  dplyr::group_by(game_id, season, week, posteam, home_team) %>%
  dplyr::summarise(
    off_epa = mean(epa),
  ) %>%
  dplyr::left_join(pbp_df %>%
                     filter(!is.na(epa), !is.na(ep), !is.na(posteam), play_type == "pass" | play_type == "run") %>%
                     dplyr::group_by(game_id, season, week, defteam, away_team) %>%
                     dplyr::summarise(def_epa = mean(epa)),
                   by = c("game_id", "posteam" = "defteam", "season", "week"),
                   all.x = TRUE
  ) %>%
  dplyr::mutate(opponent = ifelse(posteam == home_team, away_team, home_team)) %>%
  dplyr::select(game_id, season, week, home_team, away_team, posteam, opponent, off_epa, def_epa)

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

# Construct opponent dataset and lag the moving average of their last ten games.
opponent_data <- epa_data %>%
  dplyr::select(-opponent) %>%
  dplyr::rename(
    opp_off_epa = off_epa,
    opp_def_epa = def_epa
  ) %>%
  dplyr::group_by(posteam) %>%
  dplyr::arrange(season, week) %>%
  dplyr::mutate(
    opp_def_epa = pracma::movavg(opp_def_epa, n = time_series, type = "s"),
    opp_def_epa = dplyr::lag(opp_def_epa),
    opp_off_epa = pracma::movavg(opp_off_epa, n = time_series, type = "s"),
    opp_off_epa = dplyr::lag(opp_off_epa)
  )

# Merge opponent data back in with the weekly epa data
epa_data <- epa_data %>%
  left_join(
    opponent_data,
    by = c("game_id", "season", "week", "home_team", "away_team", "opponent" = "posteam"),
    all.x = TRUE
  )

epa_data <- epa_data %>%
  dplyr::left_join(epa_data %>%
                     dplyr::filter(posteam == home_team) %>%
                     dplyr::group_by(season, week) %>%
                     dplyr::summarise(
                       league_mean = mean(off_epa + def_epa)
                     ) %>%
                     dplyr::ungroup() %>%
                     dplyr::group_by(season) %>%
                     dplyr::mutate(
                       league_mean = lag(pracma::movavg(league_mean, n = as.integer(time_series), type = "s"), ) # We lag because we need to know the league mean up to that point in the season
                     ),
                   by = c("season", "week"),
                   all.x = TRUE
  )

# Adjust EPA
epa_data <- epa_data %>%
  dplyr::mutate(
    off_adjustment_factor = ifelse(!is.na(league_mean), league_mean - opp_def_epa, 0),
    def_adjustment_factor = ifelse(!is.na(league_mean), league_mean - opp_off_epa, 0),
    adjusted_off_epa = off_epa + off_adjustment_factor,
    adjusted_def_epa = def_epa + def_adjustment_factor,
  )

chart_all <- epa_data %>% 
  filter(season == 2020) %>% 
  arrange(posteam) %>% 
  group_by(posteam) %>% 
  summarize(
    adjusted_off_epa = mean(adjusted_off_epa),
    adjusted_def_epa = mean(adjusted_def_epa)
  ) %>% 
  # filter(row_number() == n()) %>% 
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr"))

p <- chart_all %>% 
  ggplot(aes(x = adjusted_off_epa, y = adjusted_def_epa)) +
  geom_image(aes(image = team_logo_espn), size = 0.05, asp = 16/9) +
  geom_hline(yintercept = mean(chart_all$adjusted_off_epa), color = "red", linetype = "dashed") +
  geom_vline(xintercept =  mean(chart_all$adjusted_def_epa), color = "red", linetype = "dashed") +
  labs(x = "Adj. Offense EPA/play",
       y = "Adj. Defense EPA/play",
       # caption = "Data: @nflscrapR",
       title = paste(year, "NFL adjusted team tiers through week", as.integer(time_series) + 1)) +
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

brand_plot(p, asp = 16/10, save_name = glue('plots/desktop/team_tiers_adj_{year}.png'), data_home = 'Data: @nflfastR', fade_borders = 'tr')

# Tiers -------------------------------------------------------------------

epa_data <- pbp_df %>%
  # dplyr::filter(!is.na(epa), !is.na(ep), !is.na(posteam), play_type == "pass" | play_type == "run") %>%
  dplyr::filter(!is.na(epa), !is.na(ep), !is.na(posteam)) %>%
  dplyr::group_by(game_id, season, week, posteam, home_team) %>%
  dplyr::summarise(
    off_epa = mean(epa),
  ) %>%
  dplyr::left_join(pbp_df %>%
                     filter(!is.na(epa), !is.na(ep), !is.na(posteam), play_type == "pass" | play_type == "run") %>%
                     dplyr::group_by(game_id, season, week, defteam, away_team) %>%
                     dplyr::summarise(def_epa = mean(epa)),
                   by = c("game_id", "posteam" = "defteam", "season", "week"),
                   all.x = TRUE
  ) %>%
  dplyr::mutate(opponent = ifelse(posteam == home_team, away_team, home_team)) %>%
  dplyr::select(game_id, season, week, home_team, away_team, posteam, opponent, off_epa, def_epa)

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

p <- chart_all %>% 
  ggplot(aes(x = off_epa, y = def_epa)) +
  geom_image(aes(image = team_logo_espn), size = 0.05, asp = 16/9) +
  geom_hline(yintercept = mean(chart_all$off_epa), color = "red", linetype = "dashed") +
  geom_vline(xintercept =  mean(chart_all$def_epa), color = "red", linetype = "dashed") +
  labs(x = "Offense EPA/play",
       y = "Defense EPA/play",
       # caption = "Data: @nflscrapR",
       title = paste(year, "NFL team tiers through week", as.integer(time_series) + 1)) +
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

# Pass v Rush EPA
p <- chart_all %>% 
  ggplot(aes(x = epa_per_rush, y = epa_per_pass)) +
  geom_image(aes(image = team_logo_espn), size = 0.05, asp = 16/9) +
  geom_hline(yintercept = mean(chart_all$epa_per_pass), color = "red", linetype = "dashed") +
  geom_vline(xintercept =  mean(chart_all$epa_per_rush), color = "red", linetype = "dashed") +
  labs(x = "Rush EPA/play",
       y = "Pass EPA/play",
       # caption = "Data: @nflscrapR",
       title = paste(year, "NFL passing and rushing team tiers through week", as.integer(time_series) + 1)) +
  geom_abline(slope=slope, intercept=.4, alpha=.2) +
  geom_abline(slope=slope, intercept=.3, alpha=.2) +
  geom_abline(slope=slope, intercept=0, alpha=.2) +
  geom_abline(slope=slope, intercept=.1, alpha=.2) +
  geom_abline(slope=slope, intercept=.2, alpha=.2) +
  geom_abline(slope=slope, intercept=-.1, alpha=.2) +
  geom_abline(slope=slope, intercept=-.2, alpha=.2) +
  geom_abline(slope=slope, intercept=-.3, alpha=.2) +
  theme_cw +
  theme(
    axis.title.y = element_text(angle = 90),
    legend.position = c(0.99, 0.99),
    legend.justification = c(1, 1) ,
    plot.title = element_text(size = 16, hjust = 0.5),
    #panel.grid.minor = element_blank()
  )

brand_plot(p, asp = 16/10, save_name = glue('plots/desktop/team_off_pass_and_rush_tiers_{year}.png'), data_home = 'Data: @nflfastR', fade_borders = 'tr')

# Next week match-ups -----------------------------------------------------

matchup_chart_all <- chart_all %>%
  left_join(
    matchup_df %>% 
      filter(week == n_week + 1) %>% 
      select(posteam, oppteam, weekday, gametime),
    by = c('posteam')
  ) %>% left_join(chart_all %>% 
                    select(
                      -season,
                    ) %>% 
                    rename(
                      opp_n_pass = n_pass,
                      opp_n_rush = n_rush,
                      opp_epa_per_pass = epa_per_pass,
                      opp_epa_per_rush = epa_per_rush,
                      opp_success_per_pass = success_per_pass,
                      opp_success_per_rush = success_per_rush,
                      opp_off_epa = off_epa,
                      opp_def_n_pass = def_n_pass,
                      opp_def_n_rush = def_n_rush,
                      opp_def_epa_per_pass = def_epa_per_pass,
                      opp_def_epa_per_rush = def_epa_per_rush,
                      opp_def_success_per_pass = def_success_per_pass,
                      opp_def_success_per_rush = def_success_per_rush,
                      opp_def_epa = def_epa,
                      opp_def_success = def_success,
                      opp_team_name = team_name,
                      opp_team_id = team_id,
                      opp_team_nick = team_nick,
                      opp_team_color = team_color,
                      opp_team_color2 = team_color2,
                      opp_team_color3 = team_color3,
                      opp_team_color4 = team_color4,
                      opp_team_logo_wikipedia = team_logo_wikipedia,
                      opp_team_logo_espn = team_logo_espn
                    ),
                  by = c('oppteam' = 'posteam')
                  ) %>%
  filter(!is.na(oppteam ))

p <- matchup_chart_all %>% 
  ggplot(aes(x = off_epa, y = opp_def_epa)) +
  geom_image(aes(image = team_logo_espn), size = 0.05, asp = 16/9) +
  geom_hline(yintercept = mean(chart_all$off_epa), color = "red", linetype = "dashed") +
  geom_vline(xintercept =  mean(chart_all$def_epa), color = "red", linetype = "dashed") +
  labs(x = "Offense EPA/play",
       y = "Defense EPA/play",
       # caption = "Data: @nflscrapR",
       title = paste(year, "NFL team tiers through week", as.integer(time_series) + 1)) +
  geom_abline(slope=slope, intercept=.4, alpha=.2) +
  geom_abline(slope=slope, intercept=.3, alpha=.2) +
  geom_abline(slope=slope, intercept=0, alpha=.2) +
  geom_abline(slope=slope, intercept=.1, alpha=.2) +
  geom_abline(slope=slope, intercept=.2, alpha=.2) +
  geom_abline(slope=slope, intercept=-.1, alpha=.2) +
  geom_abline(slope=slope, intercept=-.2, alpha=.2) +
  geom_abline(slope=slope, intercept=-.3, alpha=.2) +
  theme_cw +
  theme(
    axis.title.y = element_text(angle = 90),
    legend.position = c(0.99, 0.99),
    legend.justification = c(1, 1) ,
    plot.title = element_text(size = 16, hjust = 0.5),
    #panel.grid.minor = element_blank()
  )

brand_plot(p, asp = 16/10, save_name = glue('plots/desktop/matchup_team_tiers_{year}.png'), data_home = 'Data: @nflfastR', fade_borders = 'tr')

# Passing matchup
p <- matchup_chart_all %>% 
  ggplot(aes(x = epa_per_pass, y = opp_def_epa_per_pass)) +
  geom_image(aes(image = team_logo_espn), size = 0.05, asp = 16/9) +
  geom_hline(yintercept = mean(chart_all$off_epa), color = "red", linetype = "dashed") +
  geom_vline(xintercept =  mean(chart_all$def_epa), color = "red", linetype = "dashed") +
  labs(x = "Offense Pass EPA/play",
       y = "Opponent Defense Pass EPA/play",
       # caption = "Data: @nflscrapR",
       title = paste(year, "NFL team tiers through week", as.integer(time_series) + 1)) +
  geom_abline(slope=slope, intercept=.4, alpha=.2) +
  geom_abline(slope=slope, intercept=.3, alpha=.2) +
  geom_abline(slope=slope, intercept=0, alpha=.2) +
  geom_abline(slope=slope, intercept=.1, alpha=.2) +
  geom_abline(slope=slope, intercept=.2, alpha=.2) +
  geom_abline(slope=slope, intercept=-.1, alpha=.2) +
  geom_abline(slope=slope, intercept=-.2, alpha=.2) +
  geom_abline(slope=slope, intercept=-.3, alpha=.2) +
  theme_cw +
  theme(
    axis.title.y = element_text(angle = 90),
    legend.position = c(0.99, 0.99),
    legend.justification = c(1, 1) ,
    plot.title = element_text(size = 16, hjust = 0.5),
    #panel.grid.minor = element_blank()
  )

brand_plot(p, asp = 16/10, save_name = glue('plots/desktop/matchup_pass_team_tiers_{year}.png'), data_home = 'Data: @nflfastR', fade_borders = 'tr')

# Rushing matchup
p <- matchup_chart_all %>% 
  ggplot(aes(x = epa_per_rush, y = opp_def_epa_per_rush)) +
  geom_image(aes(image = team_logo_espn), size = 0.05, asp = 16/9) +
  geom_hline(yintercept = mean(chart_all$off_epa), color = "red", linetype = "dashed") +
  geom_vline(xintercept =  mean(chart_all$def_epa), color = "red", linetype = "dashed") +
  labs(x = "Offense Rush EPA/play",
       y = "Opponent Defense Rush EPA/play",
       # caption = "Data: @nflscrapR",
       title = paste(year, "NFL team tiers through week", as.integer(time_series) + 1)) +
  geom_abline(slope=slope, intercept=.4, alpha=.2) +
  geom_abline(slope=slope, intercept=.3, alpha=.2) +
  geom_abline(slope=slope, intercept=0, alpha=.2) +
  geom_abline(slope=slope, intercept=.1, alpha=.2) +
  geom_abline(slope=slope, intercept=.2, alpha=.2) +
  geom_abline(slope=slope, intercept=-.1, alpha=.2) +
  geom_abline(slope=slope, intercept=-.2, alpha=.2) +
  geom_abline(slope=slope, intercept=-.3, alpha=.2) +
  theme_cw +
  theme(
    axis.title.y = element_text(angle = 90),
    legend.position = c(0.99, 0.99),
    legend.justification = c(1, 1) ,
    plot.title = element_text(size = 16, hjust = 0.5),
    #panel.grid.minor = element_blank()
  )

brand_plot(p, asp = 16/10, save_name = glue('plots/desktop/matchup_rush_team_tiers_{year}.png'), data_home = 'Data: @nflfastR', fade_borders = 'tr')
