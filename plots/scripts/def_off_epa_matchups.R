offense <- pbp %>%
  filter(!is.na(posteam) & (rush == 1 | pass == 1)) %>% 
  group_by(posteam, season) %>%
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

defense <- pbp %>%
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
  inner_join(defense, by = c("season", "posteam" = "defteam")) 

chart_all %>% 
  left_join(
    matchup_df %>% 
      filter(week == 16) %>% 
      select(posteam, oppteam), 
    by = c('posteam')) %>% 
  inner_join(
    chart_all, 
    by = c('oppteam' = 'posteam'), 
    suffix = c('_pos', '_opp')) %>% 
  mutate(
    epa_play_diff = (def_epa_pos + off_epa_opp) / 2
  ) %>% 
  select(
    posteam, 
    oppteam, 
    def_epa_pos,
    off_epa_opp,
    epa_play_diff
  ) %>% 
  arrange(
    epa_play_diff
  )


