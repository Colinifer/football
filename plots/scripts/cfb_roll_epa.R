
current_season <- year

con <- fx.db_con(x.host = 'localhost')
pbp <- tbl(con, 'cfbfastR_pbp') %>% 
  filter(year == current_season & 
           !is.na(pos_team) & 
           (rush == 1 | pass == 1)) %>% 
  collect()
print(current_season)

n_week <- fx.n_week(pbp)

team_info <- cfbfastR::cfbd_team_info(year = current_season)

team_colors_logos = team_info %>% 
  select(school, abbreviation, color, logos, alt_color) %>%
  unnest(logos) %>%
  group_by(school) %>%
  slice(1) %>% 
  ungroup()