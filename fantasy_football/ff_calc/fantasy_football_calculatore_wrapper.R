# Fantasy Football Calc
url <- "https://fantasyfootballcalculator.com/api/v1/adp/"
type = "standard"
team_count = 10
year = 2020
adp_df <- GET(glue(url, type, "?teams=", team_count, "&year=", year)) %>% content(as = "parsed", type = "application/json")
adp_df$players %>% 
  
  # Test
  adp_df$players %>% 
  unlist(recursive = FALSE) %>%
  as.data.frame %>%
  select(starts_with("player_id"))

map_dfc(adp_df$players, `[`, 1)
#