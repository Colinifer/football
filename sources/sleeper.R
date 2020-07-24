sleeper_players <- paste0("http://api.sleeper.app/v1/players/nfl") %>% 
  url() %>% 
  jsonlite::stream_in(
    simplifyDataFrame=FALSE
  ) %>% 
  data.frame()

sleeper_profile <- "https://api.sleeper.app/v1/user/colinw" %>% 
  url() %>%
  jsonlite::stream_in(
    simplifyDataFrame=TRUE
  ) %>% 
  data.frame()
