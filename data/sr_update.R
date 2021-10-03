year

sr_key <- sportradar_con %>% 
  filter(sportradar_sports == "NFL Official") %>% 
  select(sportradar_keys) %>% 
  as.character()

# Update SR Schedule ------------------------------------------------------

# Automate to make calls only on Tuesdays
season_json <- jsonlite::fromJSON(url(glue('http://api.sportradar.us/nfl/official/trial/v6/en/games/{year}/REG/schedule.json?api_key={sr_key}')))
jsonlite::write_json(season_json, glue('data/schedules/{year}.json'))

sr_games_json <- read_json(glue('data/schedules/{year}.json'))

js_list <- sr_games_json %>% 
  tibble() %>% 
  unnest_longer(1) %>% 
  nth(1)

js_list %>% 
  length()

js_list %>% 
  pluck(5) %>% 
  nth(4) %>% 
  tibble() %>% 
  unnest(col)
