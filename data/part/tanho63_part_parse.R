# https://gist.github.com/tanho63/64b9405df69701311f608c174493f8b5#file-parse_sportradar_pbp
library(tidyverse)

# SR PBP ------------------------------------------------------------------

json_list <- readRDS(url("https://github.com/guga31bb/sport_radar/blob/master/data/pbp/0018213e-8423-40ab-b60d-562de98b6330.rds?raw=true"))

pbp <- json_list %>% 
  list() %>% 
  map(`[`,c("summary","periods")) %>% 
  tibble() %>% 
  unnest_wider(1) %>% 
  hoist('summary','season','week') %>% 
  mutate(
    season = map_dbl(season,pluck,"year"),
    week = map_dbl(week,pluck,"sequence"),
    summary = NULL
  ) %>% 
  hoist("periods","pbp") %>% 
  select(-periods) %>% 
  unnest_longer('pbp') %>%
  hoist('pbp','events') %>%
  select(-pbp) %>% 
  unnest_longer('events') %>% 
  unnest_wider('events') %>%
  unnest(c(-season,-week))



# SR Participation --------------------------------------------------------

json_list <- readRDS(url("https://github.com/guga31bb/sport_radar/blob/master/data/participation/c2b24b1a-98c5-465c-8f83-b82e746b4fcf.rds?raw=true"))

plays <- json_list %>% 
  list() %>% 
  map(`[`,c("summary","plays")) %>% 
  tibble() %>% 
  unnest_wider(1) %>% 
  hoist('summary','season','week') %>% 
  mutate(
    season = map_dbl(season,pluck,"year"),
    week = map_dbl(week,pluck,'sequence'),
    summary = NULL) %>%
  unnest_wider('plays') %>% 
  select(
    "season",
    "week",
    "play_id" = "sequence",
    "clock",
    "desc" = "description",
    "play_type" = "type",
    "home_team" = "home.alias",
    "away_team" = "away.alias",
    "home_player" = "home.players",
    "away_player" = "away.players") %>% 
  unnest(cols = c(-"season",-"week")) %>% 
  filter(!is.na(desc)) %>% 
  mutate(quarter_seconds_remaining = lubridate::period_to_seconds(lubridate::ms(clock)),
         increment = ifelse(lag(quarter_seconds_remaining) < quarter_seconds_remaining,1,0),
         increment = ifelse(is.na(increment), 0, increment),
         quarter = 1 + cumsum(increment),
         increment = NULL
  ) %>% 
  relocate(quarter,quarter_seconds_remaining,.before = clock) %>% 
  mutate(
    across(c("home_player","away_player"),
           map,
           ~.x %>% 
             transmute(row_id = row_number(),
                       name) %>% 
             pivot_wider(names_from = row_id, values_from = name)
    )) %>% 
  unnest_wider('home_player',names_sep = "_") %>% 
  unnest_wider("away_player",names_sep = "_")