cfb_team_info <- readRDS('data/cfb/cfb_team_info.rds') %>% 
  rename(
    logos_light = `logos[0]`, 
    logos_dark = `logos[1]`
  )