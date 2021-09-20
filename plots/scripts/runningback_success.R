# Load packages

# install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)

library(nflfastR)
library(tidyverse)
library(purrr)
library(INLA)

# acquire 2020 rushing data

current_season <- year
# current_season <- 2010
con <- fx.db_con(x.host = 'localhost')
pbp_df <- tbl(con, 'nflfastR_pbp') %>% 
  filter(season >= 2010 & 
           season_type == 'REG' &
           !is.na(posteam)) %>% 
  select(-xyac_median_yardage) %>% 
  collect()
print(current_season)
dbDisconnect(con)

n_week <- fx.n_week(pbp_df)

passers <- pbp_df %>% 
  filter(play_type == 'pass') %>% 
  group_by(season, passer_player_id) %>% 
  summarise(passes = n()) %>% 
  filter(passes >= 10)

pbp_rush <- pbp_df %>% 
  filter(!is.na(epa) & play_type == 'run' &
           qb_scramble == 0 & qb_dropback == 0) %>% 
  anti_join(passers, by = c('rusher_player_id' = 'passer_player_id'))

# Modeling
# Set penalized complexity / exponential prior
rusher_summary <- map_df(2010:current_season, function(x){
  print(x)
  pc.prior.epa <- list(prec = list(prior = 'pc.prec',
                                   param = c(3 * sd(
                                     pbp_rush %>%
                                       filter(season == x) %>%
                                       pull(epa)
                                   ),
                                   0.01)))
  
  # Model rusher epa, using student t errors
  
  rush.epa.mod <-
    inla(
      epa ~ f(rusher_player_id, model = 'iid', hyper = pc.prior.epa),
      data = pbp_rush %>% 
        filter(season == x),
      verbose = TRUE,
      family = 't',
      control.compute = list(waic = TRUE)
    )
  
  # Summarise rusher epa above or below average, with uncertainty
  
  if (exists('rusher_summary') == TRUE) {
    rusher_summary <- rbind(
      rusher_summary, 
      rush.epa.mod$summary.random$rusher_player_id %>% 
        select(
          ID,
          epa_mean = mean,
          epa_sd = sd
        ) %>% 
        mutate(
          epa_mean = round(epa_mean, 3),
          epa_sd = round(epa_sd, 3)
        ) %>% 
        arrange(-epa_mean) %>% 
        mutate(season = current_season) %>% 
        as_tibble()
    ) %>% 
      arrange(-epa_mean)
  } else {
    rusher_summary <- rush.epa.mod$summary.random$rusher_player_id %>% 
      select(
        ID,
        epa_mean = mean,
        epa_sd = sd
      ) %>% 
      mutate(
        epa_mean = round(epa_mean, 3),
        epa_sd = round(epa_sd, 3)
      ) %>% 
      arrange(-epa_mean) %>% 
      mutate(season = current_season) %>% 
      as_tibble()
  }
  return(rusher_summary)
})

roster_df <-
  readRDS(url("https://github.com/guga31bb/nflfastR-data/blob/master/roster-data/roster.rds?raw=true")) %>% 
  decode_player_ids(fast = TRUE) %>% 
  select(team = team.abbr,
         first_name = teamPlayers.firstName,
         last_name = teamPlayers.lastName,
         gsis = teamPlayers.gsisId,
         headshot_url = teamPlayers.headshot_url
  ) %>%
  mutate(full_name = glue('{first_name} {last_name}'))

rusher_summary %>%
  left_join(roster_df %>% 
              select(
                full_name
              )
            )

rusher_summary %>% 
  arrange(season) %>% 
  pull(season) %>% 
  unique()

rm(pbp_df, n_week, passers, pbp_rush, pcpc.prior.epa, rush.epa.mod)
