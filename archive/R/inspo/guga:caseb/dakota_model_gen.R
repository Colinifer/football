library(DBI)
library(RSQLite)
library(tidyverse)

if (grepl("Documents", getwd())){
  data_path <- "../"
} else { ### server
  data_path <- "/home/ben/data"
}


# this is the standard db built by nflfastR
tictoc::tic('loading data')
con <- DBI::dbConnect(SQLite(), glue::glue('{data_path}/master_db'))
pbp_db <- tbl(con, "cleaned_pbp") %>%
  select(desc, cpoe, name, down, season, week, home_wp, posteam, defteam, pass, rush, epa, success, qb_epa, id, complete_pass, incomplete_pass) %>%
  filter(pass == 1 | rush == 1) %>%
  filter(!is.na(posteam) & !is.na(epa) & !is.na(name) & season >= 2006 & season <= 2019) %>%
  mutate(name = if_else(season < 2014 & name == 'D.Carr', 'Da.Carr', name)) %>%
  collect()
tictoc::toc()

dbDisconnect(con)

pbp_db


lqbs <- pbp_db %>%
  group_by(name, season) %>%
  mutate(
    unadjusted_epa = qb_epa,
    epa = if_else(
      qb_epa < -4.5, -4.5, qb_epa)
  ) %>%
  summarize(
    n_att = sum(complete_pass + incomplete_pass),
    n_plays = n(),
    unadjusted_epa = sum(unadjusted_epa) / n_plays,
    epa = sum(epa)/n_plays,
    success =sum(success)/n_plays,
    cpoe = mean(cpoe, na.rm = TRUE),
    posteam = dplyr::last(posteam)
  ) %>%
  filter(n_att > 50 & n_plays > 200) %>%
  mutate(
    lepa = lag(epa, n = 1),
    lunad_epa = lag(unadjusted_epa, n = 1),
    lcpoe = lag(cpoe, n = 1),
    lsuccess = lag(success, n = 1),
    lplays = lag(n_plays),
    weight = (n_plays^2 + lplays^2)^.5
  ) %>% ungroup() 

lqbs


model_data <- lqbs %>%
  select(unadjusted_epa, lcpoe, lepa, weight) %>%
  dplyr::rename(
    target = unadjusted_epa,
    cpoe = lcpoe,
    epa_per_play = lepa
  )

# gam <- mgcv::gam(
#   target ~ s(cpoe) + s(epa) + ti(cpoe, epa), data = model_data, weights = weight
# )

dakota_model <- mgcv::gam(
  target ~ s(cpoe) + s(epa_per_play), data = model_data, weights = weight
)

save(dakota_model, file = '../stats/dakota_model.rda')