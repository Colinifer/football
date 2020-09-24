# https://twitter.com/itsdanpearson/status/1305954925436956672
# https://github.com/danielpearson714/theprophet/blob/master/QBefficiency

#Quarterback efficiency Reactable 
library(nflfastR)
library(dplyr)
library(reactable)

#read in Week 1 play-by-play data
# fd <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2020.rds'))

#Color palette for CPOE and EPA/Play columns
tablepal <- function(x) rgb(colorRamp(c("#FFE4B5", "tomato3"))(x), maxColorValue = 255)

#Tidy up and make the table  
qbeff <- pbp_df %>% 
  filter(play_type == "pass", down <=4, !is.na(cpoe), !is.na(xyac_mean_yardage)) %>% 
  group_by(passer, posteam, week) %>% 
  summarise(mean(cpoe), mean(epa), sum(epa), mean(air_yards), mean(xyac_mean_yardage)) %>%
  filter(passer != "R.Griffin III") %>% 
  filter(passer != "T.Hill") %>% 
  select(-week) %>% 
  reactable(
    pagination = FALSE,
    highlight = TRUE,
    outlined = TRUE,
    compact = TRUE,
    borderless = FALSE,
    striped = FALSE,
    fullWidth = FALSE,
    style = list(fontFamily = "Lucida Console"),
    defaultColDef = colDef(
      align = "center",
      minWidth = 110
    ),
    columns = list(
      passer = colDef(
        name = "Name",
        minWidth = 170,
        align = "left"),
      posteam = colDef(
        name = "Team",
        minWidth = 80),
      `mean(cpoe)` = colDef(
        name = "CPOE", format = colFormat(digits = 2),
        style = function(value) {
          normalized <- (value - min(qbeff$`mean(cpoe)`)) / (max(qbeff$`mean(cpoe)`) - min(qbeff$`mean(cpoe)`))
          color <- tablepal(normalized)
          list(background = color)
        }, class = "border-left"),
      `mean(epa)` = colDef(
        name = "EPA/Play", format = colFormat(digits = 2),
        style = function(value) {
          normalized <- (value - min(qbeff$`mean(epa)`)) / (max(qbeff$`mean(epa)`) - min(qbeff$`mean(epa)`))
          color <- tablepal(normalized)
          list(background = color)
        }, class = "border-right"),
      `sum(epa)` = colDef(
        name = "Total EPA", format = colFormat(digits = 2)),
      `mean(air_yards)` = colDef(
        name = "ADOT", format = colFormat(digits = 2)),
      `mean(xyac_mean_yardage)` = colDef(
        name = "Mean xYAC", format = colFormat(digits = 2)))
  )
