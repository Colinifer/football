#To get pbp_all_rp, you'll need to check out Ben's tutorial (https://gist.github.com/guga31bb/1a4e446773d8ed030b6a44ebe12ca14c), where he shows
#how to get the nflscrapR data and clean it up

library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggrepel)
## pbp_all_rp <- readRDS("FILENAME/pbp_rp.rds")

#Success Rate vs. EPA of Steelers receivers
pbp_all_rp %>%
  filter(posteam == "PIT" & season == 2019 & !is.na(receiver_player_name) & play_type == "pass" & down <= 4) %>%
  group_by(receiver_player_name) %>%
  summarize(success_rate = mean(success), mean_epa = mean(epa), targets = n()) %>%
  filter(targets >= 40) %>%
  ggplot(aes(x = success_rate, y = mean_epa)) + 
  geom_point() + 
  geom_text_repel(aes(label = receiver_player_name)) +
  labs(x = "Success Rate",
       y = "EPA",
       title = "Steelers Receivers: Success Rate and EPA", 
       subtitle = "Minimum 40 Targets",
       caption = "Data from nflscrapR"
  )

#Success Rate vs. EPA in red zone
pbp_all_rp %>%
  filter(posteam == "PIT" & season == 2019 & !is.na(receiver_player_name) & play_type == "pass" & down <= 4 & yardline_100 <= 20) %>%
  group_by(receiver_player_name) %>%
  summarize(success_rate = mean(success), mean_epa = mean(epa), targets = n()) %>%
  filter(targets >= 10) %>%
  ggplot(aes(x = success_rate, y = mean_epa)) + 
  geom_point() + 
  geom_text_repel(aes(label = receiver_player_name)) +
  labs(x = "Success Rate",
       y = "EPA",
       title = "Steelers Receivers: Success Rate and EPA", 
       subtitle = "In the Red Zone",
       caption = "Data from nflscrapR"
       
       
  )

#Receivers aDOT
pbp_all_rp %>%
  filter(posteam == "PIT", season == 2019 & !is.na(receiver_player_name) & play_type == "pass" & down <= 4) %>%
  group_by(receiver_player_name) %>%
  summarize(adot = mean(air_yards), targets = n()) %>%
  filter(targets >= 40) %>%
  ggplot(aes(x = receiver_player_name, y = adot)) + 
  geom_bar(stat = "identity", color = "black", fill = "gold") + 
  theme_minimal() + 
  labs(
    x = "Receiver",
    y = "aDOT",
    title = "Steelers Receivers Average Depth of Target",
    subtitle = "Minimum 40 Targets", 
    caption = "Data from nflscrapR"
  )

#aDOT vs. Catch Rate
pbp_all_rp %>%
  filter(season == 2019 & !is.na(receiver_player_name) & play_type == "pass" & down <= 4) %>%
  group_by(receiver_player_name) %>%
  summarize(adot = mean(air_yards), targets = n(), catch_rate = sum(complete_pass)/targets) %>%
  filter(adot >= 5 & targets >= 40) %>%
  ggplot(aes(x = adot, y = catch_rate)) + 
  geom_point() + 
  geom_text_repel(aes(label = receiver_player_name), color = "grey40", segment.color = "black", size = 2.5) + 
  geom_smooth(method = lm, se = FALSE, linetype = "dashed", color = "black") + 
  scale_y_continuous(breaks = c(.4,.45,.5,.55,.6,.65,.7,.75,.8,.85)) + 
  scale_x_continuous(breaks = c(6,8,10,12,14,16,18,20)) + 
  theme_bw() + 
  labs(x = "aDOT", 
       y = "Catch Rate",
       title = "Catch Rate vs aDOT",
       subtitle = "Minimum 40 Targets",
       caption = "Data from nflscrapR"
  )

#Success Rate vs. aDOT with certain receivers highlighted
pbp_all_rp %>%
  filter(season == 2019 & !is.na(receiver_player_name) & play_type == "pass" & down <= 4) %>%
  group_by(receiver_player_name) %>%
  summarize(adot = mean(air_yards), targets = n(), catch_rate = sum(complete_pass)/targets) %>%
  filter(adot >= 5 & targets >= 40) %>%
  mutate(highlight = ifelse(receiver_player_name == "A.Brown" | receiver_player_name == "J.Smith-Schuster" | receiver_player_name == "D.Moncrief", T, F)) %>%
  ggplot(aes(x = adot, y = catch_rate)) + 
  geom_point(aes(color = highlight)) + 
  geom_text_repel(aes(label = ifelse(receiver_player_name == "A.Brown" | receiver_player_name == "J.Smith-Schuster" | receiver_player_name == "D.Moncrief", receiver_player_name, " ")), color = "grey40", segment.color = "black", size = 2.5) + 
  geom_smooth(method = lm, se = FALSE, linetype = "dashed", color = "black") + 
  scale_y_continuous(breaks = c(.4,.45,.5,.55,.6,.65,.7,.75,.8,.85)) + 
  scale_x_continuous(breaks = c(6,8,10,12,14,16,18,20)) + 
  theme_bw() + 
  scale_color_manual(values = c('#595959', 'red'))
labs(x = "aDOT", 
     y = "Catch Rate",
     title = "Catch Rate vs aDOT",
     subtitle = "Minimum 40 Targets",
     caption = "Data from nflscrapR"
)