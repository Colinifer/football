# https://github.com/asonty/ngs_highlights

# * install packages ----
install.packages("devtools", "patchwork", "tidyverse")
devtools::install_github('thomasp85/ggforce')
devtools::install_github('thomasp85/gganimate')

# * load packages ----
library(devtools)
library(dplyr)
library(gganimate)
library(ggforce)
library(ggplot2)
library(readr)
library(patchwork)




# * load helper functions ----
source("~/Documents/dev/GitHub/asonty-ngs_highlights/utils/scripts/data_utils.R")
source("~/Documents/dev/GitHub/asonty-ngs_highlights/utils/scripts/plot_utils.R")

highlights <- fetch_highlights_list(team_ = "BAL", season_ = 2019)
play_data <- fetch_play_data(playKey_ = 242)

first_frame <- play_data %>%
  filter(event == "line_set") %>% 
  distinct(frame) %>% 
  slice_max(frame) %>% 
  pull()

final_frame <- play_data %>% 
  filter(event == "tackle" | event == "touchdown" | event == "out_of_bounds") %>% 
  distinct(frame) %>% 
  slice_max(frame) %>% 
  pull()

first_frame
final_frame

plot_play_frame(play_data_ = play_data, frame_ = 180)
plot_play_frame(play_data_ = play_data, frame_ = 200, velocities_ = T)
plot_play_frame(play_data_ = play_data, frame_ = 220, velocities_ = F, voronoi_ = T)
plot_play_sequence(play_data, first_frame_ = first_frame, final_frame_ = final_frame, n_ = 6, velocities_ = T, voronoi_ = T)




# * reduce dataset ----
reduced_play_data <- play_data %>% filter(frame >= first_frame, frame <= final_frame+10)




# * get play details ----
play_desc <- reduced_play_data$playDescription %>% .[1]
play_dir <- reduced_play_data$playDirection %>% .[1]
yards_togo <- reduced_play_data$yardsToGo %>% .[1]
los <- reduced_play_data$absoluteYardlineNumber %>% .[1]
togo_line <- if(play_dir=="left") los-yards_togo else los+yards_togo




# * separate player and ball tracking data ----
player_data <- reduced_play_data %>% 
  select(frame, homeTeamFlag, teamAbbr, displayName, gsisId, jerseyNumber, position, positionGroup,
         x, y, s, o, dir, event) %>% 
  filter(displayName != "ball")
ball_data <- reduced_play_data %>% 
  select(frame, homeTeamFlag, teamAbbr, displayName, jerseyNumber, position, positionGroup,
         x, y, s, o, dir, event) %>% 
  filter(displayName == "ball")



# * get team details ----
h_team <- reduced_play_data %>% filter(homeTeamFlag == 1) %>% distinct(teamAbbr) %>% pull()
a_team <- reduced_play_data %>% filter(homeTeamFlag == 0) %>% distinct(teamAbbr) %>% pull()
#  call helper function to get team colors
team_colors <- fetch_team_colors(h_team_ = h_team, a_team_ = a_team)
h_team_color1 <- team_colors[1]
h_team_color2 <- team_colors[2]
a_team_color1 <- team_colors[3]
a_team_color2 <- team_colors[4]



# * compute velocity components ----
#  velocity angle in radians
player_data$dir_rad <- player_data$dir * pi / 180

#  velocity components
player_data$v_x <- sin(player_data$dir_rad) * player_data$s
player_data$v_y <- cos(player_data$dir_rad) * player_data$s

#  there are assuredly better ways to do this

# * identify the fastest player from each team at each frame ----
fastest_players <- player_data %>% # filter out ball-tracking data
  group_by(frame, teamAbbr) %>% # group by frame and team
  arrange(s) %>% top_n(s, n=1) %>% # take only the players with the highest speed on each team at every frame
  mutate(isFastestFlag = 1) %>% # create new flag identifying fastest players
  ungroup() %>% 
  select(frame, gsisId, isFastestFlag) %>%  # reduce dataset to the columns needed for joining and the new flag
  arrange(frame) # sort by frame

player_data <- player_data %>% 
  left_join(fastest_players, by = c("frame" = "frame", "gsisId" = "gsisId")) %>% # join on frame and gsisId
  mutate(isFastestFlag = case_when(is.na(isFastestFlag) ~ 0, TRUE ~ 1)) # replace NA values for isFastestFlag with 0



play_frames <- plot_field() + # plot_field() is a helper function that returns a ggplot2 object of an NFL field
  # line of scrimmage
  annotate(
    "segment",
    x = los, xend = los, y = 0, yend = 160/3,
    colour = "#0d41e1"
  ) +
  # 1st down marker
  annotate(
    "segment",
    x = togo_line, xend = togo_line, y = 0, yend = 160/3,
    colour = "#f9c80e"
  ) +
  # away team velocities
  geom_segment(
    data = player_data %>% filter(teamAbbr == a_team),
    mapping = aes(x = x, y = y, xend = x + v_x, yend = y + v_y),
    colour = a_team_color1, size = 1, arrow = arrow(length = unit(0.01, "npc"))
  ) + 
  # home team velocities
  geom_segment(
    data = player_data %>% filter(teamAbbr == h_team),
    mapping = aes(x = x, y = y, xend = x + v_x, yend = y + v_y),
    colour = h_team_color1, size = 1, arrow = arrow(length = unit(0.01, "npc"))
  ) +
  # away team locations
  geom_point(
    data = player_data %>% filter(teamAbbr == a_team),
    mapping = aes(x = x, y = y),
    fill = "#ffffff", color = a_team_color2,
    shape = 21, alpha = 1, size = 6
  ) +
  # away team jersey numbers
  geom_text(
    data = player_data %>% filter(teamAbbr == a_team),
    mapping = aes(x = x, y = y, label = jerseyNumber),
    color = a_team_color1, size = 3.5, #family = "mono"
  ) +
  # home team locations
  geom_point(
    data = player_data %>% filter(teamAbbr == h_team),
    mapping = aes(x = x, y = y),
    fill = h_team_color1, color = h_team_color2,
    shape = 21, alpha = 1, size = 6
  ) +
  # home team jersey numbers
  geom_text(
    data = player_data %>% filter(teamAbbr == h_team),
    mapping = aes(x = x, y = y, label = jerseyNumber),
    color = h_team_color2, size = 3.5, #family = "mono"
  ) +
  # ball location
  geom_point(
    data = ball_data,
    mapping = aes(x = x, y = y),
    fill = "#935e38", color = "#d9d9d9",
    shape = 21, alpha = 1, size = 4
  ) +
  # highlight fastest players
  geom_point(
    data = player_data %>% filter(isFastestFlag == 1),
    mapping = aes(x = x, y = y),
    colour = "#e9ff70",
    alpha = 0.5, size = 8
  ) +
  # play description and always cite your data source!
  labs(
    title = play_desc,
    caption = "Source: NFL Next Gen Stats"
  ) + 
  # animation stuff
  transition_time(frame) +
  ease_aes('linear') +
  NULL

# ensure timing of play matches 10 frames-per-second (h/t NFL Football Ops)
play_length <- length(unique(player_data$frame))
play_anim <- animate(
  play_frames,
  fps = 10, 
  nframe = play_length,
  width = 850,
  height = 500,
  end_pause = 10
)