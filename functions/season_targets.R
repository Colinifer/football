# Notes -------------------------------------------------------------------
# 
# Need to fix and adjust for old team/transferred cities

# Found bug where RBs not getting credited for targets

# Create and clean data ---------------------------------------------------

u.year <- 2019
f.player_season <- paste("data/season_total/", u.year, "players.csv", sep = "")

x.players <- assign(paste(u.year, "players", sep = ""), read_csv(f.player_season))

nfl_teamcolors <- read_csv("data/season_total/team_abbr.csv")
nfl_teamcolors <- nfl_teamcolors %>% 
  arrange(nflscrapr_abbrev)
x.targets <- left_join(x.players, teamcolors, by = "Team")

x.players <- filter(x.players, rush.att > 0 | targets > 0)
x.players <- filter(x.players, Team != "APR" & Team != "NPR")
x.players <- x.players[,c("Team", "playerID", "name", "pass.att", "pass.comp", "passyds", "rush.att", "rushyds", "targets", "recept", "recyds")]

x.players <- x.players %>% group_by(Team, playerID, name) %>% 
  summarise(pass.att = sum(pass.att), 
            pass.comp = sum(pass.comp), 
            passyds = sum(passyds), 
            rush.att = sum(rush.att), 
            rushyds = sum(rushyds),
            targets = sum(targets),
            recept = sum(recept),
            recyds = sum(recyds)
            )

c("pass.att", "passyds", "rush.att", "rushyds", "targets", "recept", "recyds")

player_rush <- x.players %>% filter(targets > 0)

# Targets -----------------------------------------------------------------

gg.min <- 25 # What is the minimum receiving yards for the sample?
gg.targets <- x.players %>%
  filter(targets > gg.min) %>%
  arrange(desc(targets)) %>% 
  arrange(Team)
  # mutate(name = reorder_within(name, targets, Team, sep = "")) %>%

# Mobile
targetsplot_mobile <- gg.targets %>%
  ggplot() +
  #ggplot(aes(x=reorder_within(name, -targets, Team), fill = "transparent", colour = Team)) +
  theme_bw() +
  scale_color_manual(values = nfl_teamcolors$secondary) +
  scale_fill_manual(values = alpha(nfl_teamcolors$primary, 1)) +
  
  # geom_bar(aes(x=reorder_within(name, -targets, Team), y=targets, fill = Team), alpha = 1.0, stat ="identity") +
  geom_bar(aes(x=reorder_within(name, -recept, Team), y=recept,  fill = Team), size = 1, stat ="identity") +
  geom_text(aes(x=reorder_within(name, -recept, Team), y=recept, label = recept), nudge_y = -10, color = "white") +
  geom_bar(aes(x=reorder_within(name, -targets, Team), y=targets, fill = Team, color = Team),  alpha = .01, size = 1, stat ="identity") +
  geom_text(aes(x=reorder_within(name, -targets, Team), y=targets, label = targets), nudge_y = 10, color = "black") +
  
  #geom_bar(aes(y=targets,  colour = Team, fill = "white"), data = filter(x.players, targets > 20), alpha = 1.0, stat ="identity") +
  #geom_bar(aes(y=recept,  fill = Team), stat ="identity") +
  
  # scale_x_reordered() +
  #scale_color_teams(2, guide = FALSE) + 
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  facet_wrap( ~ Team, ncol = 32/8, scales = "free_x") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position="none") +
  labs(title = paste(u.year, "targets by team", sep = " "), 
       subtitle = "Minimum 20 targets") +
  xlab("Targeted players") +
  ylab("Targets")

f.gg_file <- paste("plots/mobile/", u.year, "targets_1440mobile.png", sep = "")
ggsave(f.gg_file, targetsplot_mobile, width = 14, height = 25, dpi = 100, limitsize = FALSE)
f.gg_file <- paste("plots/mobile/", u.year, "targets_1080mobile.png", sep = "")
ggsave(f.gg_file, targetsplot_mobile, width = 10, height = 19, dpi = 100, limitsize = FALSE)

# Desktop
targetsplot_desktop <- gg.targets %>%
  ggplot() +
  #ggplot(aes(x=reorder_within(name, -targets, Team), fill = "transparent", colour = Team)) +
  theme_bw() +
  scale_color_manual(values = nfl_teamcolors$secondary) +
  scale_fill_manual(values = alpha(nfl_teamcolors$primary, 1)) +
  
  # geom_bar(aes(x=reorder_within(name, -targets, Team), y=targets, fill = Team), alpha = 1.0, stat ="identity") +
  geom_bar(aes(x=reorder_within(name, -recept, Team), y=recept,  fill = Team), size = 1, stat ="identity") +
  geom_text(aes(x=reorder_within(name, -recept, Team), y=recept, label = recept), nudge_y = -10, color = "white") +
  geom_bar(aes(x=reorder_within(name, -targets, Team), y=targets, fill = Team, color = Team),  alpha = .01, size = 1, stat ="identity") +
  geom_text(aes(x=reorder_within(name, -targets, Team), y=targets, label = targets), nudge_y = 10, color = "black") +
  
  #geom_bar(aes(y=targets,  colour = Team, fill = "white"), data = filter(x.players, targets > 20), alpha = 1.0, stat ="identity") +
  #geom_bar(aes(y=recept,  fill = Team), stat ="identity") +
  
  # scale_x_reordered() +
  #scale_color_teams(2, guide = FALSE) + 
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  facet_wrap( ~ Team, ncol = 32/4, scales = "free_x") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position="none") +
  labs(title = paste(u.year, "targets by team", sep = " "), 
       subtitle = "Minimum 20 targets") +
  xlab("Targeted players") +
  ylab("Targets")

f.gg_file <- paste("plots/desktop/", u.year, "targets_1440desktop.png", sep = "")
ggsave(f.gg_file , targetsplot_desktop, width = 25, height = 14, dpi = 100, limitsize = FALSE)
f.gg_file <- paste("plots/desktop/", u.year, "targets_1080desktop.png", sep = "")
ggsave(f.gg_file, targetsplot_desktop, width = 19, height = 10, dpi = 100, limitsize = FALSE)

# Receiving Yards ---------------------------------------------------------

gg.min <- 250   # What is the minimum receiving yards for the sample?
gg.recyds <- x.players %>%
  filter(recyds > gg.min) %>%
  arrange(desc(recyds)) %>% 
  arrange(Team)
  # mutate(name = reorder_within(name, recyds, Team, sep = "")) %>%

mobile <- 
  facet_wrap( ~ Team, ncol = 32/8, scales = "free_x")


# Mobile
recydsplot_mobile <- gg.recyds %>%
  ggplot() +
  theme_bw() +
  scale_color_manual(values = nfl_teamcolors$primary) +
  scale_fill_manual(values = alpha(nfl_teamcolors$primary, 1)) +
  geom_bar(aes(x=reorder_within(name, -recyds, Team), y=recyds,  fill = Team), stat ="identity") +
  # scale_x_reordered() +
  #scale_color_teams(2, guide = FALSE) + 
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  facet_wrap( ~ Team, ncol = 32/8, scales = "free_x") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position="none") +
  labs(title = "2019 receiving yards by team", 
       subtitle = paste("Minimum", gg.min, "receiving yards", sep = " ")) +
  xlab("Receivers") +
  ylab("Receiving Yards")

f.gg_file <- paste("plots/mobile/recyds_1440mobile.png", sep = "")
ggsave(f.gg_file, recydsplot_mobile, width = 14, height = 25, dpi = 100, limitsize = FALSE)
f.gg_file <- paste("plots/mobile/recyds_1080mobile.png", sep = "")
ggsave(f.gg_file, recydsplot_mobile, width = 10, height = 19, dpi = 100, limitsize = FALSE)


# Desktop
recydsplot_desktop <- gg.recyds %>%
  ggplot() +
  # ggplot(aes(x=reorder_within(name, -recyds, Team), fill = "transparent", colour = Team)) +
  theme_bw() +
  scale_color_manual(values = nfl_teamcolors$primary) +
  scale_fill_manual(values = alpha(nfl_teamcolors$primary, .8)) +
  
  geom_bar(aes(x=reorder_within(name, -recyds, Team), y=recyds, fill = Team), alpha = 1.0, stat ="identity") +
  # geom_bar(aes(y=recyds,  colour = Team, fill = "white"), data = filter(x.players, recyds > 20), alpha = 1.0, stat ="identity") +
  # geom_bar(aes(y=recept,  fill = Team), stat ="identity") +
  
  # scale_x_reordered() +
  # scale_color_teams(2, guide = FALSE) +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  facet_wrap( ~ Team, ncol = 32/4, scales = "free_x") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position="none") +
  labs(title = "2019 receiving yards by team", 
       subtitle = paste("Minimum", gg.min, "receiving yards", sep = " ")) +
  xlab("Receivers") +
  ylab("Receiving Yards")

f.gg_file <- paste("plots/desktop/recyds_1440desktop.png", sep = "")
ggsave(f.gg_file , recydsplot_desktop, width = 25, height = 14, dpi = 100, limitsize = FALSE)
f.gg_file <- paste("plots/desktop/recyds_1080desktop.png", sep = "")
ggsave(f.gg_file, recydsplot_desktop, width = 19, height = 10, dpi = 100, limitsize = FALSE)









# TEST --------------------------------------------------------------------
# Data frames

gg.targets_minimum <- 20
gg.targets <- x.players %>%
  filter(targets > gg.targets_minimum) %>%  # What is the minimum receiving yards for the sample?
  arrange(desc(targets)) %>% 
  arrange(Team)

gg.recyds_minimum <- 250
gg.recyds <- x.players %>%
  filter(recyds > gg.recyds_minimum) %>%  # What is the minimum receiving yards for the sample?
  arrange(desc(recyds)) %>% 
  arrange(Team)

gg.passyds_minimum <- 400
gg.passyds <- x.players %>%
  filter(passyds > gg.passyds_minimum) %>%  # What is the minimum receiving yards for the sample?
  arrange(desc(passyds)) %>% 
  arrange(Team)

gg.pass.att_minimum <- 100
gg.pass.att <- x.players %>%
  filter(pass.att > gg.pass.att_minimum) %>%  # What is the minimum receiving yards for the sample?
  arrange(desc(pass.att)) %>% 
  arrange(Team)


# NFL colors plot
nfl_plot <-  ggplot() +
  theme_bw() +
  scale_color_manual(values = nfl_teamcolors$primary) +
  scale_fill_manual(values = alpha(nfl_teamcolors$primary, .5))


# Targets descending
targets_plot <- gg.targets %>% 
  ggplot() +
  theme_bw() +
  scale_color_manual(values = nfl_teamcolors$primary) +
  scale_fill_manual(values = alpha(nfl_teamcolors$primary, .2)) +
  geom_bar(aes(x=reorder_within(name, -targets, Team), y=targets,  fill = Team, color = Team), stat ="identity") +
  geom_bar(aes(x=reorder_within(name, -recept, Team), y=recept,  fill = Team), stat ="identity") +
  # scale_x_reordered() +
  #scale_color_teams(2, guide = FALSE) + 
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position="none") +
  labs(title = "2019 targets by team", 
       subtitle = paste("Minimum", gg.targets_minimum, "targets", sep = " ")) +
  xlab("Targeted players") +
  ylab("Targets")

# Receiving Yards descending
recyds_plot <- gg.recyds %>% 
  ggplot() +
  theme_bw() +
  scale_color_manual(values = nfl_teamcolors$primary) +
  scale_fill_manual(values = alpha(nfl_teamcolors$primary, 1)) +
  geom_bar(aes(x=reorder_within(name, -recyds, Team), y=recyds,  fill = Team), stat ="identity") +
  # scale_x_reordered() +
  #scale_color_teams(2, guide = FALSE) + 
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position="none") +
  labs(title = "2019 receiving yards by team", 
       subtitle = paste("Minimum", gg.recyds_minimum, "receiving yards", sep = " ")) +
  xlab("Receivers") +
  ylab("Receiving Yards")

# Passing Yards descending
passyds_plot <- gg.passyds %>% 
  ggplot() +
  theme_bw() +
  scale_color_manual(values = nfl_teamcolors$primary) +
  scale_fill_manual(values = alpha(nfl_teamcolors$primary, 1)) +
  geom_bar(aes(x=reorder_within(name, -passyds, Team), y=passyds,  fill = Team), stat ="identity") +
  # scale_x_reordered() +
  #scale_color_teams(2, guide = FALSE) + 
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position="none") +
  labs(title = "2019 passing yards by team", 
       subtitle = paste("Minimum", gg.passyds_minimum, "passing yards", sep = " ")) +
  xlab("Passers") +
  ylab("Passing Yards")

# Passing Yards descending
pass.att_plot <- gg.pass.att %>% 
  ggplot() +
  theme_bw() +
  scale_color_manual(values = nfl_teamcolors$primary) +
  scale_fill_manual(values = alpha(nfl_teamcolors$primary, 1)) +
  geom_bar(aes(x=reorder_within(name, -pass.att, Team), y=pass.att,  fill = Team), stat ="identity") +
  # scale_x_reordered() +
  #scale_color_teams(2, guide = FALSE) + 
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position="none") +
  labs(title = "2019 passing attempts by team", 
       subtitle = paste("Minimum", gg.pass.att_minimum, "passing attempts", sep = " ")) +
  xlab("Passers") +
  ylab("Passing Attempts")

# Facets by screen
mobile_facet <- 
  facet_wrap( ~ Team, ncol = 32/8, scales = "free_x")

desktop_facet <- 
  facet_wrap( ~ Team, ncol = 32/4, scales = "free_x")

# Save Functions
targetsplot_mobile <- targets_plot + mobile_facet
targetsplot_desktop <- targets_plot + desktop_facet
recydsplot_mobile <- recyds_plot + mobile_facet
recydsplot_desktop <-recyds_plot + desktop_facet
passyds_plot_desktop_mobile <- passyds_plot
pass.att_plot_desktop_mobile <- pass.att_plot

"plots/mobile/targets_1080mobile.png" %>% ggsave(targetsplot_mobile, width = 10, height = 19, dpi = 100, limitsize = FALSE)
"plots/mobile/targets_1440mobile.png" %>% ggsave(targetsplot_mobile, width = 14, height = 25, dpi = 100, limitsize = FALSE)
"plots/desktop/targets_1440desktop.png" %>% ggsave(targetsplot_desktop, width = 25, height = 14, dpi = 100, limitsize = FALSE)
"plots/desktop/targets_1080desktop.png" %>% ggsave(targetsplot_desktop, width = 19, height = 10, dpi = 100, limitsize = FALSE)

"plots/mobile/recyds_1440mobile.png" %>% ggsave(recydsplot_mobile, width = 14, height = 25, dpi = 100, limitsize = FALSE)
"plots/mobile/recyds_1080mobile.png" %>% ggsave(recydsplot_mobile, width = 10, height = 19, dpi = 100, limitsize = FALSE)
"plots/desktop/recyds_1440desktop.png" %>% ggsave(recydsplot_desktop, width = 25, height = 14, dpi = 100, limitsize = FALSE)
"plots/desktop/recyds_1080desktop.png" %>% ggsave(recydsplot_desktop, width = 19, height = 10, dpi = 100, limitsize = FALSE)

"plots/mobile/passyds_1440mobile.png" %>% ggsave(passyds_plot_desktop_mobile, width = 14, height = 18, dpi = 100, limitsize = FALSE)
"plots/mobile/passyds_1080mobile.png" %>% ggsave(passyds_plot_desktop_mobile, width = 10, height = 14, dpi = 100, limitsize = FALSE)
"plots/desktop/passyds_1080desktop.png" %>% ggsave(passyds_plot_desktop_mobile, width = 14, height = 10, dpi = 100, limitsize = FALSE)


"plots/mobile/pass.att_1440mobile.png" %>% ggsave(pass.att_plot_desktop_mobile, width = 14, height = 18, dpi = 100, limitsize = FALSE)
"plots/mobile/pass.att_1080mobile.png" %>% ggsave(pass.att_plot_desktop_mobile, width = 10, height = 14, dpi = 100, limitsize = FALSE)
"plots/desktop/pass.att_1080desktop.png" %>% ggsave(pass.att_plot_desktop_mobile, width = 14, height = 10, dpi = 100, limitsize = FALSE)



# Target Plot
targetplot <- x.players %>%
  filter(targets > 10) %>%
  arrange(desc(targets)) %>% 
  arrange(Team) %>% 
  # mutate(name = reorder_within(name, targets, Team, sep = "")) %>%
  ggplot() +
  theme_bw() +
  scale_color_manual(values = nfl_teamcolors$primary) +
  scale_fill_manual(values = alpha(nfl_teamcolors$primary, 0.2)) +
  geom_bar(aes(x=reorder_within(name, -targets, Team), y1=targets, y2=recept, colour = Team,  fill = Team), stat ="identity") +
  # scale_x_reordered() +
  #scale_color_teams(2, guide = FALSE) + 
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  facet_wrap( ~ Team, ncol = 32/4, scales = "free_x") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "2019 targets by team", 
       subtitle = "Minimum 10 targets") +
  xlab("Targeted players") +
  ylab("Targets")

ggsave("targets02.png", targetplot, width = 15, height = 10, dpi = 100, limitsize = FALSE)

