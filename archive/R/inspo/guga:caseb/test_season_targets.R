u.year <- 2019
f.player_season <- paste("data/season_total/", u.year, "players.csv", sep = "")

assign(paste(u.year, "players", sep = ""), read_csv(f.player_season))

nfl_teamcolors <- read_csv("data/season_total/team_abbr.csv")
nfl_teamcolors <- nfl_teamcolors %>% 
  arrange(nflscrapr_abbrev)
x.targets <- left_join(x.players, teamcolors, by = "Team")

x.players <- filter(`2019players`, rush.att > 0 | targets > 0)
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

player_rush <- x.players %>% filter(targets > 0)

x.targets <- x.players %>% filter(targets > 0)

## x.targets <- inner_join(x.players, teamcolors, by = c("Team" = )
## add_colors <- function(x){
##  tf.targets[tf.targets$playerID == x, "targets"] <- sum(targets$receiver_player_id == x)
## }
## tf.targets$playerID %>% lapply(add_targets)


g.targets <- ggplot(x.targets, aes(x = reorder(name, -targets), y = targets)) +
  geom_bar(stat ="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap( ~ Team, ncol = 32/4, scales = "free_x")

ggplot(x.players %>% filter(targets > 10), aes(x = reorder(name, -targets), y = targets)) +
  geom_bar(stat ="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap( ~ Team, ncol = 32/4, scales = "free_x")


targetplot <- x.players %>%
  filter(targets > 10) %>%
  arrange(desc(targets)) %>% 
  arrange(Team) %>% 
  ## mutate(name = reorder_within(name, targets, Team, sep = "")) %>%
  ggplot() +
  theme_bw() +
  scale_color_manual(values = nfl_teamcolors$primary) +
  scale_fill_manual(values = alpha(nfl_teamcolors$primary, 0.2)) +
  geom_bar(aes(reorder_within(name, -targets, Team), targets, colour = Team,  fill = Team), stat ="identity") +
  ## scale_x_reordered() +
  ##scale_color_teams(2, guide = FALSE) + 
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  facet_wrap( ~ Team, ncol = 32/4, scales = "free_x") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "2019 targets by team", 
       subtitle = "Minimum 10 targets") +
  xlab("Targeted players") +
  ylab("Targets")

ggsave("targets01.png", targetplot, width = 15, height = 10, dpi = 100, limitsize = FALSE)

x.targets %>% 
  ## mutate(Team = as.factor(Team),
  ##       name = reorder(name, -targets)) %>% 
  ggplot() +
  geom_bar(aes(name, targets), stat ="identity") +
  facet_wrap( ~ Team, ncol = 32/4, scales = "free_x") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("Targeted players") +
  ylab("Targets")

x.targets %>%
  filter(targets > 10) %>% 
  arrange(targets) %>%
  ## mutate(name = factor(name, levels=targets)) %>%
  ggplot(aes(x = reorder(name, targets), y = targets)) +
  geom_bar(stat ="identity") +
  coord_flip() +
  ## theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap( ~ Team, ncol = 32/4, scales = "free_y") +
  theme_bw() +
  xlab("")