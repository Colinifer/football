assign(paste(userYear, "players", sep = ""), read_csv(f.player_season))

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

g.targets <- ggplot(x.players, aes(x = name, y = targets)) +
  facet_grid(~Team)
