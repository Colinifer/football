xtargets <- as_tibble(xplayers)
xtargets

xtargets <- as_tibble(xtargets)

xtargets <- filter(xtargets, targets >= 1)

xtargets %>% select(Team, name, targets, recyds) %>% arrange(Team, desc(targets), desc(recyds))

players xplayers
