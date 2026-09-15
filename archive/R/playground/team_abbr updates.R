team_abbr <- read_csv("data/season_total/team_abbr.csv")

team_abbr <- dplyr::left_join(team_abbr, nflteams, by = "full_name")

team_abbr <- rename(team_abbr, c("wiki_logo"="X7"))

team_abbr$abbr <- NULL

colnames(team_abbr)

team_abbr %>% colnames() %>% length()

team_abbr
  

team_abbr <- team_abbr[,c(1:5,ncol(team_abbr),7:ncol(team_abbr)-1)]
