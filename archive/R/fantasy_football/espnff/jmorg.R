# jmorg code --------------------------------------------------------------

#First Download playbyplay data...

#Create new variable. LOS_AIR is where pass ends up on field, 0 is saftey, 100 is touchdown 
NFL_pbp2019 <- NFL_pbp2019 %>% mutate(LOS = 100 - yardline_100, LOS_AIR = (100-yardline_100) + air_yards )

# I didnt like the NFLFastR td_prob, so I made my own
# I think using XYAC here may be a good idea...
tdprob<-glm(as.factor(touchdown) ~  LOS_AIR + cp,
            data = NFL_pbp2019, family = "binomial")

#using the logistic regression model to put my own TDprob into PBP data
NFL_pbp2019$tdprob2<-predict(tdprob,NFL_pbp2019, type = "response")

#Creating EX_FP, withing the PBP data
NFL_pbp2019 <- NFL_pbp2019 %>% mutate(Ex_FP = cp + cp * air_yards * .1 + cp * xyac_median_yardage * .1 + cp * tdprob2 * 6  )




# Functions ---------------------------------------------------------------

TeamRecords = 
  tibble(
    location = ESPNFromJSON$teams$location,
    nickname = ESPNFromJSON$teams$nickname,
    abbr = ESPNFromJSON$teams$abbrev,
    teamId = ESPNFromJSON$teams$id,
    wins = ESPNFromJSON$teams$record$overall$wins,
    losses = ESPNFromJSON$teams$record$overall$losses,
    points_for = ESPNFromJSON$teams$record$overall$pointsFor,
    points_against = ESPNFromJSON$teams$record$overall$pointsAgainst,
    seed = ESPNFromJSON$teams$playoffSeed,
    rank = ESPNFromJSON$teams$rankCalculatedFinal
  ) %>% 
  mutate(Percent = wins/ (wins+losses),
         Division = case_when(teamId %in% c(1,3,4,6,9) ~ 'Nick Division',
                              TRUE ~ 'Non-Nick Division')) %>% 
  unite(Team,c(location, nickname), sep=" ") %>% 
  arrange(Division, desc(Percent)) %>% 
  select(Team,Division,teamId, wins,losses,Percent,points_for,points_against,seed,rank)

TeamRecords = TeamRecords %>% 
  mutate(Manager = case_when(
    Team == 'Gould is Gold' ~ 'Nick Hawkes',
    Team == 'Brookline Satanic Turkeys' ~ 'Kenneth Amaya',
    Team == 'Appalachian BeerMen' ~ 'Josh Sexton',
    Team == 'Luck of Lucy' ~ 'Derrick Parham',
    Team == 'Always Filling RXes' ~ 'Michael Alexander',
    Team == 'Dont Touch My Butker' ~ 'Ethan Brown',
    Team == 'Calcified Kankers' ~ 'Dominic Decono',
    Team == 'Hollywoo Stars n Celebs' ~ 'Matt Betz',
    Team == 'Potomac P.I.\'s' ~ 'Nick Slawson',
    str_detect(Team,'Arizona Fear') ~ 'Nick Van Dyke'
  )) %>% 
  select(3,11,2,1,4:10) %>% 
  arrange(rank)

# TeamRecords2 = TeamRecords %>% 
#   select(Manager,Division,Team) %>% 
#   rename(Owner = Manager)

write.csv(TeamRecords,'teams.csv',row.names=FALSE)

Schedule =
  tibble(
    Winner = ESPNFromJSON$schedule$winner,
    Week = ESPNFromJSON$schedule$matchupPeriodId,
    AwayTeam = ESPNFromJSON$schedule$away$teamId,
    AwayPoints = ESPNFromJSON$schedule$away$totalPoints,
    HomeTeam = ESPNFromJSON$schedule$home$teamId,
    HomePoints = ESPNFromJSON$schedule$home$totalPoints
  ) %>%
  left_join(TeamRecords %>% select(teamId, Manager), by = c("AwayTeam" = "teamId")) %>%
  select(-AwayTeam) %>%
  rename(AwayTeam = Manager) %>%
  left_join(TeamRecords %>% select(teamId, Manager), by = c("HomeTeam" = "teamId")) %>%
  select(-HomeTeam) %>%
  rename(HomeTeam = Manager) %>% 
  mutate(Bracket = c(rep(NA,65),1:2),
         Week = as.character(Week),
         Week = as.character(case_when(Week == '15' ~ 'Playoffs1',
                                       Week == '16' ~ 'Playoffs2',
                                       TRUE ~ Week)))

Schedule2 = rbind(Schedule %>% mutate(Manager = AwayTeam,
                                      GameCode = c(300:(299+nrow(Schedule)))),
                  (Schedule %>% mutate(Manager = HomeTeam,
                                       GameCode = c(300:(299+nrow(Schedule)))))) %>% 
  arrange(Week,GameCode) %>% 
  mutate(Location = case_when(Manager == AwayTeam ~ 'Away',
                              TRUE ~ 'Home'),
         TeamScore = case_when(Location == 'Away' ~ AwayPoints,
                               TRUE ~ HomePoints),
         OpponentScore = case_when(Location == 'Away' ~ HomePoints,
                                   TRUE ~ AwayPoints),
         Outcome = case_when(TeamScore > OpponentScore ~ 'Win',
                             OpponentScore > TeamScore ~ 'Lose',
                             TRUE ~ 'Tie'),
         Season = case_when(str_detect(Week,'Playoffs')==TRUE ~ 'Playoffs',
                            TRUE ~ 'Regular'),
         GameCode = case_when(Week =='Playoffs1' ~ as.integer(GameCode + 10),
                              Week == 'Playoffs2' ~ as.integer(GameCode + 20),
                              TRUE ~ as.integer(GameCode)))

# Schedule3 = as_tibble(sqldf(
#   'select s2.*
#     , s.Manager as Opponent
#   from Schedule2 s2
#   left join Schedule2 s
#   on s2.GameCode=s.GameCode
#   and s2.Manager!=s.Manager'
# ) %>% 
#   mutate(Year = 2019) %>% 
#   select(GameCode,Year,Week,Manager,Opponent,Location,Outcome,TeamScore,OpponentScore,Season,Bracket))

#####################
#####################
#####################
weeks=16
# leagueID = "1000432"
year = "2019"

PositionDF =
  tibble(
    PositionId = c(1, 2, 3, 4, 5, 16),
    Position = c(
      "Quarterback",
      "Running Back",
      "Wide Receiver",
      "Tight End",
      "Kicker",
      "Defense"
    )
  )

PlayerSlotIDs = tibble(
  playerrosterslot = c(0, 2, 4, 6, 16, 17, 20, 21, 23),
  SlottedPosition = c(
    "Quarterback",
    "Running Back",
    "Wide Receiver",
    "Tight End",
    "Defense",
    "Kicker",
    "Bench",
    "IR",
    "Flex"
  )
)

teamiddf = TeamRecords %>% 
  select(teamId, Manager, Team, Division) %>% 
  arrange(teamId)

gofunction = function(weeks = 16, leagueID = leagueID, year = year){
  
  playerperformance = NULL
  
  for (i in 1:weeks) {
    base = "http://fantasy.espn.com/apis/v3/games/ffl/seasons/"
    mid = "/segments/0/leagues/"
    tail = "?view=mMatchup&view=mMatchupScore&scoringPeriodId="
    url = paste0(base, year, mid, leagueID, tail, i)
    
    ESPNGet = httr::GET(url=url)
    ESPNGet$status_code
    ESPNRaw = rawToChar(ESPNGet$content)
    ESPNFromJSON = jsonlite::fromJSON(ESPNRaw)
    
    players =
      ESPNFromJSON$teams$roster$entries %>% 
      map("playerPoolEntry") %>% 
      map('player') %>% 
      map_df(magrittr::extract, c('id','fullName','defaultPositionId')) %>% 
      mutate(id = as.character(id))
    
    observations =
      ESPNFromJSON$teams$roster$entries %>% map("playerPoolEntry") %>% map("player") %>% map("stats")  %>%
      flatten() %>% map_df( ~ count(.))
    
    playervec =
      players %>%
      mutate(observations = observations$n) %>%
      uncount(observations)
    
    
    ## projections and results for players withnames
    playerperformanceshort =
      ESPNFromJSON$teams$roster$entries %>% map("playerPoolEntry") %>% map("player") %>% map("stats")  %>%
      flatten() %>%
      map_df(
        magrittr::extract,
        c(
          "scoringPeriodId",
          "seasonId",
          "statSourceId",
          "statSplitTypeId",
          "id",
          "externalId",
          "appliedTotal"
        )
      ) %>%
      mutate(Player = playervec$fullName) %>%
      mutate(PositionId = playervec$defaultPositionId) %>%
      left_join(PositionDF) %>%
      mutate(iteration = i)
    
    playerperformance = as_tibble(bind_rows(playerperformance, playerperformanceshort))
  }
  
  playerperformance =
    playerperformance %>%
    select(-iteration, -seasonId) %>%
    distinct() %>% 
    filter(statSplitTypeId != 2) %>% 
    mutate(ScoringType = case_when(scoringPeriodId > 0 & statSourceId == 1 ~ 'Predicted',
                                   scoringPeriodId > 0 & statSourceId == 0 ~ 'Actual',
                                   scoringPeriodId == 0 & statSourceId == 0 & statSplitTypeId == 0 ~ 'Season_Total',
                                   scoringPeriodId == 0 & statSourceId == 1 & statSplitTypeId == 0 ~ 'Preseason_Prediction',
                                   TRUE ~ as.character(NA)))
  
  write.csv(playerperformance,paste0(year,'_PlayerScores.csv'),row.names=FALSE)
  ## gets team names and records
  PlayerTeamDF = NULL
  PlayerTeamDf = data.frame(Team = vector(),
                            Player = vector(),
                            playersrosterslot = vector(),
                            scoringPeriodId = vector())
  
  for (i in 1:weeks) {
    base = "http://fantasy.espn.com/apis/v3/games/ffl/seasons/"
    year = year
    mid = "/segments/0/leagues/"
    leagueID = leagueID
    tail10 = "?view=mMatchup&view=mMatchupScore&scoringPeriodId="
    tail = "?view=mDraftDetail&view=mLiveScoring&view=mMatchupScore&view=mPendingTransactions&view=mPositionalRatings&view=mSettings&view=mTeam&view=modular&view=mNav&view=mMatchupScore&scoringPeriodId="
    url = paste0(base, year, mid, leagueID, tail, i)
    url10 = paste0(base, year, mid, leagueID, tail10, i)
    
    ESPNGet <- httr::GET(url = url,
                         config = httr::config(cookie = cookie))
    ESPNGet$status_code
    ESPNRaw <- rawToChar(ESPNGet$content)
    ESPNFromJSON2 <- jsonlite::fromJSON(ESPNRaw)
    # ESPNFromJSON2 %>% listviewer::jsonedit()
    
    Sys.sleep(time = runif(1, 2, 4))
    
    ESPNGet10 <- httr::GET(url = url10,
                           config = httr::config(cookie = cookie))
    ESPNGet10$status_code
    ESPNRaw10 <- rawToChar(ESPNGet10$content)
    ESPNFromJSON10 <- jsonlite::fromJSON(ESPNRaw10)
    # ESPNFromJSON10 %>% listviewer::jsonedit()
    
    playerrosterslot =
      ESPNFromJSON10$teams$roster$entries %>%
      map_df(`[`, "lineupSlotId")
    
    assignedpositions =
      ESPNFromJSON10$teams$roster$entries %>%
      map("playerPoolEntry") %>% map("player") %>%
      map_df(magrittr::extract, c("id", "fullName", "defaultPositionId"))
    
    TeamPlayers =
      ESPNFromJSON10$teams$roster$entries %>% map("playerPoolEntry") %>%
      map_df( ~ count(.))
    
    PlayerTeamDFshort =
      ESPNFromJSON2$teams %>% select(location, nickname, id) %>%
      unite(Team, c(location, nickname),sep=' ') %>%
      mutate(TeamPlayers = TeamPlayers$n) %>%
      uncount(TeamPlayers) %>%
      mutate(Player = assignedpositions$fullName) %>%
      select(-id) %>%
      mutate(playerrosterslot = playerrosterslot$lineupSlotId) %>%
      mutate(scoringPeriodId = i)
    
    PlayerTeamDF = bind_rows(PlayerTeamDF, PlayerTeamDFshort)
  }
  
  ## adds team info to player dataframe
  
  PlayerPerformance =
    playerperformance %>%
    left_join(PlayerTeamDF, by = c("Player", "scoringPeriodId")) %>%
    as_tibble()
  
  WeeklyEstimates =
    PlayerPerformance %>% 
    # filter(Team == "'R'm Chair_Quarterback") %>%
    filter(externalId != as.numeric(year)*10) %>%
    #mutate(statSourceId = if_else(statSourceId == 1, "Predicted", "Actual")) %>%
    select(
      scoringPeriodId,
      ScoringType,
      appliedTotal,
      Player,
      Position,
      Team,
      playerrosterslot
    ) %>%
    spread(ScoringType, appliedTotal) %>%
    arrange(Player) %>%
    mutate(Actual = case_when(is.na(Actual) ~ 0,
                              TRUE ~ Actual),
           ActualMinusPredicted = Actual - Predicted) %>%
    group_by(Player) %>% 
    mutate(Preseason_Prediction = max(Preseason_Prediction,na.rm=TRUE),
           Season_Total = max(Season_Total,na.rm=TRUE)) %>% 
    ungroup() %>% 
    left_join(PlayerSlotIDs) %>%
    # filter(scoringPeriodId==1)
    # select(-playerrosterslot) %>%
    mutate(Starter = if_else(SlottedPosition %in% c("Bench", "IR"), "Bench", "Starter"))
  
  #write_csv(WeeklyEstimates, "FantasyFootballData.csv")
  
  base = "http://fantasy.espn.com/apis/v3/games/ffl/seasons/"
  year = year
  mid = "/segments/0/leagues/"
  leagueID = leagueID
  tail = "?&view=mMatchupScore&scoringPeriodId="
  url = paste0(base,year,mid,leagueID,tail)
  
  ESPNGet <- httr::GET(url = url)
  ESPNGet$status_code
  ESPNRaw <- rawToChar(ESPNGet$content)
  ESPNFromJSON2 <- jsonlite::fromJSON(ESPNRaw)
  # ESPNFromJSON2 %>% listviewer::jsonedit()
  
  season1 = tibble(
    awayid = ESPNFromJSON2$schedule$away$teamId,
    awaypoints = ESPNFromJSON2$schedule$away$totalPoints,
    homeid = ESPNFromJSON2$schedule$home$teamId,
    homepoints = ESPNFromJSON2$schedule$home$totalPoints,
    winner = ESPNFromJSON2$schedule$winner,
    weekID = ESPNFromJSON2$schedule$matchupPeriodId
  ) %>%
    left_join((teamiddf %>% select(-Division)), by = c("awayid"="teamId")) %>%
    rename(AwayTeam = Team) %>%
    left_join((teamiddf %>% select(-Division)), by = c("homeid"="teamId")) %>%
    rename(HomeTeam = Team) %>%
    mutate(winner = if_else(awaypoints>homepoints,AwayTeam,HomeTeam))
  
  season =
    season1 %>% select(-awayid,-homeid) %>%
    gather(Location, Points, -winner, -weekID, -AwayTeam,-HomeTeam) %>%
    mutate(Points = as.numeric(Points)) %>%
    filter(!is.na(Points)) %>% 
    arrange(weekID) %>%
    group_by(weekID) %>%
    mutate(rank = rank(-Points)) %>%
    mutate(EWETL = rank-1) %>%
    mutate(EWETW = 10-rank) %>%
    group_by()
  
  #season %>%
  #  write_csv("weekbyweekresults.csv")
  
  #season1 %>%
  #  write_csv("weekbyweekresultssimple.csv")
  
}

gofunction(weeks = 17, leagueID = leagueID, year = year)

flex = c('Tight End','Running Back','Wide Receiver')
View(test %>% arrange(scoringPeriodId) %>% filter(Team == 'Luck of Lucy' & scoringPeriodId == 5))

WeeklyEstimates %>% 
  filter(scoringPeriodId > 0) %>% 
  group_by(scoringPeriodId,Team,Position) %>% 
  mutate(position_team_rank = rank(-Actual,ties.method = 'random'), 
         select_flag = case_when(Position == 'Quarterback' & position_team_rank == 1 ~ 1,
                                 Position == 'Defense' & position_team_rank == 1 ~ 1,
                                 Position == 'Kicker' & position_team_rank == 1 ~ 1,
                                 Position == 'Wide Receiver' & position_team_rank <= 2 ~ 1,
                                 Position == 'Running Back' & position_team_rank <= 2 ~ 1,
                                 Position == 'Tight End' & position_team_rank == 1 ~ 1,
                                 TRUE ~ 0)) %>% 
  ungroup() %>% 
  group_by(scoringPeriodId,Team) %>% 
  mutate(flex_flag = max(case_when(!is.na(Team) & select_flag == 0 & Position %in% flex ~ Actual),na.rm=TRUE),
         flex_flag = case_when(flex_flag == Actual))
select_flag = case_when(select_flag == 0 & flex_flag ==  ~ 1,) %>% arrange(scoringPeriodId,Team,desc(select_flag),desc(Actual)) %>% View()

test %>% filter(select_flag == 0 & flex_flag == Actual) %>% group_by(scoringPeriodId,Team) %>% summarise(rows = n()) %>% View()


TeamScore = sum(Actual)
Var = sum(ActualMinusPredicted) %>%
  ungroup() %>% 
  group_by(scoringPeriodId,Team) %>% 
  mutate(TotalScore = sum(TeamScore),
         TotaVar = sum(Var))

arrange(scoringPeriodId,Team,Starter) %>% View()


Schedule3
TeamRecords
PlayerTeamDF
playerperformance
WeeklyEstimates
season1
season

###HTML Documents
WeeklyEstimates = read_csv("FantasyFootballData.csv")
WeeklyEstimates$Team  %>% unique() %>% na.omit() -> teamlist
currentweek = 13
for (i in 1:10) {
  rmarkdown::render("Fantasy Football Team Report.Rmd",params=list(team=teamlist[i],week = currentweek))
  file.rename(from="Fantasy-Football-Team-Report.html", to =paste0(teamlist[i],"_Update.html"))
  file.copy(from=paste0(getwd(),"/",teamlist[i],"_Update.html"), to = paste0(getwd(),"/FF Update Reports/",teamlist[i],"_Update.html"), overwrite = TRUE)
  file.remove(paste0(getwd(),"/",teamlist[i],"_Update.html"))
}