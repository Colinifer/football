### This entire code will take days to run

#### pull from PFF 
def_list <- lapply(c(1996,1998,2001,2003:2004,2006:2019), function(yr) {
  def_json <- fromJSON(paste0('https://premium.pff.com/api/v1/facet/defense/summary?league=nfl&season=',yr))
  all_col_names <- unique(unlist(lapply(def_json$defense_stats, function(x) names(unlist(x)))))
  def_df <- data.frame(t(sapply(def_json$defense_stats, function(x) unlist(x)[paste0(all_col_names)])))
  def_df$year <- yr
  return(def_df)
})
saveRDS(do.call(rbind, def_list), 'data/players/pff/defensive_players.rds')

off_list <- lapply(c(1996,1998,2001,2003:2004,2006:2019), function(yr) {
  off_json <- RJSONIO::fromJSON(paste0('https://premium.pff.com/api/v1/facet/offense/summary?league=nfl&season=',yr))
  all_col_names <- unique(unlist(lapply(off_json$offense_stats, function(x) names(unlist(x)))))
  off_df <- data.frame(t(sapply(off_json$offense_stats, function(x) unlist(x)[paste0(all_col_names)])))
  off_df$year <- yr
  return(off_df)
})
saveRDS(do.call(rbind, off_list), 'data/players/pff/offensive_players.rds')

st_list <- lapply(2013:2019, function(yr) {
  st_json <- RJSONIO::fromJSON(paste0('https://premium.pff.com/api/v1/facet/special/summary?league=nfl&season=',yr))
  all_col_names <- unique(unlist(lapply(st_json$special_teams_stats, function(x) names(unlist(x)))))
  st_df <- data.frame(t(sapply(st_json$special_teams_stats, function(x) unlist(x)[paste0(all_col_names)])))
  st_df$year <- yr
  return(st_df)
})
saveRDS(do.call(rbind, st_list), 'data/players/pff/specialteams_players.rds')

#######################################

###get all files back out and get a list of unique IDs
pff_spec_df <- readRDS('data/players/pff/specialteams_players.rds')
pff_def_df <- readRDS('data/players/pff/defensive_players.rds')
pff_off_df <- readRDS('data/players/pff/offensive_players.rds')

all_player_id <- data.frame(
  'player_id' = c(
    pff_spec_df$player_id,
    pff_def_df$player_id,
    pff_off_df$player_id
  ) %>%
    unique()
) %>%
  as_tibble()

###find players without a json and loop to pull from API
players_to_check <- all_player_id %>%
  filter(!player_id %in% gsub('.json', '', dir('data/players/pff/player_info/json'))) %>% 
  pull(player_id)

for (j in players_to_check) {
  start_time <- Sys.time()
  RJSONIO::fromJSON(glue('https://premium.pff.com/api/v1/players?league=nfl&id={j}')) %>% 
    RJSONIO::toJSON() %>% 
    write(glue('data/players/pff/player_info/json/{j}.json'))
  end_time <- Sys.time()
  print(end_time - start_time)
}

###go through all json files and put a df together
all_json <- dir('data/players/pff/player_info/json', full=T)
all_cols <- c("id", "first_name", "last_name", "jersey_number", "height", "weight",
              "dob", "speed", "college", "current_eligible_year", "position")

pff_players <- purrr::map_df(all_json, function(x) {
  # start_time <- Sys.time()
  # print(x)
  player_json <- jsonlite::fromJSON(x)[[1]]
  player_df <- c(player_json %>% select(all_cols),
    'team'=player_json$team$abbreviation,
    'draft_team'=player_json$draft$team$abbreviation,
    'draft_yr'=player_json$draft$season,
    'draft_rnd'=player_json$draft$round,
    'draft_pick'=player_json$draft$selection,
    'draft_type'=player_json$draft$type
  ) %>% as_tibble()
  # end_time <- Sys.time()
  # print(end_time - start_time)
  return(player_df)
}) %>% 
  mutate(
    height = floor(height / 100) * 12 + height %% 12,
    dob = dob %>% as.Date(),
    ball_side = case_when(position %in% c('C','FB','G','HB','QB','T','TE','WR') ~ 'offense',
                          position %in% c('CB','DI','ED','LB','S') ~ 'defense',
                          TRUE ~ 'special')
  )

###save RDS with all the PFF players
saveRDS(pff_players, 'data/players/pff/pff_players.rds')

#######################################

### look to see which seasons the player was active in
### could also just look in PFF csvs pulled above
pff_players <- readRDS('data/players/pff/pff_players.rds')

### This may crash several times while running
### Just rerun from the players_to_check_row to see what is missing and resume
players_to_check_row <- pff_players %>%
  filter(!id %in% gsub('.json', '', dir(
    'data/players/pff/pff_player_seasons/json'
  ))) %>%
  pull(id)

# Don't think this is pulling correct IDs, it's overwriting existing json files

for (j in players_to_check_row) {
  start_time <- Sys.time()
  RJSONIO::fromJSON(glue('https://premium.pff.com/api/v1/player/{pff_players %>% pull(ball_side) %>% nth(j)}/summary?league=nfl&career=true&player_id={pff_players %>% pull(id) %>% nth(j)}')) %>% 
    RJSONIO::toJSON() %>% 
    write(glue('data/players/pff/pff_player_seasons/json/{pff_players %>% pull(id) %>% nth(j)}.json'))
  end_time <- Sys.time()
  print(glue('{pff_players %>% pull(id) %>% nth(j)}, {end_time - start_time}'))
}

### get all the JSON files and save them in a columnwise format
all_json <- dir('data/players/pff/pff_player_seasons/json', full=T)

player_season_df <- map_df(all_json, function(x) {
  print(x)
  json <- jsonlite::fromJSON(x)[[1]]
  franchise <- json$seasons$franchise_id
  seasons <- json$seasons$season
  if (length(franchise) == 0) {
    franchise <- NA
  }
  if (length(seasons) == 0) {
    seasons <- NA
  }
  team_season <-
    do.call(rbind, lapply(1:length(seasons), function(z)
      cbind('season' = seasons[[z]], 'franchise' = franchise[[z]]))) %>%
    as_tibble() %>%
    mutate(id = fromJSON(x)[[1]]$subject$player_id) %>%
    relocate(id)
  return(team_season)
})

row.names(player_season_df) <- NULL
player_season_df %>% 
  saveRDS('data/players/pff/pff_player_seasons.rds')

#######################################

### get the last two files back out
pff_players <- readRDS('data/players/pff/pff_players.rds')
player_season_df <- readRDS('data/players/pff/pff_player_seasons.rds')

player_season_df <- pff_players %>%
  left_join(player_season_df, by = 'id') %>%
  filter(season >= 2006)

### This may crash several times while running
### Just rerun from players_complete to see what is missing and resume
### players complete will see what is currently saved, players_to_check_row will loop on everything else
players_complete <- gsub('data/players/pff/game_status/json', '', unlist(sapply(dir('data/players/pff/game_status/json', full = T), function(yr)
  gsub('.json', '', dir(yr, full = T)))))

players_to_check_row <- which(!paste0(player_season_df$season,'/',player_season_df$id) %in% players_complete)


for (j in players_to_check_row) {
  RJSONIO::fromJSON(glue('https://premium.pff.com/api/v1/player/{player_season_df %>% pull(ball_side) %>% nth(j)}/summary?league=nfl&season={player_season_df %>% pull(season) %>% nth(j)}&player_id={player_season_df %>% pull(id) %>% nth(j)}')) %>% 
    RJSONIO::toJSON() %>% 
    write(glue('data/players/pff/game_status/json/{player_season_df %>% pull(season) %>% nth(j)}/{player_season_df %>% pull(id) %>% nth(j)}.json'))
}


### get all the completed JSONs and save in columnwise format
all_json <- unlist(sapply(dir('data/players/pff/game_status/json', full = T), function(yr) dir(yr, full = T)))
names(all_json) <- NULL

player_games_df <- map_df(all_json, function(x) {
  pl_json <- RJSONIO::fromJSON(x)[[1]]
  subject_info <- c('player_id'=pl_json$subject$player_id,
                    'season'=pl_json$subject$season,
                    'eligible'=pl_json$subject$eligible,
                    'roster'=pl_json$subject$roster,
                    'secondary'=pl_json$subject$secondary)
  games_df <- data.frame(t(sapply(pl_json$weeks, function(wk) c(wk$game, 'jersey_number'=wk$jersey_number))))
  return(cbind(rbind(subject_info), games_df, row.names = NULL))
})

player_games_df <- map_df(all_json, function(x) {
  pl_json <- RJSONIO::fromJSON(x)[[1]]
  subject_info <- c('player_id'=pl_json$subject$player_id,
                  'season'=pl_json$subject$season,
                  'eligible'=pl_json$subject$eligible,
                  'roster'=pl_json$subject$roster,
                  'secondary'=pl_json$subject$secondary)
  games_df <- data.frame(t(sapply(pl_json$weeks, function(wk) c(wk$game, 'jersey_number'=wk$jersey_number))))
  return(cbind(rbind(subject_info), games_df, row.names = NULL))
})

### remove NULLs
for (z in 1:ncol(player_games_df)) player_games_df[,z] <- sapply(player_games_df[,z], function(y) ifelse(is.null(y), NA, y))

saveRDS(player_games_df, 'player games.rds')

#######################################

### Try to join with nflfastR rosters
roster_df <- readRDS('C:/Users/Owner/Documents/nflfastR-data/roster.rds')
player_games_df <- readRDS('player games.rds')
player_df <- readRDS('basic player info.rds')

franchise_vec <- c('ARI', 'ATL', 'BAL', 'BUF', 'CAR', 'CHI', 'CIN', 'CLE', 'DAL', 'DEN',
                   'DET', 'GB', 'HOU', 'IND', 'JAX', 'KC', 'MIA', 'MIN', 'NE', 'NO',
                   'NYG', 'NYJ', 'OAK', 'PHI', 'PIT', 'LA', 'LAC', 'SF', 'SEA', 'TB',
                   'TEN', 'WAS')

main_pff_df <- player_games_df %>% 
  mutate(player_id = as.numeric(player_id),
         season = as.numeric(season),
         jersey_number = as.numeric(jersey_number)
  ) %>% 
  left_join(player_df, by = c('player_id'='id'), suffix = c('_game','_career')) %>% 
  select(player_id, season, dob, first_name, last_name, jersey_number_game, position_career, player_franchise_id) %>% 
  distinct() %>% 
  mutate(
    name_begin = substr(first_name,1,1),
    nflfastR_tm_code = franchise_vec[player_franchise_id],
    nflfastR_tm_code = case_when(nflfastR_tm_code == 'LA' & season <= 2015 ~ 'STL',
                                nflfastR_tm_code == 'LAC' & season <= 2016 ~ 'SD',
                                nflfastR_tm_code == 'OAK' & season >= 2019 ~ 'LV',
                                TRUE ~ nflfastR_tm_code
    )
  )

main_pff_df$last_name_no_suff <- main_pff_df$last_name
for (x in c('Sr.','Jr.','III','II','IV','VII','VI')) {
  main_pff_df$last_name_no_suff <- gsub(paste0(' ',x),'',main_pff_df$last_name_no_suff)
}

gsis_pff_df <- main_pff_df %>%
  inner_join(roster_df %>% mutate(teamPlayers.birthDate = as.Date(teamPlayers.birthDate, format = '%m/%d/%Y')), by = c(
                      'season'='team.season',
                      'nflfastR_tm_code'='team.abbr',
                      'jersey_number_game'='teamPlayers.jerseyNumber',
                      'last_name'='teamPlayers.lastName',
                      'dob'='teamPlayers.birthDate'
    )
  ) %>% 
  select(teamPlayers.gsisId, player_id) %>% 
  distinct() %>% 
  filter(!is.na(player_id) & !is.na(teamPlayers.gsisId))

table(table(gsis_pff_df$teamPlayers.gsisId))
table(table(gsis_pff_df$player_id))

gsis_pff_df <- main_pff_df %>%
  inner_join(roster_df %>% mutate(teamPlayers.birthDate = as.Date(teamPlayers.birthDate, format = '%m/%d/%Y')), by = c(
    'season'='team.season',
    'nflfastR_tm_code'='team.abbr',
    'jersey_number_game'='teamPlayers.jerseyNumber',
    'last_name'='teamPlayers.lastName',
    'first_name'='teamPlayers.firstName'
  )
  ) %>% 
  anti_join(gsis_pff_df, by = c('player_id')) %>%
  anti_join(gsis_pff_df, by = c('teamPlayers.gsisId')) %>%
  select(teamPlayers.gsisId, player_id) %>% 
  distinct() %>% 
  filter(!is.na(player_id) & !is.na(teamPlayers.gsisId)) %>% 
  rbind(gsis_pff_df)

table(table(gsis_pff_df$teamPlayers.gsisId))
table(table(gsis_pff_df$player_id))

gsis_pff_df <- main_pff_df %>%
  inner_join(roster_df %>% mutate(teamPlayers.birthDate = as.Date(teamPlayers.birthDate, format = '%m/%d/%Y')), by = c(
    'season'='team.season',
    'nflfastR_tm_code'='team.abbr',
    'last_name_no_suff'='teamPlayers.lastName',
    'first_name'='teamPlayers.firstName',
    'dob'='teamPlayers.birthDate'
  )
  ) %>% 
  anti_join(gsis_pff_df, by = c('player_id')) %>%
  anti_join(gsis_pff_df, by = c('teamPlayers.gsisId')) %>%
  select(teamPlayers.gsisId, player_id) %>% 
  distinct() %>% 
  filter(!is.na(player_id) & !is.na(teamPlayers.gsisId)) %>% 
  rbind(gsis_pff_df)

table(table(gsis_pff_df$teamPlayers.gsisId))
table(table(gsis_pff_df$player_id))

gsis_pff_df <- main_pff_df %>%
  inner_join(roster_df %>% mutate(teamPlayers.birthDate = as.Date(teamPlayers.birthDate, format = '%m/%d/%Y')), by = c(
    'last_name_no_suff'='teamPlayers.lastName',
    'first_name'='teamPlayers.firstName',
    'dob'='teamPlayers.birthDate'
  )
  ) %>% 
  anti_join(gsis_pff_df, by = c('player_id')) %>%
  anti_join(gsis_pff_df, by = c('teamPlayers.gsisId')) %>%
  select(teamPlayers.gsisId, player_id) %>% 
  distinct() %>% 
  filter(!is.na(player_id) & !is.na(teamPlayers.gsisId)) %>% 
  rbind(gsis_pff_df)

table(table(gsis_pff_df$teamPlayers.gsisId))
table(table(gsis_pff_df$player_id))

gsis_pff_df <- main_pff_df %>%
  inner_join(roster_df %>% mutate(teamPlayers.birthDate = as.Date(teamPlayers.birthDate, format = '%m/%d/%Y')), by = c(
    'season'='team.season',
    'nflfastR_tm_code'='team.abbr',
    'jersey_number_game'='teamPlayers.jerseyNumber',
    'last_name_no_suff'='teamPlayers.lastName'
  )
  ) %>% 
  anti_join(gsis_pff_df, by = c('player_id')) %>%
  anti_join(gsis_pff_df, by = c('teamPlayers.gsisId')) %>%
  select(teamPlayers.gsisId, player_id) %>% 
  distinct() %>% 
  filter(!is.na(player_id) & !is.na(teamPlayers.gsisId)) %>% 
  rbind(gsis_pff_df)

table(table(gsis_pff_df$teamPlayers.gsisId))
table(table(gsis_pff_df$player_id))


gsis_pff_df <- main_pff_df %>%
  inner_join(roster_df %>% mutate(first_letter = substr(teamPlayers.firstName,1,1)), by = c(
    'season'='team.season',
    'nflfastR_tm_code'='team.abbr',
    'last_name_no_suff'='teamPlayers.lastName',
    'name_begin'='first_letter'
  )
  ) %>% 
  anti_join(gsis_pff_df, by = c('player_id')) %>%
  anti_join(gsis_pff_df, by = c('teamPlayers.gsisId')) %>%
  select(teamPlayers.gsisId, player_id) %>% 
  distinct() %>% 
  filter(!is.na(player_id) & !is.na(teamPlayers.gsisId)) %>% 
  rbind(gsis_pff_df)

table(table(gsis_pff_df$teamPlayers.gsisId))
table(table(gsis_pff_df$player_id))


player_df <- player_df %>% 
  left_join(gsis_pff_df, by = c('id'='player_id')) %>% 
  rename(gsis_id = teamPlayers.gsisId)

saveRDS(player_df, 'C:/Users/Owner/Documents/GitHub/NFL/NFL/pff player info.rds')


#######################################

### start going after player grades
### get the player season out
player_season_df <- readRDS('player seasons.rds') %>% 
  filter(!is.na(season))
done_begin <- 0

### This may crash several times while running
### Just rerun from players_complete to see what is missing and resume
### players complete will see what is currently saved, players_to_check_row will loop on everything else
players_complete <- gsub('grades/','',unlist(sapply(dir('grades', full = T), function(yr) gsub('.json','',dir(yr, full = T)))))
players_to_check_row <- which(!paste0(player_season_df$season,'/',player_season_df$id) %in% players_complete)

completed_last_loop <- length(players_complete) - done_begin
(completed_last_loop / as.numeric(last_loop_tm[3])) * length(players_to_check_row) / 60 / 60

done_begin <- length(players_complete)
ptm <- proc.time()
for (j in players_to_check_row) {
  plr_json <- fromJSON(paste0('https://www.pff.com/api/players/',player_season_df$id[j],'/stats?season=',player_season_df$season[j],'&week_group=REG'))
  write(toJSON(plr_json), paste0('grades/',player_season_df$season[j],'/',player_season_df$id[j],'.json'))
}
last_loop_tm <- proc.time() - ptm

players_complete <- unlist(sapply(dir('grades', full = T), function(yr) dir(yr, full = T)))
grades_df <- data.frame(t(sapply(players_complete, function(x) {
  if(length(fromJSON(x)[[1]]) > 0) {
    return(c(unlist(strsplit(gsub('.json','',gsub('grades/','',x)),'/')),fromJSON(x)[[1]][[1]][1]))
  } else {
    return(c(unlist(strsplit(gsub('.json','',gsub('grades/','',x)),'/')),NA))
  }
  })), row.names = NULL)
names(grades_df)[1:3] <- c('season','id','grade')
for(j in 1:3) grades_df[,j] <- as.numeric(grades_df[,j])
saveRDS(grades_df, 'grades.rds')

### start going after player snaps
### get the player season out
player_games_df <- readRDS('player games.rds')
done_begin <- 0

### This may crash several times while running
### Just rerun from players_complete to see what is missing and resume
### players complete will see what is currently saved, players_to_check_row will loop on everything else
players_complete <- gsub('snaps/','',unlist(sapply(dir('snaps', full = T), function(yr) gsub('.json','',unlist(sapply(dir(yr, full = T), function(wk) dir(wk, full = T)))))))
players_to_check_row <- which(!paste0(player_games_df$season,'/',player_games_df$week,'/',player_games_df$player_id) %in% players_complete)

completed_last_loop <- length(players_complete) - done_begin
(as.numeric(last_loop_tm[3]) / completed_last_loop) * length(players_to_check_row) / 60 / 60

done_begin <- length(players_complete)
ptm <- proc.time()
for (j in players_to_check_row) {
  plr_json <- fromJSON(paste0('https://premium.pff.com/api/v1/player/snaps/summary?league=nfl&season=',player_games_df$season[j],'&player_id=',player_games_df$player_id[j],'&week=',player_games_df$week[j]))
  write(toJSON(plr_json), paste0('snaps/',player_games_df$season[j],'/',player_games_df$week[j],'/',player_games_df$player_id[j],'.json'))
}
last_loop_tm <- proc.time() - ptm

players_complete <- unlist(sapply(dir('snaps', full = T), function(yr) unlist(sapply(dir(yr, full = T), function(wk) dir(wk, full = T)))))
snap_df <- data.frame(t(sapply(players_complete, function(x) c(unlist(strsplit(gsub('.json','',gsub('snaps/','',x)),'/')),fromJSON(x)[[1]]$snap_counts))), row.names = NULL)
names(snap_df)[1:3] <- c('season','week','id')
for(j in 1:25) snaps_df[,j] <- as.numeric(snaps_df[,j])
saveRDS(snap_df, 'snaps.rds')


#######################################

#doubles <- names(which(table(gsis_pff_df$player_id)>1))
#gsis_id_doub <- gsis_pff_df$teamPlayers.gsisId[which(gsis_pff_df$player_id %in% doubles)]
#roster_df[which(roster_df$teamPlayers.gsisId %in% gsis_id_doub & roster_df$teamPlayers.status== 'ACT'),]
#pff_doub <- gsis_pff_df$teamPlayers.gsisId[which(gsis_pff_df$player_id %in% doubles)]
#main_pff_df[which(main_pff_df$player_id %in% doubles),]
#which(gsis_pff_df$player_id=='3840')

#str(gsis_pff_no_dob_df)
#str(gsis_pff_df)

#str(player_df)

#player_df[which(player_df$id=='3975'),]

#player_games_df[which(player_games_df$player_id=='3975' & player_games_df$season == 2007),]

### players listed post '06 that were active with no match
### 99.7% match rate for 2013 and later actives
#table(is.na(match(unique(roster_df$teamPlayers.gsisId[which(roster_df$team.season>=2013 & roster_df$teamPlayers.status== 'ACT')]),gsis_pff_df$teamPlayers.gsisId)))

#unique(roster_df$teamPlayers.gsisId[which(roster_df$team.season>=2013 & roster_df$teamPlayers.status== 'ACT')])[which(is.na(match(unique(roster_df$teamPlayers.gsisId[which(roster_df$team.season>=2013 & roster_df$teamPlayers.status== 'ACT')]),gsis_pff_df$teamPlayers.gsisId)))[3]]

#roster_df[which(roster_df$teamPlayers.gsisId=='00-0028073'),]

#player_df[which(player_df$last_name=='Ingram III'),]
#main_pff_df[which(main_pff_df$player_id=='7024'),]

#######################################
#Some helpful endpoints to remember

#shows team played for and years play in
##https://premium.pff.com/api/v1/player/offense/summary?league=nfl&career=true&player_id=5598

#weekly status by season & side of ball, may need to look at position to know which endpoint
##https://premium.pff.com/api/v1/player/offense/summary?league=nfl&season=2017&player_id=5598

#snap counts by week
##https://premium.pff.com/api/v1/player/snaps/summary?league=nfl&league=nfl&season=2017&player_id=5598&week=7

#player grades, week_group can be PRE/REG/POST
##https://www.pff.com/api/players/5598/stats?season=2017&week_group=REG'

#basic player info
##https://premium.pff.com/api/v1/players?league=nfl&id=5598

