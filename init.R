# Packages & Init Setup ---------------------------------------------------

# devtools::install_github("nflverse/nflfastR")
# devtools::install_github("ffverse/ffopportunity")
# devtools::install_github("dynastyprocess/ffpros")
# devtools::install_github("guga31bb/nfl4th")
# devtools::install_github("saiemgilani/cfbfastR")
# devtools::install_github("dynastyprocess/ffscrapr")
# devtools::install_github("jthomasmock/espnscrapeR")
# devtools::install_github("colinifer/initR", auth_token = Sys.getenv('authtoken'))
# devtools::install_github('gregce/ipify')
proj_name <- 'football'
pkgs <- c(
  # Core packages
  'devtools',
  'tidyverse',
  'glue',
  'initR',
  
  # Football packages
  'nflfastR',
  'teamcolors',
  'nflreadr',
  'nflplotR',
  'cfbfastR',
  'nfl4th',
  'ffscrapr',
  'ffopportunity',
  'ffpros',
  'ffsimulator',
  'gsisdecoder',
  'espnscrapeR',
  
  # DB packages
  'odbc',
  'RPostgres',
  
  # Web packages
  'RCurl',
  
  # Stats packages
  'pracma',
  'DescTools',
  'zoo',
  'fmsb',
  
  # Table packages
  'gt',
  'reactable',
  'png',
  'DT',
  
  # ggplot packages
  'ggthemes',
  'ggimage',
  'ggforce',
  'ggtext',
  'ggridges',
  'ggrepel',
  'ggpmisc',
  'ggbeeswarm',
  'cowplot',
  'webshot',
  'gridExtra',
  'grid',
  'animation',
  'viridis',
  
  # Font packages
  'extrafont',
  'shadowtext',
  
  # Misc. packages
  'furrr',
  'tidytext',
  'na.tools',
  'tictoc',
  'shiny',
  'qs',
  
  # Unnecessary packages
  'pander',
  'distill',
  'arrow', # incompatible w/ Apple ARM
    
  NULL
)

initR::fx.load_packages(pkgs) |>
  suppressMessages()

options(tibble.print_min=25)
`%notin%` <- Negate(`%in%`)


# Initialize working directory --------------------------------------------

fx.setdir(proj_name)


# Create standard objects -------------------------------------------------

current_season <- fx.get_year()
year <- fx.get_year()

source_files <- c(
  'plots/assets/plot_theme.R',
  'plots/assets/gt_themes.R',
  'https://raw.githubusercontent.com/nflverse/nflfastR/master/R/utils.R',
  'https://raw.githubusercontent.com/nflverse/nflfastR/master/R/aggregate_game_stats.R',
  'https://raw.githubusercontent.com/nflverse/nflfastR/master/R/helper_add_xyac.R',
  'https://raw.githubusercontent.com/nflverse/nflfastR/master/R/helper_add_nflscrapr_mutations.R',
  'data/fastr_mods.R',
  'init/init_cfb.R',
  # 'data/cfb_fastr_mods.R'
  NULL
)

map(.x = source_files, ~source(.x, echo = F)) |> 
  invisible()


# Update DBs --------------------------------------------------------------

future::plan("multisession")
nflfastR::update_db(
  tblname = "nflfastR_pbp",
  force_rebuild = FALSE,
  db_connection = initR::fx.db_con(x.host = 'localhost')
)

cfbfastR::update_cfb_db(
  tblname = 'cfbfastR_pbp',
  force_rebuild = FALSE,
  db_connection = initR::fx.db_con(x.host = 'localhost')
)


# Create variables & dataframes -------------------------------------------
# sleeper_players_df <- fx.get_sleeper_api_players()
# source("fantasy_football/ff_init.R")
# espn_players_df <- fx.get_espn_players() # not working, relies on roster load

source('data/fix_rookies.R')

# nflfastR data
con <- fx.db_con(x.host = 'localhost')
# update_roster_db(season = year, db_connection = fx.db_con(x.host = 'localhost'))
roster_df <- tbl(con, 'nflfastR_rosters') |> 
  filter(season == year) |>
  collect()
# update_schedule_db(season = year, db_connection = fx.db_con(x.host = 'localhost'))
schedule_df <- tbl(con, 'nflfastR_schedule') |> 
  filter(season == year) |> 
  collect()
# update_trades_db(season = year, db_connection = fx.db_con(x.host = 'localhost'))
trades_df <- tbl(con, 'nflfastR_trades') |> 
  filter(season == year) |> 
  collect()
# update_draft_db(season = year, db_connection = fx.db_con(x.host = 'localhost'))
draft_df <- tbl(con, 'nflfastR_draft') |> 
  filter(season == year) |> 
  collect()

pbp_df <- tbl(con, 'nflfastR_pbp') |> 
  filter(season == year) |> 
  collect() # |> 
  # fix_rookies()
dbDisconnect(con)

# inherit update_db arguments
# Not working before 2013
update_player_stats_db()
update_player_stats_weekly_db()
# Not working before 2006
update_team_stats_db()
update_team_stats_weekly_db()


source('init_ff.R')

# Rosters
fantasy_rosters <- ff_rosters(ff_conn_beep_boop) |>
  mutate(on_roster = TRUE,
         league = 'Beep Boop') |>
  rbind(ff_rosters(ff_conn_drinkers) |>
          mutate(on_roster = TRUE,
                 league = 'Drinkers')) |>
  # rbind(ff_rosters(ff_conn_kepler) |>
  #         mutate(on_roster = TRUE,
  #                league = 'Kepler')) |>
  rbind(ff_rosters(ff_conn_family) |>
          mutate(on_roster = TRUE,
                 league = 'Family')) |>
  left_join(roster_df %>%
              select(
                gsis_id,
                espn_id) |> 
              mutate(espn_id = as.numeric(espn_id)),
            by = c('player_id' = 'espn_id'))

matchup_df <- schedule_df |> 
  filter(season == year) |> 
  mutate(posteam = home_team,
         oppteam = away_team) |>
  select(
    game_id,
    season,
    game_type,
    week,
    gameday,
    weekday,
    gametime,
    posteam,
    oppteam,
    away_team,
    home_team,
    away_score,
    home_score,
    home_result = result,
    stadium,
    location,
    roof,
    surface,
    old_game_id
  ) |> 
  rbind(
    schedule_df |> 
      mutate(posteam = away_team,
             oppteam = home_team) |>
      select(
        game_id,
        season,
        game_type,
        week,
        gameday,
        weekday,
        gametime,
        posteam,
        oppteam,
        away_team,
        home_team,
        away_score,
        home_score,
        home_result = result,
        stadium,
        location,
        roof,
        surface,
        old_game_id
      )
  ) |> 
  arrange(old_game_id)


team_stats <- pbp_df |>
  calculate_team_stats_mod() |>
  left_join(
    pbp_df |> 
      group_by(posteam) |> 
      rename(team = posteam) |> 
      filter(play_type %in% c('pass', 'run')) |> 
      summarise(
        offense_snaps = n()
        )
  ) |> 
  left_join(
    pbp_df |> 
      group_by(defteam) |> 
      rename(team = defteam) |> 
      filter(play_type %in% c('pass', 'run')) |> 
      summarise(
        defense_snaps = n()
      )
  )


team_stats_weekly <- pbp_df |> 
  calculate_team_stats_mod(weekly = TRUE) |> 
  left_join(
    pbp_df |> 
      group_by(posteam, game_id) |> 
      rename(team = posteam) |> 
      filter(play_type %in% c('pass', 'run')) |> 
      summarise(
        offense_snaps = n()
      ),
    by = c('team', 'game_id')
  ) |> 
  left_join(
    pbp_df |> 
      group_by(defteam, game_id) |> 
      rename(team = defteam) |> 
      filter(play_type %in% c('pass', 'run')) |> 
      summarise(
        defense_snaps = n()
      ),
    by = c('team', 'game_id')
  )


player_stats <- pbp_df |> 
  calculate_player_stats_mod() 


player_stats_weekly <- pbp_df |> 
  calculate_player_stats_mod(weekly = TRUE)


ff_free_agents <- fx.ff_free_agents(player_stats, 'Beep Boop')




# Update DBs -------------------------------------------------------


# Maverick
  nflfastR::update_db(
    tblname = "nflfastR_pbp",
    force_rebuild = FALSE,
    db_connection = initR::fx.db_con(x.host = 'Maverick.local')
  )
  cfbfastR::update_cfb_db(
    tblname = 'cfbfastR_pbp',
    force_rebuild = FALSE,
    db_connection = initR::fx.db_con(x.host = 'Maverick.local')
  )

  
  
# Moose
  
  # nflfastR::update_db(
  #   tblname = "nflfastR_pbp",
  #   force_rebuild = FALSE,
  #   db_connection = initR::fx.db_con(x.host = 'Moose.local')
  # )
  # cfbfastR::update_cfb_db(
  #   tblname = 'cfbfastR_pbp',
  #   force_rebuild = FALSE,
  #   db_connection = initR::fx.db_con(x.host = 'Moose.local')
  # )
  
  nflfastR::update_db(
    tblname = "nflfastR_pbp",
    force_rebuild = FALSE,
    db_connection = initR::fx.db_con(x.port = '4433')
  )
  cfbfastR::update_cfb_db(
    tblname = 'cfbfastR_pbp',
    force_rebuild = FALSE,
    db_connection = initR::fx.db_con(x.port = '4433')
  )
  update_player_stats_db(con = initR::fx.db_con(x.port = '4433'),
                         pbp = pbp_df |> filter(season == season))
  update_player_stats_weekly_db(con = initR::fx.db_con(x.port = '4433'),
                                pbp = pbp_df |> filter(season == season))
  
  update_team_stats_db(con = initR::fx.db_con(x.port = '4433'),
                       pbp = pbp_df |> filter(season == season))
  update_team_stats_weekly_db(con = initR::fx.db_con(x.port = '4433'),
                              pbp = pbp_df |> filter(season == season))
  
  # nflfastR::update_db(
  #   tblname = "nflfastR_pbp",
  #   force_rebuild = FALSE,
  #   db_connection = initR::fx.db_con(x.host = 'Iceman.local')
  # )
  # cfbfastR::update_cfb_db(
  #   tblname = 'cfbfastR_pbp',
  #   force_rebuild = FALSE,
  #   db_connection = initR::fx.db_con(x.host = 'Iceman.local')
  # )
  
  nflfastR::update_db(
    tblname = "nflfastR_pbp",
    force_rebuild = FALSE,
    db_connection = initR::fx.db_con()
  )
  cfbfastR::update_cfb_db(
    tblname = 'cfbfastR_pbp',
    force_rebuild = FALSE,
    db_connection = initR::fx.db_con()
  )
  update_player_stats_db(con = initR::fx.db_con(),
                         pbp = pbp_df |> filter(season == season))
  update_player_stats_weekly_db(con = initR::fx.db_con(),
                                pbp = pbp_df |> filter(season == season))
  
  update_team_stats_db(con = initR::fx.db_con(),
                       pbp = pbp_df |> filter(season == season))
  update_team_stats_weekly_db(con = initR::fx.db_con(),
                              pbp = pbp_df |> filter(season == season))
 
