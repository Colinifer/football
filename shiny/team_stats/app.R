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
  'dbplyr',
  
  # Web packages
  'RCurl',
  'DT',
  'reactable',
  NULL
)
initR::fx.load_packages(pkgs) |>
  suppressMessages()

source('~/Documents/dev/football/data/fastr_mods.R')

reactiveConsole(TRUE)

# Data --------------------------------------------------------------------


# UI ----------------------------------------------------------------------

ui <- fluidPage(
  
  # Input Row
  h1(str_to_title(proj_name)),
  fluidRow(
    column(width = 6,
           selectInput(inputId = 'rx.query_type',
                       label = 'Team or Player stats',
                       # Seasons sorted in decreasing order
                       selected = 'Team',
                       choices = c('Team', 'Player'),
                       multiple = FALSE),
           selectInput(inputId = 'rx.season',
                       label = 'Season',
                       # Seasons sorted in decreasing order
                       selected = nflreadr::get_current_season(),
                       choices = NULL,
                       multiple = FALSE),
           selectInput(inputId = 'rx.stats_type',
                       label = 'Stats',
                       # Seasons sorted in decreasing order
                       selected = textOutput('default_stat_choice'),
                       choices = NULL,
                       multiple = FALSE),
           # selectInput(inputId = 'season',
           #             label = 'season',
           #             # Seasons sorted in decreasing order1
           #             choices = sort(c(1999:nflreadr::get_current_season()), decreasing = TRUE),
           #             selected = nflreadr::get_current_season(),
           #             multiple = FALSE),
           submitButton(text = 'Update')
    ),
    column(width = 6,
           plotOutput("distPlot")
    )
  ),
  
  # Table Summary
  # h2('Summary'),
  # verbatimTextOutput(outputId = 'summary'),
  
  # Table
  h2('Data'),
  column(width = 12, 
         DT::dataTableOutput(outputId = 'table')
         )
)



# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  
  
  # Create a reactive expression
  source('~/Documents/dev/football/data/fastr_mods.R')
  
  
  # Seasons
  choices_seasons <- reactive({
    con <- initR::fx.db_con(# x.host = 'localhost',
                            x.port = '4433',
                            x.dbname = 'football')
    on.exit(dbDisconnect(con), add = TRUE)
    
    i.query_type <- tolower(input$rx.query_type[1])
    
    choices_seasons <- tbl(con, glue('nflfastR_{i.query_type}_stats')) |> 
      select(season) |> 
      collect() |> 
      pull() |> 
      sort(decreasing = TRUE)
    choices_seasons
  })
  
  updateSelectInput(inputId = 'rx.season', choices = choices_seasons())
  
  i_season <- reactive({
    input$rx.season
  })
  
  
  # Stats Type
  choices_stats <- reactive({
    choices_stats <- c('Passing', 'Receiving', 'Rushing')
    choices_stats
  })
  updateSelectInput(inputId = 'rx.stats_type', choices = choices_stats())
  
  output$default_stat_choice <- renderText({ 
    choices_stats()[1]
  })
  
  
  # Roster DF
  roster_df <- reactive({
    con <- initR::fx.db_con(#x.host = 'localhost',
                            x.port = '4433', 
                            x.dbname = 'football')
    on.exit(dbDisconnect(con), add = TRUE)
    
    i.season <- as.integer(input$rx.season[1])
    
    roster_df <- tbl(con, glue('nflfastR_rosters')) |> 
      filter(season == i.season) |>
      select(gsis_id, headshot_url) |>
      collect()
    roster_df
  })
  
  # Data Table
  output$table <- try(DT::renderDataTable(DT::datatable({
    i.query_type <- tolower(input$rx.query_type[1])
    i.season <- as.integer(input$rx.season[1])
    i.columns <- as.character(input$rx.stats_type[1])
    
    con <- initR::fx.db_con(#x.host = 'localhost',
                            x.port = '4433', 
                            x.dbname = 'football')
    
    # roster_df <- tbl(con, glue('nflfastR_rosters')) |> 
    #   filter(season == i_season) |> 
    #   select(gsis_id, headshot_url) |> 
    #   collect()
    
    team_information <- c("team", "season")
    team_passing_stats <- c("completions", "attempts", "passing_yards", "passing_tds", "interceptions",
    "sacks", "passing_air_yards", "passing_yards_after_catch",
    "passing_first_downs", "passing_epa", "pacr", "anya", "dakota")
    team_receiving_stats <- c("receptions", "targets", "receiving_yards", "receiving_tds", 
                              "receiving_air_yards", "receiving_yards_after_catch",
                              "receiving_first_downs", "receiving_epa", "racr",
                              "receiving_receiving_redzone_targets")
    team_rushing_stats <- c("carries", "rushing_yards", "rushing_tds", "rushing_fumbles", "rushing_fumbles_lost",
                              "rushing_first_downs", "rushing_epa", "hvts")
    
    player_information <- c("player_id", "player_name", "recent_team", "season")
    player_passing_stats <- c("completions", "attempts", "passing_yards", "passing_tds", "interceptions",
                       "sacks", "passing_air_yards", "passing_yards_after_catch",
                       "passing_first_downs", "passing_epa", "pacr", "anya", "dakota")
    player_receiving_stats <- c("routes_run", "receptions", "targets", "receiving_yards", "receiving_tds", 
                         "receiving_air_yards", "receiving_yards_after_catch",
                         "receiving_first_downs", "receiving_epa", "racr",
                         "target_share", "air_yards_share", "wopr", "hvt")
    player_rushing_stats <- c("carries", "rushing_yards", "rushing_tds", "rushing_fumbles", "rushing_fumbles_lost",
                       "rushing_first_downs", "rushing_epa", "hvt")
    
    
    if (i.query_type == 'team' & i.columns == 'Passing') {
      o.columns <- c(team_information, team_passing_stats)
      o.sort <- c('passing_epa')
      
      data <- tbl(con, glue('nflfastR_{i.query_type}_stats')) |>
        filter(season == i.season) |> 
        select(any_of(o.columns)) |>
        collect() |>
        arrange(desc(!!!rlang::syms(o.sort))) |> 
        left_join(teams_colors_logos |> select(team_abbr, team_logo_espn),
                  by = c('team' = 'team_abbr')) |>
        select(team_logo_espn, everything(), -team) |> 
        mutate(team_logo_espn = paste0('<img src="', team_logo_espn, '" height="52"></img>')) |>
        mutate_if(is.numeric,
                  round,
                  digits = 2) |> 
        rename(team = team_logo_espn)
      
      
    } else if (i.query_type == 'team' & i.columns == 'Receiving') {
      o.columns <- c(team_information, team_receiving_stats)
      o.sort <- c('receiving_epa')
      
      data <- tbl(con, glue('nflfastR_{i.query_type}_stats')) |>
        filter(season == i.season) |> 
        select(any_of(o.columns)) |>
        collect() |>
        arrange(desc(!!!rlang::syms(o.sort))) |> 
        left_join(teams_colors_logos |> select(team_abbr, team_logo_espn),
                  by = c('team' = 'team_abbr')) |>
        select(team_logo_espn, everything(), -team) |> 
        mutate(team_logo_espn = paste0('<img src="', team_logo_espn, '" height="52"></img>')) |>
        mutate_if(is.numeric,
                  round,
                  digits = 2) |> 
        rename(team = team_logo_espn)
      
      
    } else if (i.query_type == 'team' & i.columns == 'Rushing') {
      o.columns <- c(team_information, team_rushing_stats)
      o.sort <- c('rushing_epa')
      
      data <- tbl(con, glue('nflfastR_{i.query_type}_stats')) |>
        filter(season == i.season) |> 
        select(any_of(o.columns)) |>
        collect() |>
        arrange(desc(!!!rlang::syms(o.sort))) |> 
        left_join(teams_colors_logos |> select(team_abbr, team_logo_espn),
                  by = c('team' = 'team_abbr')) |>
        select(team_logo_espn, everything(), -team) |> 
        mutate(team_logo_espn = paste0('<img src="', team_logo_espn, '" height="52"></img>')) |>
        mutate_if(is.numeric,
                  round,
                  digits = 2) |> 
        rename(team = team_logo_espn)
      
      
    } else if (i.query_type == 'player' & i.columns == 'Passing') {
      o.columns <- c(player_information, player_passing_stats)
      o.sort <- c('passing_epa')
      
      data <- tbl(con, glue('nflfastR_{i.query_type}_stats')) |>
        filter(season == i.season & 
                 completions > 1) |>
        select(any_of(o.columns)) |>
        collect() |>
        arrange(desc(!!!rlang::syms(o.sort))) |>
        left_join(teams_colors_logos |> select(team_abbr, team_logo_espn),
                  by = c('recent_team' = 'team_abbr')) |>
        left_join(
          roster_df(),
          by = c('player_id' = 'gsis_id')) |>
        select(headshot_url, team_logo_espn, everything(), -player_id, -recent_team) |>
        mutate(headshot_url = paste0('<img src="', headshot_url, '" height="52"></img>'),
               team_logo_espn = paste0('<img src="', team_logo_espn, '" height="52"></img>')) |>
        mutate_if(is.numeric,
                  round,
                  digits = 2) |> 
        rename(player = headshot_url,
               team = team_logo_espn)
      
      
    } else if (i.query_type == 'player' & i.columns == 'Receiving') {
      o.columns <- c(player_information, player_receiving_stats)
      o.sort <- c('receiving_epa')
      
      data <- tbl(con, glue('nflfastR_{i.query_type}_stats')) |>
        filter(season == i.season & 
                 receptions > 1) |> 
        select(any_of(o.columns)) |>
        collect() |>
        arrange(desc(!!!rlang::syms(o.sort))) |> 
        left_join(teams_colors_logos |> select(team_abbr, team_logo_espn),
                  by = c('recent_team' = 'team_abbr')) |>
        left_join(
          roster_df(),
          by = c('player_id' = 'gsis_id')) |> 
        select(headshot_url, team_logo_espn, everything(), -player_id, -recent_team) |>
        mutate(headshot_url = paste0('<img src="', headshot_url, '" height="52"></img>'),
               team_logo_espn = paste0('<img src="', team_logo_espn, '" height="52"></img>')) |>
        mutate_if(is.numeric,
                  round,
                  digits = 2) |> 
        rename(player = headshot_url,
               team = team_logo_espn)
      
      
    } else if (i.query_type == 'player' & i.columns == 'Rushing') {
      o.columns <- c(player_information, player_rushing_stats)
      o.sort <- c('rushing_epa')
      
      data <- tbl(con, glue('nflfastR_{i.query_type}_stats')) |>
        filter(season == i.season & 
                 carries > 1) |> 
        select(any_of(o.columns)) |>
        collect() |>
        arrange(desc(!!!rlang::syms(o.sort))) |> 
        left_join(teams_colors_logos |> select(team_abbr, team_logo_espn),
                  by = c('recent_team' = 'team_abbr')) |>
        left_join(
          roster_df(),
          by = c('player_id' = 'gsis_id')) |> 
        select(headshot_url, team_logo_espn, everything(), -player_id, -recent_team) |>
        mutate(headshot_url = paste0('<img src="', headshot_url, '" height="52"></img>'),
               team_logo_espn = paste0('<img src="', team_logo_espn, '" height="52"></img>')) |>
        mutate_if(is.numeric,
                  round,
                  digits = 2) |> 
        rename(player = headshot_url,
               team = team_logo_espn)
      
      
    }
    
    on.exit(dbDisconnect(con), add = TRUE)
    
    data
    
  }, options = list(pageLength = 50),
  escape = F),
  server = FALSE
  
  ))

  }

shinyApp(ui, server)
