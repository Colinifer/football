proj_name <- 'football'
pkgs <- c(
  # Core packages
  'devtools',
  'tidyverse',
  'bslib',
  'thematic',
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

thematic::thematic_shiny()

# theme <- bs_theme(
#   bg = "#2C2D35", fg = "white", primary = "#375A7F",
#   heading_font = font_google("Chivo"),
#   base_font = font_google("Montserrat")
# )
# 
# bs_theme_preview(theme)

source('~/Documents/dev/football/data/fastr_mods.R')

reactiveConsole(TRUE)

# Data --------------------------------------------------------------------


# UI ----------------------------------------------------------------------

ui <- fluidPage(
  theme = bs_theme(
    bg = "#2C2D35", fg = "white", primary = "#375A7F",
    heading_font = font_google("Chivo"),
    base_font = font_google("Montserrat")
  ),
  
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
         reactable::reactableOutput(outputId = 'table')
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
  output$table <- try(
    reactable::renderReactable(
      reactable::reactable(

        # Reactable Data --------------------------------------------------
        
        data = {
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
          team_passing_stats <- c("completions", "attempts",
                                  "passing_yards", "passing_tds",
                                  "interceptions", "sacks",
                                  "passing_air_yards", "passing_yards_after_catch",
                                  "passing_first_downs", "passing_epa",
                                  "pacr", "anya", "dakota"
          )
          team_receiving_stats <- c("receptions", "targets",
                                    "receiving_yards", "receiving_tds",
                                    "receiving_air_yards", "receiving_yards_after_catch",
                                    "receiving_first_downs", "receiving_epa",
                                    "racr", "receiving_receiving_redzone_targets"
          )
          team_rushing_stats <- c("carries", "rushing_yards",
                                  "rushing_tds", "rushing_fumbles",
                                  "rushing_fumbles_lost", "rushing_first_downs",
                                  "rushing_epa", "hvts"
          )
          
          player_information <- c("player_id", "player_name", 
                                  "recent_team", "season"
          )
          player_passing_stats <- c("completions",
                                    "attempts",
                                    "passing_yards",
                                    "passing_tds",
                                    "interceptions",
                                    "sacks",
                                    "passing_air_yards",
                                    "passing_yards_after_catch",
                                    "passing_first_downs",
                                    "passing_epa",
                                    "pacr",
                                    "anya",
                                    "dakota"
          )
          player_receiving_stats <- c("routes_run",
                                      "receptions",
                                      "targets",
                                      "receiving_yards",
                                      "receiving_tds",
                                      "receiving_air_yards",
                                      "receiving_yards_after_catch",
                                      "receiving_first_downs",
                                      "receiving_epa",
                                      "racr",
                                      "target_share",
                                      "air_yards_share",
                                      "wopr",
                                      "hvt"
          )
          player_rushing_stats <- c("carries",
                                    "rushing_yards",
                                    "rushing_tds",
                                    "rushing_fumbles",
                                    "rushing_fumbles_lost",
                                    "rushing_first_downs",
                                    "rushing_epa",
                                    "hvt"
          )
          
          
          if (i.query_type == 'team' & i.columns == 'Passing') {
            
            # Team Passing ------------------------------------------------
            
            o.columns <- c(team_information, team_passing_stats)
            o.sort <- c('passing_epa')
            
            data <- tbl(con, glue('nflfastR_{i.query_type}_stats')) |>
              filter(season == i.season) |>
              select(any_of(o.columns)) |> 
              mutate(adot = passing_air_yards / attempts) |> 
              collect() |>
              arrange(desc(!!!rlang::syms(o.sort))) |>
              left_join(teams_colors_logos |> select(team_abbr, team_logo_espn),
                        by = c('team' = 'team_abbr')) |>
              select(team_logo_espn, everything(), -team) |>
              # mutate(team_logo_espn = paste0('<img src="', team_logo_espn, '" height="52"></img>')) |>
              mutate_if(is.numeric,
                        round,
                        digits = 2) |>
              rename(team = team_logo_espn)
            
            
          } else if (i.query_type == 'team' & i.columns == 'Receiving') {
            
            # Team Receiving ----------------------------------------------
            
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
              # mutate(team_logo_espn = paste0('<img src="', team_logo_espn, '" height="52"></img>')) |>
              mutate_if(is.numeric,
                        round,
                        digits = 2) |>
              rename(team = team_logo_espn)
            
            
          } else if (i.query_type == 'team' & i.columns == 'Rushing') {
            
            # Team Rushing ------------------------------------------------
            
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
              # mutate(team_logo_espn = paste0('<img src="', team_logo_espn, '" height="52"></img>')) |>
              mutate_if(is.numeric,
                        round,
                        digits = 2) |>
              rename(team = team_logo_espn)
            
            
          } else if (i.query_type == 'player' & i.columns == 'Passing') {
            
            # Player Passing ----------------------------------------------
            
            o.columns <- c(player_information, player_passing_stats)
            o.sort <- c('passing_epa')
            
            data <- tbl(con, glue('nflfastR_{i.query_type}_stats')) |>
              filter(season == i.season &
                       completions > 1) |>
              select(any_of(o.columns)) |>
              mutate(adot = passing_air_yards / attempts) |> 
              collect() |>
              arrange(desc(!!!rlang::syms(o.sort))) |>
              left_join(
                teams_colors_logos |> select(team_abbr, team_logo_espn),
                by = c('recent_team' = 'team_abbr')
              ) |>
              left_join(roster_df(),
                        by = c('player_id' = 'gsis_id')) |>
              select(headshot_url,
                     team_logo_espn,
                     everything(),
                     -player_id,
                     -recent_team) |>
              # mutate(
              #   headshot_url = paste0('<img src="', headshot_url, '" height="52"></img>'),
              #   team_logo_espn = paste0('<img src="', team_logo_espn, '" height="52"></img>')
              # ) |>
              mutate_if(is.numeric,
                        round,
                        digits = 2) |>
              rename(player = headshot_url,
                     team = team_logo_espn)
            
            
          } else if (i.query_type == 'player' & i.columns == 'Receiving') {
            
            # Player Receiving --------------------------------------------
            
            o.columns <- c(player_information, player_receiving_stats)
            o.sort <- c('receiving_epa')
            
            data <- tbl(con, glue('nflfastR_{i.query_type}_stats')) |>
              filter(season == i.season &
                       receptions > 1) |>
              select(any_of(o.columns)) |>
              collect() |>
              arrange(desc(!!!rlang::syms(o.sort))) |>
              left_join(
                teams_colors_logos |> select(team_abbr, team_logo_espn),
                by = c('recent_team' = 'team_abbr')
              ) |>
              left_join(roster_df(),
                        by = c('player_id' = 'gsis_id')) |>
              select(headshot_url,
                     team_logo_espn,
                     everything(),
                     -player_id,
                     -recent_team) |>
              # mutate(
              #   headshot_url = paste0('<img src="', headshot_url, '" height="52"></img>'),
              #   team_logo_espn = paste0('<img src="', team_logo_espn, '" height="52"></img>')
              # ) |>
              mutate_if(is.numeric,
                        round,
                        digits = 2) |>
              rename(player = headshot_url,
                     team = team_logo_espn)
            
            
          } else if (i.query_type == 'player' & i.columns == 'Rushing') {
            
            # Player Receiving --------------------------------------------
            
            o.columns <- c(player_information, player_rushing_stats)
            o.sort <- c('rushing_epa')
            
            data <- tbl(con, glue('nflfastR_{i.query_type}_stats')) |>
              filter(season == i.season &
                       carries > 1) |>
              select(any_of(o.columns)) |>
              collect() |>
              arrange(desc(!!!rlang::syms(o.sort))) |>
              left_join(
                teams_colors_logos |> select(team_abbr, team_logo_espn),
                by = c('recent_team' = 'team_abbr')
              ) |>
              left_join(roster_df(),
                        by = c('player_id' = 'gsis_id')) |>
              select(headshot_url,
                     team_logo_espn,
                     everything(),
                     -player_id,
                     -recent_team) |>
              # mutate(
              #   headshot_url = paste0('<img src="', headshot_url, '" height="52"></img>'),
              #   team_logo_espn = paste0('<img src="', team_logo_espn, '" height="52"></img>')
              # ) |>
              mutate_if(is.numeric,
                        round,
                        digits = 2) |>
              rename(player = headshot_url,
                     team = team_logo_espn)
            
            
          }
          
          on.exit(dbDisconnect(con), add = TRUE)
          
          data
          
        }, 

        # Reactable Columns -----------------------------------------------

        columns = {
          i.query_type <- tolower(input$rx.query_type[1])
          i.season <- as.integer(input$rx.season[1])
          i.columns <- as.character(input$rx.stats_type[1])
          
          if (i.query_type == 'team' & i.columns == 'Passing') {
            
            # Team Passing ------------------------------------------------
            
            list(
              team = colDef('Team',
                            sortable = FALSE,
                            filterable = FALSE,
                            cell = function(value) {
                              image <-
                                img(src = value,
                                    style = "height: 36px;",
                                    alt = value
                                )
                              tagList(div(style = "display: inline-block; width: 45px;", image))
                            }),
              season = colDef('Season',
                              sortable = FALSE,
                              filterable = FALSE),
              completions = colDef('C'),
              attempts = colDef('ATT'),
              passing_yards = colDef('YDS'),
              passing_tds = colDef('Pass TD'),
              interceptions = colDef('INT'),
              sacks = colDef('SACKS'),
              passing_air_yards = colDef('Air YDS'),
              passing_yards_after_catch = colDef('YAC'),
              adot = colDef('aDOT'),
              passing_first_downs = colDef('1stD'),
              passing_epa = colDef('EPA'),
              pacr = colDef('PACR'),
              dakota = colDef('DAKOTA')
            )
            
          } else if (i.query_type == 'team' & i.columns == 'Receiving') {
            
            # Team Receiving ----------------------------------------------
            
            list(
              team = colDef('Team',
                            sortable = FALSE,
                            filterable = FALSE,
                            cell = function(value) {
                              image <-
                                img(src = value,
                                    style = "height: 36px;",
                                    alt = value
                                )
                              tagList(div(style = "display: inline-block; width: 45px;", image))
                            }),
              season = colDef('Season',
                              sortable = FALSE,
                              filterable = FALSE),
              receptions = colDef('REC'),
              targets = colDef('TGTS'),
              receiving_yards = colDef('YDS'),
              receiving_tds = colDef('TD'),
              receiving_air_yards = colDef('Air YDS'),
              receiving_yards_after_catch = colDef('YAC'),
              receiving_first_downs = colDef('1stD'),
              receiving_epa = colDef('EPA'),
              racr = colDef('RACR'),
              receiving_receiving_redzone_targets = colDef('Redz TGTS')
            )
            
          } else if (i.query_type == 'team' & i.columns == 'Rushing') {
            
            # Team Rushing ------------------------------------------------
            
            list(
              team = colDef('Team',
                            sortable = FALSE,
                            filterable = FALSE,
                            cell = function(value) {
                              image <-
                                img(src = value,
                                    style = "height: 36px;",
                                    alt = value
                                )
                              tagList(div(style = "display: inline-block; width: 45px;", image))
                            }),
              season = colDef('Season',
                              sortable = FALSE,
                              filterable = FALSE),
              carries = colDef('CAR'),
              rushing_yards = colDef('YDS'),
              rushing_tds = colDef('TD'),
              rushing_fumbles = colDef('FUM'),
              rushing_fumbles_lost = colDef('LOST'),
              rushing_first_downs = colDef('1stD'),
              rushing_epa = colDef('EPA'),
              hvts = colDef('HVT')
            )
            
          } else if (i.query_type == 'player' & i.columns == 'Passing') {
            
            # Player Passing ----------------------------------------------
            
            list(
              player = colDef('',
                            sortable = FALSE,
                            filterable = FALSE,
                            cell = function(value) {
                              image <-
                                img(src = value,
                                    style = "height: 36px;",
                                    alt = value
                                )
                              tagList(div(style = "display: inline-block; width: 45px;", image))
                            }),
              team = colDef('',
                              sortable = FALSE,
                              filterable = FALSE,
                              cell = function(value) {
                                image <-
                                  img(src = value,
                                      style = "height: 36px;",
                                      alt = value
                                  )
                                tagList(div(style = "display: inline-block; width: 45px;", image))
                              }),
              player_name = colDef('Player'),
              season = colDef('Season',
                              sortable = FALSE,
                              filterable = FALSE),
              completions = colDef('C'),
              attempts = colDef('ATT'),
              passing_yards = colDef('YDS'),
              passing_tds = colDef('Pass TD'),
              interceptions = colDef('INT'),
              sacks = colDef('SACKS'),
              passing_air_yards = colDef('Air YDS'),
              passing_yards_after_catch = colDef('YAC'),
              adot = colDef('aDOT'),
              passing_first_downs = colDef('1stD'),
              passing_epa = colDef('EPA'),
              pacr = colDef('PACR'),
              anya = colDef('anya'),
              dakota = colDef('DAKOTA')
            )
            
          } else if (i.query_type == 'player' & i.columns == 'Receiving') {
            
            # Player Receiving --------------------------------------------
            
            list(
              player = colDef('',
                              sortable = FALSE,
                              filterable = FALSE,
                              cell = function(value) {
                                image <-
                                  img(src = value,
                                      style = "height: 36px;",
                                      alt = value
                                  )
                                tagList(div(style = "display: inline-block; width: 45px;", image))
                              }),
              team = colDef('',
                            sortable = FALSE,
                            filterable = FALSE,
                            cell = function(value) {
                              image <-
                                img(src = value,
                                    style = "height: 36px;",
                                    alt = value
                                )
                              tagList(div(style = "display: inline-block; width: 45px;", image))
                            }),
              player_name = colDef('Player'),
              season = colDef('Season',
                              sortable = FALSE,
                              filterable = FALSE),
              receptions = colDef('REC'),
              targets = colDef('TGTS'),
              receiving_yards = colDef('YDS'),
              receiving_tds = colDef('TD'),
              receiving_air_yards = colDef('Air YDS'),
              receiving_yards_after_catch = colDef('YAC'),
              receiving_first_downs = colDef('1stD'),
              receiving_epa = colDef('EPA'),
              racr = colDef('RACR'),
              target_share = colDef('TGT Share'),
              air_yards_share = colDef('Air YDS Share'),
              wopr = colDef('WOPR'),
              hvt = colDef('HVT')
            )
            
          } else if (i.query_type == 'player' & i.columns == 'Rushing') {
            
            # Player Rushing ----------------------------------------------
            
            list(
              player = colDef('',
                              sortable = FALSE,
                              filterable = FALSE,
                              cell = function(value) {
                                image <-
                                  img(src = value,
                                      style = "height: 36px;",
                                      alt = value
                                  )
                                tagList(div(style = "display: inline-block; width: 45px;", image))
                              }),
              team = colDef('',
                            sortable = FALSE,
                            filterable = FALSE,
                            cell = function(value) {
                              image <-
                                img(src = value,
                                    style = "height: 36px;",
                                    alt = value
                                )
                              tagList(div(style = "display: inline-block; width: 45px;", image))
                            }),
              player_name = colDef('Player'),
              season = colDef('Season',
                              sortable = FALSE,
                              filterable = FALSE),
              carries = colDef('CAR'),
              rushing_yards = colDef('YDS'),
              rushing_tds = colDef('TD'),
              rushing_fumbles = colDef('FUM'),
              rushing_fumbles_lost = colDef('LOST'),
              rushing_first_downs = colDef('1stD'),
              rushing_epa = colDef('EPA'),
              hvt = colDef('HVT')
            )
            
          }
        },
        fullWidth = FALSE,
        filterable = FALSE,
        showPageSizeOptions = TRUE,
        pageSizeOptions = c(32, 50, 100),
        minRows = 32,
        defaultPageSize = 32,
        outlined = TRUE,
        striped = TRUE,
        highlight = TRUE,
        theme = reactableTheme(
          color = "hsl(233, 9%, 87%)",
          backgroundColor = "hsl(233, 9%, 19%)",
          borderColor = "hsl(233, 9%, 22%)",
          stripedColor = "hsl(233, 12%, 22%)",
          highlightColor = "hsl(233, 12%, 24%)",
          inputStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
          selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
          pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
          pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)")
        )
    )
  )
  )
}

shinyApp(ui, server)
