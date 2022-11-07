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
  NULL
)
initR::fx.load_packages(pkgs) |>
  suppressMessages()

source('~/Documents/dev/football/data/fastr_mods.R')



# UI ----------------------------------------------------------------------

ui <- fluidPage(
  
  # Input Row
  fluidRow(
    column(width = 6,
           # selectInput(inputId = 'dataset', 
           #             label = 'Dataset', 
           #             # Choices of datasets
           #             choices = ls('package:datasets')),
           selectInput(inputId = 'season',
                       label = 'season',
                       # Seasons sorted in decreasing order1
                       choices = sort(c(1999:nflreadr::get_current_season()), decreasing = TRUE),
                       selected = nflreadr::get_current_season(),
                       multiple = FALSE),
           submitButton(text = 'Update')
    ),
    column(width = 6,
           plotOutput("distPlot")
    )
  ),
  
  # Table Summary
  h2('Summary'),
  verbatimTextOutput(outputId = 'summary'),
  
  # Table
  h2('Data'),
  DT::dataTableOutput(outputId = 'table')
)



# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  # Create a reactive expression
  source('~/Documents/dev/football/data/fastr_mods.R')
  
  # dataset <- reactive({
  #   con <- initR::fx.db_con(x.host = 'localhost',
  #                           x.dbname = 'football')
  #   tbl(con, 'nflfastR_pbp') |>
  #     filter(season == input$season) |>
  #     collect() |>
  #     calculate_team_stats_mod()
  #   dbDisconnect(con)
  # 
  #   # get(input$dataset, 'package:datasets')
  # })
  
  # output$summary <- renderPrint({
  #   # Use a reactive expression by calling it like a function
  #   summary(dataset())
  # })
  
  # output$table <- renderTable({dataset()})
  
  output$table <- DT::renderDataTable({
    u.season <- as.integer(input$season)[1]
    
    con <- initR::fx.db_con(x.host = 'localhost',
                            x.dbname = 'football')
    # on.exit(dbDisconnect(con), add = TRUE)

    data <- tbl(con, 'nflfastR_pbp') |>
          filter(season == u.season) |>
          collect() |>
          calculate_team_stats_mod() |> 
      as.data.frame()

    data
        # dbGetQuery(con, paste0(
    #   "SELECT season, game_id, passer_player_name FROM \"nflfastR_pbp\" WHERE season = ", input$season, " LIMIT 200;"))
  })
}

shinyApp(ui, server)
