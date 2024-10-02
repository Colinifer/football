server <- function(input, output, session) {
  
  
  # Create a reactive expression
  source('~/Documents/dev/football/data/fastr_mods.R')
  
  
  # Seasons
  choices_seasons <- reactive({
    con <- initR::fx.db_con(# x.host = 'localhost',
      x.host = 'localhost',
      x.dbname = 'football')
    on.exit(dbDisconnect(con), add = TRUE)
    
    choices_seasons <- tbl(con, glue('nflfastR_pbp')) |> 
      select(season) |> 
      distinct() |> 
      arrange(desc(season)) |>
      collect() |> 
      pull()
    choices_seasons
  })
  
  updateSelectInput(inputId = 'rx.season', choices = choices_seasons())
  
  i_season <- reactive({
    input$rx.season
  })
  
  
  # Teams
  choices_teams <- reactive({
    con <- initR::fx.db_con(# x.host = 'localhost',
      x.host = 'localhost',
      x.dbname = 'football')
    on.exit(dbDisconnect(con), add = TRUE)
    
    choices_teams <-  tbl(con, glue('nflfastR_pbp')) |> 
      filter(season == input$rx.season) |> 
      select(posteam) |> 
      distinct() |> 
      arrange(posteam) |> 
      collect() |> 
      pull(posteam)
    choices_teams
  })
  updateSelectInput(inputId = 'rx.team', choices = choices_teams())
  
  output$default_stat_choice <- renderText({ 
    choices_teams()['PHI']
  })
  
  
  
}
