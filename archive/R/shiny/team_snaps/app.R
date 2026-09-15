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

reactiveConsole(TRUE)

source('shiny/team_snaps/ui.R')
source('shiny/team_snaps/server.R')

# Run the application 
shinyApp(ui = team_snap_ui, server = team_snap_server)