#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

proj_name <- 'football'
pkgs <- c(
    'devtools',
    'tidyverse',
    'nflfastR',
    'nflreadr',
    'cfbfastR',
    'nfl4th',
    'ffscrapr',
    'gsisdecoder',
    'espnscrapeR',
    'DBI',
    'odbc',
    'RPostgres',
    'arrow',
    'shiny',
    'qs',
    'distill',
    'httr',
    'readr',
    'pander',
    'furrr',
    'na.tools',
    'ggimage',
    'teamcolors',
    'glue',
    'dplyr',
    'jsonlite',
    'tictoc',
    'animation',
    'gt',
    'reactable',
    'png',
    'DT',
    'ggthemes',
    'ggforce',
    'ggridges',
    'ggrepel',
    'ggpmisc',
    'ggbeeswarm',
    'cowplot',
    'webshot',
    'gridExtra',
    'grid',
    'extrafont',
    'shadowtext',
    'viridis',
    'tidytext',
    'RCurl',
    'pracma',
    'DescTools',
    'initR'
)
installed_packages <- pkgs %in%
    rownames(installed.packages())
if (any(installed_packages == FALSE)) {
    install.packages(pkgs[!installed_packages])
}
lapply(pkgs, library, character.only = TRUE)

# Detach all packages
# lapply(paste0('package:', names(sessionInfo()$otherPkgs)), detach, character.only=TRUE, unload=TRUE)

rm(pkgs, installed_packages)

options(tibble.print_min=25)

`%notin%` <- Negate(`%in%`)

# source("../initR/init.R")
fx.setdir(proj_name)

current_season <- initR::fx.get_year()
year <- initR::fx.get_year()

# Create standard objects -------------------------------------------------

source('plots/assets/plot_theme.R', echo = F)
source('data/fastr_mods.R')
source('shiny/wopr/R/data_load.R')
source('shiny/wopr/R/ui.R')
source('shiny/wopr/R/server.R')

# Run the application 
shinyApp(ui = wopr_ui, server = wopr_server)