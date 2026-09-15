#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

proj_name <- "football"
pkgs <- c(
    "devtools",
    "tidyverse",
    "nflfastR",
    "gsisdecoder",
    "espnscrapeR",
    "DBI",
    "odbc",
    "RMariaDB",
    "shiny",
    "distill",
    "httr",
    "readr",
    "pander",
    "furrr",
    "na.tools",
    "ggimage",
    "teamcolors",
    "glue",
    "dplyr",
    "jsonlite",
    "RJSONIO",
    "tictoc",
    "animation",
    "gt",
    "reactable",
    "png",
    "DT",
    "ggthemes",
    "ggforce",
    "ggridges",
    "ggrepel",
    "ggpmisc",
    "ggbeeswarm",
    "cowplot",
    "gridExtra",
    "grid",
    "extrafont",
    "shadowtext",
    "tidytext",
    "RCurl",
    "pracma"
)
installed_packages <- pkgs %in%
    rownames(installed.packages())
if (any(installed_packages == FALSE)) {
    install.packages(pkgs[!installed_packages])
}
lapply(pkgs, library, character.only = TRUE)
# library("nflscrapR") # doesn't work anymore
library("nflfastR")

setwd('../../')
# getwd() %>% print()
# 
# source('init.R', local = TRUE)

con <- fx.db_con(x.host = 'localhost')
year <- fx.get_year()
player_stats <- tbl(con, 'nflfastR_pbp') %>% 
    filter(season == year) %>% 
    collect() %>%
    calculate_player_stats_mod(weekly = TRUE)

fantasy_rosters <- ff_rosters(ff_conn_beep_boop) %>%
    mutate(on_roster = TRUE,
           league = 'Beep Boop') %>%
    rbind(ff_rosters(ff_conn_drinkers) %>%
              mutate(on_roster = TRUE,
                     league = 'Drinkers')) %>%
    rbind(ff_rosters(ff_conn_kepler) %>%
              mutate(on_roster = TRUE,
                     league = 'Kepler')) %>%
    rbind(ff_rosters(ff_conn_family) %>%
              mutate(on_roster = TRUE,
                     league = 'Family'))

dbDisconnect(con)

# Define UI for application that draws a histogram
ui <- fluidPage(
    selectInput("dataset", label = "Dataset", choices = ls("package:datasets")),
    # selectInput("dataset", label = "Season", choices = c(2021:2000)),
    verbatimTextOutput("summary"),
    tableOutput("table")
)
# ui <- fluidPage(
#     
#     title = "Receiving Stats",
#     
#     sidebarPanel(
#         h4(""),
#         selectInput('posteam', 'Team', choices = player_stats)
#     ),
#     mainPanel(plotOutput("def_rec_yard_plot"))
# )

server <- function(input, output, session) {
    # Create a reactive expression
    dataset <- reactive({
        get(input$dataset, "package:datasets")
    })
    
    output$summary <- renderPrint({
        # Use a reactive expression by calling it like a function
        summary(dataset())
    })
    
    output$table <- renderTable({
        dataset()
    })
}

# server <- function(input, output) {
#     
#     output$def_rec_yard_plot <- renderPlot({
#         # generate bins based on input$bins from ui.R
#         ggplot(
#             dataset %>% 
#                 dplyr::filter(defteam == input$defteam) %>% 
#                 slice(1:20),
#             aes(x = receiver, y = tot_air_yards)) +
#             geom_bar(
#                 aes(x = reorder(receiver, -tot_air_yards), 
#                     y = tot_air_yards), 
#                 stat='identity') +
#             geom_image(
#                 aes(image = headshot_url), 
#                 asp = 16/9,
#                 nudge_y = 1
#             ) +
#             theme_cw +
#             theme(
#                 axis.text.x = element_text(angle = 45)
#             )
#     }, height = 800)
# }

# Run the application 
shinyApp(ui = ui, server = server)
