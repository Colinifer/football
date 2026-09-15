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

dataset <- pbp_df %>%
    filter(pass_attempt==1 & season_type=='REG' & two_point_attempt==0 & !is.na(receiver_id)) %>% 
    select(season, 
           game_id, 
           play_id, 
           posteam,
           defteam,
           receiver, 
           receiver_player_id, 
           receiver_id, 
           air_yards, 
           yards_gained, 
           complete_pass,
           pass_location,
           cp,
           pass_touchdown) %>% 
    # filter(!is.na(receiver)) %>% 
    # filter(defteam == 'LV') %>%
    group_by(receiver_player_id) %>% 
    mutate(
        target = 0,
        game_played = 0
    )  %>% 
    group_by(game_id, receiver, defteam) %>% 
    mutate(game_played = ifelse(row_number()==1,1,0)) %>% 
    ungroup %>% 
    group_by(game_id, play_id, receiver) %>% 
    mutate(target = ifelse(row_number()==1,1,0),
           pass_left = ifelse(pass_location=='left',1,0),
           pass_middle = ifelse(pass_location =='middle',1,0),
           pass_right = ifelse(pass_location =='right',1,0),
           complete_left = ifelse(complete_pass == 1 & pass_location=='left',1,0),
           complete_middle = ifelse(complete_pass == 1 & pass_location =='middle',1,0),
           complete_right = ifelse(complete_pass == 1 & pass_location =='right',1,0)
    ) %>% 
    ungroup %>% 
    group_by(game_id, posteam, receiver, defteam) %>% 
    summarize(
        receiver_id = unique(receiver_id),
        games = sum(game_played, na.rm = T),
        targets = sum(target, na.rm = T),
        targets_pg = targets / games,
        tot_rec_yards = sum(yards_gained, na.rm =TRUE),
        tot_air_yards = sum(air_yards, ra.rm = TRUE),
        air_yards_pg = tot_air_yards / games,
        racr = tot_rec_yards / tot_air_yards,
        receptions = sum(complete_pass),
        adot = tot_air_yards / targets,
        td = sum(pass_touchdown, ra.rm = TRUE),
        td_pg = td / games,
        target_l = sum(pass_left),
        target_m = sum(pass_middle),
        target_r = sum(pass_right),
        target_l_perct = target_l / targets,
        target_m_perct = target_m / targets,
        target_r_perct = target_r / targets,
        rec_l = sum(complete_left),
        rec_m = sum(complete_middle),
        rec_r = sum(complete_right),
        rec_l_perct = rec_l / targets,
        rec_m_perct = rec_m / targets,
        rec_r_perct = rec_r / targets
    ) %>% 
    left_join(sleeper_players_df %>% 
                  select(gsis_id, height, headshot_url),
              by = c('receiver_id' = 'gsis_id')
    ) %>% 
    ungroup() %>% 
    arrange(air_yards_pg %>% 
                desc()
    )

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    title = "Receiving Yards Allowed by Defense",
    
    sidebarPanel(
        h4("Rec. Yards Allowed by Defense"),
        selectInput('defteam', 'Def. Team', choices = dataset$defteam %>% unique() %>% sort(decreasing = FALSE))
    ),
    mainPanel(plotOutput("def_rec_yard_plot"))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$def_rec_yard_plot <- renderPlot({
        # generate bins based on input$bins from ui.R
        ggplot(
            dataset %>% 
                dplyr::filter(defteam == input$defteam) %>% 
                slice(1:20),
            aes(x = receiver, y = tot_air_yards)) +
            geom_bar(
                aes(x = reorder(receiver, -tot_air_yards), 
                    y = tot_air_yards), 
                stat='identity') +
            geom_image(
                aes(image = headshot_url), 
                asp = 16/9,
                nudge_y = 1
            ) +
            theme_cw +
            theme(
                axis.text.x = element_text(angle = 45)
            )
    }, height = 800)
}

# Run the application 
shinyApp(ui = ui, server = server)
