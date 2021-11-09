#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# devtools::install_github("nflverse/nflfastR")
# devtools::install_github("guga31bb/nfl4th")
# devtools::install_github("saiemgilani/cfbfastR")
# devtools::install_github("dynastyprocess/ffscrapr")
# devtools::install_github("jthomasmock/espnscrapeR")
# devtools::install_github("colinifer/initR", auth_token = Sys.getenv('authtoken'))
# devtools::install_github('gregce/ipify')
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

current_season <- fx.get_year()
year <- fx.get_year()

# Create standard objects -------------------------------------------------

source('plots/assets/plot_theme.R', echo = F)
# source('data/fastr_scrape.R')
# source('https://raw.githubusercontent.com/nflverse/nflfastR/master/R/utils.R')
# source('https://raw.githubusercontent.com/nflverse/nflfastR/master/R/aggregate_game_stats.R')
# source('https://raw.githubusercontent.com/nflverse/nflfastR/master/R/helper_add_xyac.R')
# source('https://raw.githubusercontent.com/nflverse/nflfastR/master/R/helper_add_nflscrapr_mutations.R')
source('data/fastr_mods.R')
# source('data/cfb_fastr_mods.R')

# Create variables & dataframes -------------------------------------------

# nflfastR data
con <- fx.db_con(x.host = 'localhost')
# update_roster_db(season = year, db_connection = fx.db_con(x.host = 'localhost'))
roster_df <- tbl(con, 'nflfastR_rosters') %>% 
    filter(season == year) %>%
    collect()
# update_schedule_db(season = year, db_connection = fx.db_con(x.host = 'localhost'))
schedule_df <- tbl(con, 'nflfastR_schedule') %>% 
    filter(season == year) %>% 
    collect()
# update_trades_db(season = year, db_connection = fx.db_con(x.host = 'localhost'))
trades_df <- tbl(con, 'nflfastR_trades') %>% 
    filter(season == year) %>% 
    collect()
# update_draft_db(season = year, db_connection = fx.db_con(x.host = 'localhost'))
draft_df <- tbl(con, 'nflfastR_draft') %>% 
    filter(season == year) %>% 
    collect()

pbp_df <- tbl(con, 'nflfastR_pbp') %>% 
    filter(season == year) %>% 
    collect()
dbDisconnect(con)


source('init_ff.R')
# Rosters
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
                     league = 'Family')) %>% 
    left_join(roster_df %>% 
                  select(
                      gsis_id,
                      espn_id),
              by = c('player_id' = 'espn_id'))

player_stats <- pbp_df %>% 
    calculate_player_stats_mod() 

ff_free_agents <- fx.ff_free_agents(player_stats, 'Beep Boop')

player_stats_weekly <- pbp_df %>% 
    calculate_player_stats_mod(weekly = TRUE)

u.players <- player_stats_weekly %>% 
    select(player_id, player_name, recent_team) %>% 
    unique() %>% 
    mutate(
        full_player_info = paste(recent_team, player_name)
    ) %>% 
    arrange(full_player_info) %>% 
    pull(full_player_info)

ui <- fluidPage(
    titlePanel(),
    sidebarLayout(
        sidebarPanel(
            selectizeInput(
                'player_comparisons', 
                'Select Players', 
                choices = u.players, 
                multiple = TRUE
            )
        ),
        mainPanel(plotOutput('wopr_plot', width = '900px'))
    )
    # selectizeInput(
    #     'player_comparisons', 
    #     'Select Players', 
    #     choices = u.players, 
    #     multiple = TRUE
    #     ),
    # plotOutput('wopr_plot', width = '900px')
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    updateSelectizeInput(
        session, 
        inputId = 'player_comparisons', 
        choices = u.players, 
        server = TRUE
        )
    
    # u.player_comparisons <- input$player_comparisons
    
    # output$wopr_plot <- player_stats_weekly %>%
    #     left_join(
    #         roster_df %>%
    #             select(
    #                 gsis_id,
    #                 position
    #             ),
    #         by = c('player_id' = 'gsis_id')) %>% 
    #     mutate(
    #         full_player_info = paste(recent_team, player_name)
    #     ) %>% 
    #     filter(position == 'WR' &
    #                full_player_info %in% u.player_comparisons) %>%
    #     group_by(player_id) %>%
    #     mutate(
    #         mean_wopr = mean(wopr)
    #     ) %>%
    #     ggplot(aes(x = week, y = wopr)) +
    #     geom_line(aes(group = player_id, color = player_name)) +
    #     # geom_smooth(aes(group = player_id, color = player_name), se = FALSE) +
    #     geom_line(aes(y = mean_wopr, group = player_id, color = player_id), label = player_name)
}

# Run the application 
shinyApp(ui = ui, server = server)
