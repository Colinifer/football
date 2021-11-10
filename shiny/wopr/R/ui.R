
wopr_ui <- fluidPage(
  titlePanel('Player WOPR'),
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        'player_comparisons', 
        'Select Players', 
        choices = u.players, # from data_load.R
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
