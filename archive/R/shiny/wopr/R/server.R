
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
