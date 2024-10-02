#' Plot offensive series results by team
#'
#' @param pbp A dataframe containing the NFL play-by-play data
#'
#' @return A ggplot object containing the plot
#'
#' @examples
#' series_results_plot <- fx.plot_series_results()
#' # Save the plot to a file
#' ggsave(series_results_plot, file = 'plots/team_series_results.png')
#'
#' @export

fx.plot_series_results <- function(pbp = pbp_df) {
  success <- c("First down", "Touchdown", "QB kneel")
  turnovers <- c("Turnover", "Turnover on downs", "Safety")
  field_goals <- c("Field goal", "End of half")
  
  df <- pbp_df |>
    arrange(season,
            game_id,
            play_id) |>
    filter(play == 1
           & series_result != "QB kneel") |>
    select(game_id,
           posteam,
           drive,
           series,
           series_result) |>
    unique() |>
    group_by(game_id, posteam) |>
    mutate(series = row_number()) |>
    mutate(
      series_result = case_when(
        series_result %in% success ~ "First down or Touchdown",
        series_result %in% turnovers ~ "Turnover",
        series_result %in% field_goals ~ "Field goal",
        series_result == "Punt" ~ "Punt",
        TRUE ~ NA
      )
    ) |>
    filter(!is.na(series_result)) |>
    group_by(posteam, series_result) |>
    count() |>
    # pivot_wider(names_from = "series_result", values_from = "n") |>
    # janitor::clean_names() |>
    group_by(posteam) |>
    mutate(
      n_percent = scales::percent(round(n / sum(n), 2)),
      n = round(n / sum(n), 4),
      series_result = factor(
        series_result,
        levels = c("Turnover", "Punt", "Field goal", "First down or Touchdown")
      ),
    ) |>
    left_join(teams_colors_logos |>
                select(team_abbr,
                       team_color),
              by = c('posteam' = 'team_abbr')) |>
    arrange(posteam,
            match(
              series_result,
              c("First down or Touchdown", "Field goal", "Punt", "Turnover")
            ))
  
  team_order <- df |>
    filter(series_result == "First down or Touchdown") |>
    arrange(n_percent) |>
    pull(posteam)
  
  p <- df |>
    ggplot(aes(x = n, y = factor(posteam, team_order))) +
    geom_bar(aes(fill = series_result),
             position = 'fill',
             stat = 'identity',
             width=.5) +
    stat_identity(
      geom = "shadow_text",
      aes(label = n_percent),
      position = position_fill(vjust = 0.5),
      size=2,
      color = "white",
      family = "Montserrat"
    ) +
    scale_x_continuous(labels = scales::percent) +
    theme_cw_dark +
    guides(fill = guide_legend(title = "Series Results")) +
    labs(x='Offensive Series Results',
         y='Team') +
    theme(
      axis.text.y = nflplotR::element_nfl_logo(size = 0.5),
      axis.title = element_blank(),
      legend.position = "top",
      legend.key.size = unit(.25, 'cm'),
      legend.title = element_text(size=4),
      legend.text = element_text(size=4),
      NULL
    )
  
  brand_plot(
    p,
    asp = 10 / 16,
    save_name = glue('plots/desktop/team_series/team_series_results_{current_season}.png'),
    data_home = 'Data: @nflfastR',
    fade_borders = ''
  )
  
  return(p)
}
