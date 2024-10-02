#' Create a plot of EPA per play for all NFL teams
#'
#' @param data A dataframe containing the NFL play-by-play data
#' @param season The season for which to create the plot
#'
#' @return A ggplot object containing the plot
#'
#' @examples
#' team_tiers_plot <- fx.epa_team_tiers()
#'
#' # Save the plot to a file
#' ggsave(team_tiers_plot, file = 'plots/team_tiers_2023.png')
#'
#' @export

fx.plot_epa_team_tiers <- function(con = fx.db_con(x.host = 'localhost'),
                                   x.season = current_season
) {
  # Start timer
  start_time <- Sys.time()
  on.exit(dbDisconnect(con))
  
  
  # Plot Annotations --------------------------------------------------------
  res <- 800 # size of exported plots
  off_def_slope = -1.5 # for offense & defense comparison (offense is 1.5x more important than defense)
  pass_rush_slope <- -1 # for pass & rush comparison
  
  plot_width <- 16
  plot_height <- 10
  plot_aspect_ratio <- plot_width/plot_height
  
  translate_angle <- 90-(plot_height/sqrt(plot_height^2+plot_width^2)*100)
  # translate_angle <- 90-(atan(2/3)*100)
  qb_min <- 320 # min # of qb plays
  
  ab_intercepts <- seq(-1,1,by=0.1)
  
  
  # Prep data ---------------------------------------------------------------
  density_pbp <- tbl(con, 'nflfastR_pbp') |> 
    filter(season_type == 'REG'
           & !is.na(posteam)
           & (rush == 1 | pass == 1)) |> 
    select(season,
           week,
           game_id,
           posteam,
           defteam,
           down,
           play_type,
           pass,
           rush,
           epa,
           success,
           home_team,
           away_team,
           NULL
    ) |> 
    collect()
  
  pbp <- density_pbp |>
    filter(season == x.season & 
             !is.na(posteam) & 
             (rush == 1 | pass == 1)) |> 
    select(season,
           week,
           game_id,
           posteam,
           defteam,
           down,
           play_type,
           pass,
           rush,
           epa,
           success,
           home_team,
           away_team,
           NULL
    )
  
  n_week <- fx.n_week(pbp)
  
  # If week < 10, use current weeks in season
  time_series <- dplyr::if_else(pbp |>
                                  select(week) |>
                                  max() - 1 < 10,
                                pbp |>
                                  select(week) |>
                                  max() - 2,
                                10)
  
  offense <- density_pbp |>
    filter(!is.na(posteam) & (rush == 1 | pass == 1)) |>
    group_by(posteam, season) |>
    summarize(
      n_pass = sum(pass, na.rm = T),
      n_rush = sum(rush, na.rm = T),
      epa_per_pass = sum(epa * pass, na.rm = T) / n_pass,
      epa_per_rush = sum(epa * rush, na.rm = T) / n_rush,
      success_per_pass = sum(pass * epa > 0, na.rm = T) / n_pass,
      success_per_rush = sum(rush * epa > 0, na.rm = T) / n_rush,
      off_epa = mean(epa, na.rm = T),
      off_success = mean(success, na.rm = T)
    )
  
  early_down_offense <- density_pbp |>
    filter(!is.na(posteam) & down < 3 & (rush == 1 | pass == 1)) |>
    group_by(posteam, season) |>
    summarize(
      early_down_off_n_pass = sum(pass, na.rm = T),
      early_down_off_n_rush = sum(rush, na.rm = T),
      early_down_off_epa_per_pass = sum(epa * pass, na.rm = T) / early_down_off_n_pass,
      early_down_off_epa_per_rush = sum(epa * rush, na.rm = T) / early_down_off_n_rush,
      early_down_off_success_per_pass = sum(pass * epa > 0, na.rm = T) / early_down_off_n_pass,
      early_down_off_success_per_rush = sum(rush * epa > 0, na.rm = T) / early_down_off_n_rush,
      early_down_off_epa = mean(epa, na.rm = T),
      early_down_off_success = mean(success, na.rm = T)
    )
  
  defense <- density_pbp |>
    filter(!is.na(posteam) & (rush == 1 | pass == 1)) |>
    group_by(defteam, season) |>
    summarize(
      def_n_pass = sum(pass, na.rm = T),
      def_n_rush = sum(rush, na.rm = T),
      def_epa_per_pass = sum(epa * pass, na.rm = T) / def_n_pass,
      def_epa_per_rush = sum(epa * rush, na.rm = T) / def_n_rush,
      def_success_per_pass = sum(pass * epa > 0, na.rm = T) / def_n_pass,
      def_success_per_rush = sum(rush * epa > 0, na.rm = T) / def_n_rush,
      def_epa = mean(epa, na.rm = T),
      def_success = mean(success, na.rm = T)
    )
  
  early_down_defense <- density_pbp |>
    filter(!is.na(defteam) & down < 3 & (rush == 1 | pass == 1)) |>
    group_by(defteam, season) |>
    summarize(
      early_down_def_n_pass = sum(pass, na.rm = T),
      early_down_def_n_rush = sum(rush, na.rm = T),
      early_down_def_epa_per_pass = sum(epa * pass, na.rm = T) / early_down_def_n_pass,
      early_down_def_epa_per_rush = sum(epa * rush, na.rm = T) / early_down_def_n_rush,
      early_down_def_success_per_pass = sum(pass * epa > 0, na.rm = T) /
        early_down_def_n_pass,
      early_down_def_success_per_rush = sum(rush * epa > 0, na.rm = T) /
        early_down_def_n_rush,
      early_down_def_epa = mean(epa, na.rm = T),
      early_down_def_success = mean(success, na.rm = T)
    )
  
  chart_all <- offense |> 
    inner_join(defense, by = c('season', 'posteam' = 'defteam')) |> 
    inner_join(early_down_offense, by = c('season', 'posteam')) |> 
    inner_join(early_down_defense, by = c('season', 'posteam' = 'defteam')) |> 
    left_join(teams_colors_logos, by = c('posteam' = 'team_abbr')) |> 
    mutate(epa_y_intercept = def_epa-(-off_def_slope*off_epa),
           epa_early_down_intercept = early_down_def_epa-(-off_def_slope*early_down_off_epa))
  
  # Standard Team Tiers -----------------------------------------------------
  
  epa_percentiles <- quantile(chart_all$epa_y_intercept, 
                              c(0.1, 0.25, 0.5, 0.75, 0.9))
  
  annot_x_bound <- min(c(min(
    filter(chart_all, season == x.season)$off_epa
  ) - 0.02,
  -0.21
  ))
  
  annot_y_bound <- min(c((min(
    filter(chart_all, season == x.season)$def_epa
  ) - 0.02),
  -0.15
  ))
  
  epa_annotations <- enframe(epa_percentiles,
                             name = 'percentile',
                             value = 'percentile_y_intercept') |>
    mutate(
      annotation_x = (annot_y_bound / -off_def_slope) - (percentile_y_intercept / off_def_slope)-0.01,
      annotation_x = case_when(annotation_x < annot_x_bound
                               ~ annot_x_bound,
                               TRUE
                               ~ annotation_x
      ),
      annotation_y = (-off_def_slope * annotation_x) + -percentile_y_intercept
    ) |> 
    bind_cols(
      label = c('p10 (Top 3 Pick)',
        'p25 (Top 10 Pick)',
        'Mean',
        'p75 (Playoff Contender)',
        'p90 (Super Bowl Contender)'),
      label2 = c('p10',
                'p25',
                'p50',
                'p75',
                'p90')
    )
  
  p <- chart_all |> 
    ggplot(aes(x = off_epa, y = def_epa)) + 
    # add density of previous seasons
    stat_density_2d(aes(alpha = ..level../2,
                        fill = after_stat(level)),
                    bins = 6,
                    n = 20,
                    # color = color_cw['grey'],
                    geom = "polygon") + 
    scale_fill_distiller(palette = 'Greys') + 
    geom_abline(slope = off_def_slope, 
                intercept = epa_percentiles, 
                alpha = 0.5,
                color = color_cw['black']
    ) + 
    geom_hline(yintercept = mean(chart_all$def_epa, na.rm = T), color = 'red', linetype = 'dashed') + 
    geom_vline(xintercept =  mean(chart_all$off_epa, na.rm = T), color = 'red', linetype = 'dashed') + 
    geom_text(data = epa_annotations,
              aes(x = annotation_x-0.015,
                  y = annotation_y,
                  label = label2),
              size = 1.75,
              color = color_cw['grey'],
              hjust = 0,
              family = 'Montserrat',
              alpha = 0.75
    #           angle = 
    #             atan(
    #               # slope
    #               off_def_slope *
    #                 # aspect ratio of plot:
    #                 # unit(0, 'npc') %>% grid::convertY('native', valueOnly = T) /
    #                 # unit(1, 'npc') %>% grid::convertX('native', valueOnly = T) /
    #                 # par('fin')[2] /
    #                 # par('fin')[1] /
    #                 # par('plt')[4]-par('plt')[3] / 
    #                 # par('plt')[2]-par('plt')[1] / 
    #                 par('pin')[2] /
    #                 par('pin')[1] #/
    #                 # ratio of y-range to x-range of plot:
    #                 # (
    #                 #   max(filter(chart_all, season == x.season)$def_epa) - annot_y_bound /
    #                 #     max(filter(chart_all, season == x.season)$off_epa) - annot_x_bound
    #                 # )
    #             ) * 180 / pi,
    ) +
    nflplotR::geom_nfl_logos(data = chart_all |> 
                               filter(season == x.season),
                             aes(team_abbr = posteam), width = 0.05, alpha = 0.9) + 
    labs(x = 'Offense EPA/play',
         y = 'Defense EPA/play',
         # caption = 'Data: @nflscrapR',
         title = glue('{x.season} NFL Team Tiers'),
         subtitle = glue('Offense and defense EPA per play through week {n_week}')) + 
    scale_y_reverse() + 
    theme_cw_dark + 
    theme(
      axis.title.y = element_text(angle = 90),
      legend.position = "none",
      legend.justification = c(1, 1) ,
      plot.title = element_text(size = 16),
      panel.grid.major = element_blank()
    )
  
  brand_plot(
    p,
    asp = plot_aspect_ratio,
    save_name = glue('plots/desktop/team_tiers/team_tiers_{x.season}.png'),
    data_home = 'Data: @nflfastR',
    fade_borders = ''
  )
  
  
  # Rotated Team Tiers ------------------------------------------------------

  # Rotate ---
  off_epa_min <- -.3
  off_epa_max <- .3
  def_epa_min <- -.3
  def_epa_max <- .3
  
  # set rotation to 45 degrees
  rotation <- 45
  
  # make plot
  p <- chart_all |> 
    ggplot(aes(x = off_epa, y = def_epa)) + 
    # add density of previous seasons
    stat_density_2d(aes(alpha = ..level..,
                        fill = after_stat(level)),
                    bins = 5,
                    n = 20,
                    # color = color_cw['grey'],
                    geom = "polygon") + 
    scale_fill_distiller(palette = 'Greys') + 
    # add color blocking
    annotate('rect', xmin = (off_epa_max + off_epa_min) / 2, xmax = off_epa_max,
             ymin = def_epa_min, ymax = (def_epa_max + def_epa_min) / 2, 
             fill= color_cw['green'], alpha = .1, color = 'transparent') +
    annotate('rect', xmin = off_epa_min, xmax = (off_epa_max + off_epa_min) / 2,
             ymin = (def_epa_max + def_epa_min) / 2, ymax = def_epa_max, 
             fill= color_cw['red'], alpha = .1, color = 'transparent') +
    annotate('rect', xmin = (off_epa_max + off_epa_min) / 2, xmax = off_epa_max,
             ymin = (def_epa_max + def_epa_min) / 2, ymax = def_epa_max, 
             fill= color_cw['blue'], alpha = .1, color = 'transparent')  +
    annotate('rect', xmin = off_epa_min, xmax = (off_epa_max + off_epa_min) / 2,
             ymin = def_epa_min, ymax = (def_epa_max + def_epa_min) / 2, 
             fill= color_cw['blue'], alpha = .1, color = 'transparent') +
    # add team logos
    nflplotR::geom_nfl_logos(data = chart_all |> 
                               filter(season == max(season)),
                             aes(team_abbr = posteam), 
                             width = 0.08, 
                             alpha = 0.9, 
                             angle = -1*rotation
    ) +
    scale_y_reverse(limits = c(.3, -.3), breaks = seq(-.3, .3, .15), labels = scales::number_format(accuracy = 0.01)) +  
    scale_x_continuous(limits = c(-.3, .3), breaks = seq(.3, -.3, -.15), labels = scales::number_format(accuracy = 0.01)) +
    coord_equal() +
    # add axis labels
    labs(
      x = 'Offense EPA/play',
      y = 'Defense EPA/play'
    ) +
    theme_cw_dark +
    theme(
      legend.position = "none",
      axis.text.x = element_text(
        angle = (-1 * rotation),
        hjust = 0.5,
        margin = margin(t = -5)
      ),
      axis.text.y = element_text(
        angle = (-1 * rotation),
        hjust = 0.4,
        vjust = -.005,
        margin = margin(r = -5)
      ),
      axis.title.x = element_text(
        size = 12,
        #angle=(-1 * rotation + 45),
        vjust = 0.5,
        margin = margin(t = 10),
        face = 'bold'
      ),
      axis.title.y = element_text(
        size = 12,
        angle = (-1 * rotation - 45),
        hjust = 0.5,
        margin = margin(r = 10),
        face = 'bold'
      ),
      plot.margin = margin(1, .5, .5, 0, unit = 'in'),
      plot.background = element_rect(color = 'transparent',
                                     fill = 'transparent'),
      panel.background = element_rect(color = 'transparent',
                                      fill = 'transparent'),
      panel.border = element_rect(color = 'transparent'),
      panel.grid.minor = element_blank()
    ) +
    # hack together a few chart guides (ie, 'Good D, Bad D')
    geom_richtext(
      aes(x = 0.225, y = 0.225, label = 'Good O, Bad D'),
      angle = -1 * rotation,
      size = 3,
      family = 'Montserrat',
      fontface = 'bold',
      color = 'black',
      fill = color_cw['blue'],
      label.size = 0
    ) +
    geom_richtext(
      aes(
        x = -0.225,
        y = -0.225,
        label = 'Bad O, Good D'
      ),
      angle = -1 * rotation,
      size = 3,
      family = 'Montserrat',
      fontface = 'bold',
      color = 'black',
      fill = color_cw['blue'],
      label.size = 0
    ) +
    geom_richtext(
      aes(x = -0.225, y = 0.225, label = 'Bad O, Bad D'),
      angle = -1 * rotation,
      size = 3,
      family = 'Montserrat',
      fontface = 'bold',
      color = 'black',
      fill = color_cw['red'],
      label.size = 0
    ) +
    geom_richtext(
      aes(x = 0.225, y = -0.225, label = 'Good O, Good D'),
      angle = -1 * rotation,
      size = 3,
      family = 'Montserrat',
      fontface = 'bold',
      color = 'black',
      fill = color_cw['green'],
      label.size = 0
    )
  
  p_background <- ggplot() +
    labs(
      title = glue('{current_season} NFL Team Tiers'),
      subtitle = glue('Offense and defense EPA per play through week {n_week}')
    ) +
    theme_cw_dark +
    theme(
      legend.position = "none",
      legend.justification = c(1, 1) ,
      panel.background = element_rect(fill = color_cw[1],
                                      color = 'transparent'),
      plot.title = element_text(
        family = 'Chivo',
        face = 'bold',
        size = 16
      )
    )
  
  # save plot
  png(
    glue('plots/desktop/team_tiers/team_tiers_diamond_{year}.png'),
    res = 300,
    width = 6,
    height = 6,
    units = 'in',
    bg = color_cw[1]
  )
  
  print(p_background)
  print(p, vp = viewport(
    angle = rotation,
    width = unit(6, 'in'),
    height = unit(6, 'in')
  )) |>
    suppressWarnings()
  
  dev.off()
  

# notes -------------------------------------------------------------------

  epa_percentiles <- quantile(chart_all$epa_y_intercept, 
                              c(0.1, 0.25, 0.5, 0.75, 0.9))
  
  annot_x_bound <- min(c(min(
    filter(chart_all, season == x.season)$off_epa
  ) - 0.02,
  -0.21
  ))
  
  annot_y_bound <- min(c((min(
    filter(chart_all, season == x.season)$def_epa
  ) - 0.02),
  -0.15
  ))
  
  epa_annotations <- enframe(epa_percentiles,
                             name = 'percentile',
                             value = 'percentile_y_intercept') |>
    mutate(
      annotation_x = (annot_y_bound / -off_def_slope) - (percentile_y_intercept / off_def_slope)-0.01,
      annotation_x = case_when(annotation_x < annot_x_bound
                               ~ annot_x_bound,
                               TRUE
                               ~ annotation_x
      ),
      annotation_y = (-off_def_slope * annotation_x) + -percentile_y_intercept
    )  
  
  # Early Down Team Tiers ---------------------------------------------------
  
  epa_early_down_percentiles <- quantile(chart_all$epa_early_down_intercept, 
                              c(0.1, 0.25, 0.5, 0.75, 0.9))
  
  annot_x_bound <- min(c(min(
    filter(chart_all, season == x.season)$early_down_off_epa
  ) - 0.02,
  -0.21
  ))
  
  annot_y_bound <- min(c((min(
    filter(chart_all, season == x.season)$early_down_def_epa
  ) - 0.02),
  -0.15
  ))
  
  min_def_epa <- chart_all |> 
    filter(season == max(season)) |> 
    ungroup() |> 
    filter(def_epa == min(def_epa)) |> 
    pull(def_early_down_epa)
  
  epa_annotations <- (min_def_epa/off_def_slope)-(epa_percentiles/off_def_slope)
  
  
  p <- chart_all |> 
    ggplot(aes(x = early_down_off_epa, y = def_early_down_epa)) +
    # add density of previous seasons
    stat_density_2d(aes(alpha = ..level..,
                        fill = after_stat(level)),
                    bins = 5,
                    n = 20,
                    # color = color_cw['grey'],
                    geom = "polygon") + 
    scale_fill_distiller(palette = 'Greys') + 
    geom_abline(slope=off_def_slope, intercept=ab_intercepts, alpha=.2) +
    nflplotR::geom_nfl_logos(data = chart_all |> 
                               filter(season == max(season)),
                             aes(team_abbr = posteam), 
                             width = 0.05, 
                             alpha = 0.9
    ) +
    geom_hline(yintercept = mean(chart_all$def_early_down_epa, na.rm = T), color = 'red', linetype = 'dashed') +
    geom_vline(xintercept =  mean(chart_all$early_down_off_epa, na.rm = T), color = 'red', linetype = 'dashed') +
    labs(x = 'Offense early down EPA/play',
         y = 'Defense early down EPA/play',
         # caption = 'Data: @nflscrapR',
         title = glue('{current_season} NFL Team Tiers'),
         subtitle = glue('Offense and defense early down EPA per play through week {n_week}')) +
    scale_y_reverse() +
    theme_cw_dark +
    theme(
      axis.title.y = element_text(angle = 90),
      legend.position = "none",
      legend.justification = c(1, 1) ,
      plot.title = element_text(size = 16),
      #panel.grid.minor = element_blank()
    )
  
  brand_plot(p, asp = 16/10, save_name = glue('plots/desktop/team_tiers/early_down_team_tiers_{current_season}.png'), data_home = 'Data: @nflfastR', fade_borders = '')
  
  # Pass v Rush EPA
  p <- chart_all |> 
    ggplot(aes(x = epa_per_pass, y = epa_per_rush)) +
    # add density of previous seasons
    stat_density_2d(aes(alpha = ..level..,
                        fill = after_stat(level)),
                    bins = 5,
                    # color = color_cw['grey'],
                    geom = "polygon") + 
    scale_fill_distiller(palette = 'Greys') + 
    # geom_image(aes(image = team_logo_espn), size = 0.05, asp = 16/9) +
    geom_abline(slope=pass_rush_slope, intercept=ab_intercepts, alpha=.2) +
    nflplotR::geom_nfl_logos(data = chart_all |> 
                               filter(season == max(season)),
                             aes(team_abbr = posteam), 
                             width = 0.05, 
                             alpha = 0.9
    ) +
    geom_hline(yintercept = mean(chart_all$epa_per_rush), color = 'red', linetype = 'dashed') +
    geom_vline(xintercept =  mean(chart_all$epa_per_pass), color = 'red', linetype = 'dashed') +
    labs(x = 'Pass EPA/play',
         y = 'Rush EPA/play',
         # caption = 'Data: @nflscrapR',
         title = glue('{current_season} NFL Offense Team Tiers'),
         subtitle = glue('Offense passing and rushing EPA per play through week {n_week}')) +
    theme_cw_dark +
    theme(
      axis.title.y = element_text(angle = 90),
      legend.position = "none",
      legend.justification = c(1, 1) ,
      plot.title = element_text(size = 16),
      #panel.grid.minor = element_blank()
    )
  
  brand_plot(p, asp = 16/10, save_name = glue('plots/desktop/team_tiers/team_off_pass_and_rush_tiers_{current_season}.png'), data_home = 'Data: @nflfastR', fade_borders = '')
  
  # Need to add Defense pass and rush epa/play
  
  p <- chart_all |> 
    ggplot(aes(x = def_epa_per_pass, y = def_epa_per_rush)) +
    # add density of previous seasons
    stat_density_2d(aes(alpha = ..level..,
                        fill = after_stat(level)),
                    bins = 5,
                    # color = color_cw['grey'],
                    geom = "polygon") + 
    scale_fill_distiller(palette = 'Greys') + 
    geom_abline(slope=pass_rush_slope, intercept=ab_intercepts, alpha=.2) +
    nflplotR::geom_nfl_logos(data = chart_all |> 
                               filter(season == max(season)),
                             aes(team_abbr = posteam), 
                             width = 0.05, 
                             alpha = 0.9
    ) +
    geom_hline(yintercept = mean(chart_all$def_epa_per_rush), color = 'red', linetype = 'dashed') +
    geom_vline(xintercept =  mean(chart_all$def_epa_per_pass), color = 'red', linetype = 'dashed') +
    labs(x = 'Defense Pass EPA/play',
         y = 'Defense Rush EPA/play',
         # caption = 'Data: @nflscrapR',
         title = glue('{current_season} NFL Defense Team Tiers'),
         subtitle = glue('Defense passing and rushing EPA per play through week {n_week}')) +
    scale_x_reverse() +
    scale_y_reverse() +
    theme_cw_dark +
    theme(
      axis.title.y = element_text(angle = 90),
      legend.position = "none",
      legend.justification = c(1, 1) ,
      plot.title = element_text(size = 16),
      #panel.grid.minor = element_blank()
    )
  
  brand_plot(p, asp = 16/10, save_name = glue('plots/desktop/team_tiers/team_def_pass_and_rush_tiers_{current_season}.png'), data_home = 'Data: @nflfastR', fade_borders = '')
  
  
  
  # Adjust EPA --------------------------------------------------------------
  library(glmnet)
  opp_adj_y <- chart_all$off_epa
  opp_adj_x <- chart_all[, c('posteam', 'off_epa', 'def_epa')]
  opp_adjustments <- glmnet(opp_adj_x, opp_adj_y, alpha = 0)
  
  if (max(pbp$week) >= 10) {
    # https://www.opensourcefootball.com/posts/2020-08-20-adjusting-epa-for-strenght-of-opponent/
    epa_data <- pbp |>
      filter(posteam != '') |> 
      # dplyr::filter(!is.na(epa), !is.na(ep), !is.na(posteam), play_type == 'pass' | play_type == 'run') |>
      dplyr::filter(!is.na(posteam)) |>
      dplyr::group_by(game_id, season, week, posteam, home_team) |>
      dplyr::summarise(
        off_epa = mean(epa, na.rm = T),
      ) |>
      dplyr::left_join(pbp |>
                         filter(play_type == 'pass' | play_type == 'run') |>
                         dplyr::group_by(game_id, season, week, defteam, away_team) |>
                         dplyr::summarise(def_epa = mean(epa, na.rm = T)),
                       by = c('game_id', 'posteam' = 'defteam', 'season', 'week'),
                       all.x = T
      ) |>
      dplyr::mutate(opponent = ifelse(posteam == home_team, away_team, home_team)) |>
      dplyr::select(game_id, season, week, home_team, away_team, posteam, opponent, off_epa, def_epa)
    
    offense <- pbp |>
      filter(!is.na(epa) & !is.na(posteam)) |> 
      group_by(posteam, season) |>
      summarize(
        n_pass=sum(pass, na.rm = T),
        n_rush=sum(rush, na.rm = T),
        epa_per_pass=sum(epa*pass, na.rm = T)/n_pass,
        epa_per_rush=sum(epa*rush, na.rm = T)/n_rush,
        success_per_pass=sum(pass*epa>0, na.rm = T)/n_pass,
        success_per_rush=sum(rush*epa>0, na.rm = T)/n_rush,
        off_epa=mean(epa, na.rm = T),
        off_success=mean(success, na.rm = T)
      )
    
    # Construct opponent dataset and lag the moving average of their last ten games.
    opponent_data <- epa_data |>
      dplyr::select(-opponent) |>
      dplyr::rename(
        opp_off_epa = off_epa,
        opp_def_epa = def_epa
      ) |>
      dplyr::group_by(posteam) |>
      dplyr::arrange(season, week) |>
      dplyr::mutate(
        opp_def_epa = pracma::movavg(opp_def_epa, n = time_series - 1, type = 's'),
        opp_def_epa = dplyr::lag(opp_def_epa),
        opp_off_epa = pracma::movavg(opp_off_epa, n = time_series - 1, type = 's'),
        opp_off_epa = dplyr::lag(opp_off_epa)
      )
    
    # Merge opponent data back in with the weekly epa data
    epa_data <- epa_data |>
      left_join(
        opponent_data,
        by = c('game_id', 'season', 'week', 'home_team', 'away_team', 'opponent' = 'posteam'),
        all.x = TRUE
      )
    
    epa_data <- epa_data |>
      dplyr::left_join(epa_data |>
                         dplyr::filter(posteam == home_team) |>
                         dplyr::group_by(season, week) |>
                         dplyr::summarise(
                           league_mean = mean(off_epa + def_epa)
                         ) |>
                         dplyr::ungroup() |>
                         dplyr::group_by(season) |>
                         dplyr::mutate(
                           league_mean = lag(pracma::movavg(league_mean, n = as.integer(time_series), type = 's'), ) # We lag because we need to know the league mean up to that point in the season
                         ),
                       by = c('season', 'week'),
                       all.x = TRUE
      )
    
    # Adjust EPA
    epa_data <- epa_data |>
      dplyr::mutate(
        off_adjustment_factor = ifelse(!is.na(league_mean), league_mean - opp_def_epa, 0),
        def_adjustment_factor = ifelse(!is.na(league_mean), league_mean - opp_off_epa, 0),
        adjusted_off_epa = off_epa + off_adjustment_factor,
        adjusted_def_epa = def_epa + def_adjustment_factor,
      )
    
    chart_all_adj_epa <- epa_data |> 
      filter(season == current_season) |> 
      arrange(posteam) |> 
      group_by(posteam) |> 
      summarize(
        adjusted_off_epa = mean(adjusted_off_epa, na.rm = T),
        adjusted_def_epa = mean(adjusted_def_epa, na.rm = T)
      ) |> 
      # filter(row_number() == n()) |> 
      left_join(teams_colors_logos, by = c('posteam' = 'team_abbr'))
    
    p <- chart_all_adj_epa |> 
      ggplot(aes(x = adjusted_off_epa, y = adjusted_def_epa)) +
      # geom_image(aes(image = team_logo_espn), size = 0.05, asp = 16/10) +
      geom_hline(yintercept = mean(chart_all_adj_epa$adjusted_def_epa, na.rm = T), color = 'red', linetype = 'dashed') +
      geom_vline(xintercept =  mean(chart_all_adj_epa$adjusted_off_epa, na.rm = T), color = 'red', linetype = 'dashed') +
      nflplotR::geom_nfl_logos(data = chart_all |> 
                                 filter(season == max(season)),
                               aes(x = adjusted_off_epa,
                                   y = adjusted_def_epa,
                                   label = grob_img_adj(team_logo_espn),
                                   vp.height = 0.08
                               )
      ) + 
      labs(x = 'Adj. Offense EPA/play',
           y = 'Adj. Defense EPA/play',
           # caption = 'Data: @nflscrapR',
           title = glue('{current_season} NFL Adjusted Team Tiers'),
           subtitle = glue('Offense and defense adjusted EPA per play through week {n_week}\nAdjusted for previous matchups')) +
      geom_abline(slope=off_def_slope, intercept=ab_intercepts, alpha=.2) +
      scale_y_reverse() +
      theme_cw_dark +
      theme(
        axis.title.y = element_text(angle = 90),
        legend.position = c(0.99, 0.99),
        legend.justification = c(1, 1) ,
        plot.title = element_text(size = 16),
        #panel.grid.minor = element_blank()
      )
    
    brand_plot(p, asp = 16/10, save_name = glue('plots/desktop/team_tiers/team_tiers_adj_{current_season}.png'), data_home = 'Data: @nflfastR', fade_borders = '')
  }
  
  
  # Next week match-ups -----------------------------------------------------
  if (n_week < 17) {
    matchup_chart_all <- chart_all |>
      left_join(
        matchup_df |> 
          filter(week == n_week + 1) |> 
          select(posteam, oppteam, weekday, gametime),
        by = c('posteam')
      ) |> 
      left_join(chart_all |> 
                  rename(
                    opp_n_pass = n_pass,
                    opp_n_rush = n_rush,
                    opp_epa_per_pass = epa_per_pass,
                    opp_epa_per_rush = epa_per_rush,
                    opp_success_per_pass = success_per_pass,
                    opp_success_per_rush = success_per_rush,
                    opp_off_epa = off_epa,
                    opp_def_n_pass = def_n_pass,
                    opp_def_n_rush = def_n_rush,
                    opp_def_epa_per_pass = def_epa_per_pass,
                    opp_def_epa_per_rush = def_epa_per_rush,
                    opp_def_success_per_pass = def_success_per_pass,
                    opp_def_success_per_rush = def_success_per_rush,
                    opp_def_epa = def_epa,
                    opp_def_success = def_success,
                    opp_team_name = team_name,
                    opp_team_id = team_id,
                    opp_team_nick = team_nick,
                    opp_team_color = team_color,
                    opp_team_color2 = team_color2,
                    opp_team_color3 = team_color3,
                    opp_team_color4 = team_color4,
                    opp_team_logo_wikipedia = team_logo_wikipedia,
                    opp_team_logo_espn = team_logo_espn
                  ),
                by = c('oppteam' = 'posteam')
      ) |>
      filter(!is.na(oppteam ))
    
    p <- matchup_chart_all |> 
      ggplot(aes(x = off_epa, y = opp_def_epa)) +
      # geom_image(aes(image = team_logo_espn), size = 0.05, asp = 16/9) +
      geom_grob(aes(
        x = off_epa,
        y = opp_def_epa,
        label = grob_img_adj(team_logo_espn),
        vp.height = 0.08
      )) +
      geom_hline(yintercept = mean(matchup_chart_all$opp_def_epa), color = 'red', linetype = 'dashed') +
      geom_vline(xintercept =  mean(matchup_chart_all$off_epa), color = 'red', linetype = 'dashed') +
      labs(x = 'Offense EPA/play',
           y = 'Opponent Defense EPA/play',
           # caption = 'Data: @nflscrapR',
           title = glue('{current_season} NFL Team Tiers Matchups through Week {n_week}'),
           subtitle = glue('Team offense and week {n_week + 1} opponent defense EPA per play')) +
      geom_abline(slope=off_def_slope, intercept=ab_intercepts, alpha=.2) +
      theme_cw_dark +
      theme(
        axis.title.y = element_text(angle = 90),
        legend.position = c(0.99, 0.99),
        legend.justification = c(1, 1) ,
        plot.title = element_text(size = 16),
        #panel.grid.minor = element_blank()
      )
    
    brand_plot(p, asp = 16/10, save_name = glue('plots/desktop/team_tiers/matchup_team_tiers_{current_season}.png'), data_home = 'Data: @nflfastR', fade_borders = '')
    
    # Passing matchup
    p <- matchup_chart_all |> 
      ggplot(aes(x = epa_per_pass, y = opp_def_epa_per_pass)) +
      # geom_image(aes(image = team_logo_espn), size = 0.05, asp = 16/9) +
      geom_grob(aes(
        x = epa_per_pass,
        y = opp_def_epa_per_pass,
        label = grob_img_adj(team_logo_espn),
        vp.height = 0.08
      )) +
      geom_hline(yintercept = mean(matchup_chart_all$opp_def_epa_per_pass), color = 'red', linetype = 'dashed') +
      geom_vline(xintercept =  mean(matchup_chart_all$epa_per_pass), color = 'red', linetype = 'dashed') +
      labs(x = 'Offense Pass EPA/play',
           y = 'Opponent Defense Pass EPA/play',
           # caption = 'Data: @nflscrapR',
           title = glue('{current_season} NFL Team Tiers Passing Matchups through Week {n_week}'),
           subtitle = glue('Team passing offense and week {n_week + 1} opponent passing defense EPA per play')) +
      geom_abline(slope=off_def_slope, intercept=ab_intercepts, alpha=.2) +
      theme_cw_dark +
      theme(
        axis.title.y = element_text(angle = 90),
        legend.position = c(0.99, 0.99),
        legend.justification = c(1, 1) ,
        plot.title = element_text(size = 16),
        #panel.grid.minor = element_blank()
      )
    
    brand_plot(p, asp = 16/10, save_name = glue('plots/desktop/team_tiers/matchup_pass_team_tiers_{current_season}.png'), data_home = 'Data: @nflfastR', fade_borders = '')
    
    # Rushing matchup
    p <- matchup_chart_all |> 
      ggplot(aes(x = epa_per_rush, y = opp_def_epa_per_rush)) +
      # geom_image(aes(image = team_logo_espn), size = 0.05, asp = 16/9) +
      geom_grob(aes(
        x = epa_per_rush,
        y = opp_def_epa_per_rush,
        label = grob_img_adj(team_logo_espn),
        vp.height = 0.08
      )) +
      geom_hline(yintercept = mean(matchup_chart_all$opp_def_epa_per_rush), color = 'red', linetype = 'dashed') +
      geom_vline(xintercept =  mean(matchup_chart_all$epa_per_rush), color = 'red', linetype = 'dashed') +
      labs(x = 'Offense Rush EPA/play',
           y = 'Opponent Defense Rush EPA/play',
           # caption = 'Data: @nflscrapR',
           title = glue('{current_season} NFL Team Tiers Rushing Matchups through Week {n_week}'),
           subtitle = glue('Team rushing offense and week {n_week + 1} opponent rushing defense EPA per play')) +
      geom_abline(slope=off_def_slope, intercept=ab_intercepts, alpha=.2) +
      theme_cw_dark +
      theme(
        axis.title.y = element_text(angle = 90),
        legend.position = c(0.99, 0.99),
        legend.justification = c(1, 1) ,
        plot.title = element_text(size = 16),
        #panel.grid.minor = element_blank()
      )
    
    brand_plot(p, asp = 16/10, save_name = glue('plots/desktop/team_tiers/matchup_rush_team_tiers_{current_season}.png'), data_home = 'Data: @nflfastR', fade_borders = '')
  }
  end_time <- Sys.time()
  print(end_time - start_time)
}

fx.edpa_team_tiers <- function(data = pbp_df,
                              season = current_season
) {
  # Start timer
  start_time <- Sys.time()
  
  # Prep data ---------------------------------------------------------------
  pbp <- data |>
    filter(season == current_season & 
             season_type == 'REG' &
             !is.na(posteam) & 
             (rush == 1 | pass == 1)) |> 
    arrange(season, game_id, play_id) |> 
    select(season,
           week,
           game_id,
           posteam,
           defteam,
           drive,
           down,
           play_type,
           pass,
           rush,
           epa,
           success,
           home_team,
           away_team,
           series_result,
           NULL
    )
  
  n_week <- fx.n_week(pbp)
  
  # If week < 10, us current weeks in season
  time_series <- dplyr::if_else(pbp |>
                                  select(week) |>
                                  max() - 1 < 10,
                                pbp |>
                                  select(week) |>
                                  max() - 2,
                                10)
  
  ab_intercepts <- seq(-4,4,by=0.5)
  
  # Plot Annotations --------------------------------------------------------
  res <- 800 # size of exported plots
  off_def_slope = -1.5 # for offense & defense comparison (offense is 1.5x more important than defense)
  pass_rush_slope <- -1 # for pass & rush comparison
  qb_min <- 320 # min # of qb plays
  
  offense <- pbp |> 
    group_by(season, posteam, game_id, drive) |> 
    summarise(edpa = sum(epa, na.rm = T)) |> 
    ungroup() |> 
    group_by(posteam) |> 
    summarise(off_edpa = mean(edpa, na.rm = T)) |> 
    arrange(desc(off_edpa)) |> 
    left_join(
      pbp_df |> 
        filter(play_type %in% c("pass", "run")) |> 
        group_by(defteam, game_id, drive) |> 
        summarise(edpa = sum(epa, na.rm = T)) |> 
        ungroup() |> 
        group_by(defteam) |> 
        summarise(def_edpa = mean(edpa, na.rm = T)) |> 
        arrange(desc(def_edpa)),
      by = c('posteam' = 'defteam')
    ) |> 
    ggplot(aes(x = off_edpa, y = def_edpa)) + 
    geom_abline(slope=-1.5, intercept=seq(-3,3,by=0.5), alpha=.2) +
    nflplotR::geom_nfl_logos(aes(team_abbr = posteam), width = 0.05, alpha = 0.9) +
    scale_y_reverse() + 
    theme_cw_dark +
    theme(
      axis.title.y = element_text(angle = 90),
      legend.position = c(0.99, 0.99),
      legend.justification = c(1, 1) ,
      plot.title = element_text(size = 16),
      #panel.grid.minor = element_blank()
    )
  
  offense <- pbp |> 
    group_by(season, posteam, game_id, drive) |> 
    summarise(epa = sum(epa, na.rm = T)) |> 
    ungroup() |> 
    group_by(season, posteam) |> 
    summarise(off_epa = mean(epa, na.rm = T)) |> 
    arrange(desc(off_epa))
  
  defense <- pbp |> 
    group_by(season, defteam, game_id, drive) |> 
    summarise(epa = sum(epa, na.rm = T)) |> 
    ungroup() |> 
    group_by(season, defteam) |> 
    summarise(def_epa = mean(epa, na.rm = T)) |> 
    arrange(desc(def_epa))
  
  chart_all <- offense |> 
    inner_join(defense, by = c('season', 'posteam' = 'defteam')) |> 
    left_join(teams_colors_logos, by = c('posteam' = 'team_abbr'))
  
  p <- chart_all |> 
    ggplot(aes(x = off_epa, y = def_epa)) +
    nflplotR::geom_nfl_logos(aes(team_abbr = posteam), width = 0.05, alpha = 0.9) +
    geom_hline(yintercept = mean(chart_all$def_epa, na.rm = T), color = 'red', linetype = 'dashed') +
    geom_vline(xintercept =  mean(chart_all$off_epa, na.rm = T), color = 'red', linetype = 'dashed') +
    labs(x = 'Offense EPA/Drive',
         y = 'Defense EPA/Drive',
         # caption = 'Data: @nflscrapR',
         title = glue('{current_season} NFL Team Tiers'),
         subtitle = glue('Offense and defense EPA per play through week {n_week}')) +
    geom_abline(slope=off_def_slope, intercept=ab_intercepts, alpha=.2) +
    scale_y_reverse() +
    theme_cw_dark +
    theme(
      axis.title.y = element_text(angle = 90),
      legend.position = c(0.99, 0.99),
      legend.justification = c(1, 1) ,
      plot.title = element_text(size = 16),
      #panel.grid.minor = element_blank()
    )
  
  brand_plot(p, asp = 16/10, save_name = glue('plots/desktop/team_tiers/epa_drive_team_tiers_{current_season}.png'), data_home = 'Data: @nflfastR', fade_borders = '')
  
  
  # Rotate ---
  off_epa_min <- -3
  off_epa_max <- 3
  def_epa_min <- -3
  def_epa_max <- 3
  
  # set rotation to 45 degrees
  rotation <- 45
  
  # make plot
  p <- chart_all |> 
    ggplot(aes(x = off_epa, y = def_epa)) + 
    # add color blocking
    annotate('rect', xmin = (off_epa_max + off_epa_min) / 2, xmax = off_epa_max,
             ymin = def_epa_min, ymax = (def_epa_max + def_epa_min) / 2, 
             fill= color_cw['green'], alpha = .1, color = 'transparent') +
    annotate('rect', xmin = off_epa_min, xmax = (off_epa_max + off_epa_min) / 2,
             ymin = (def_epa_max + def_epa_min) / 2, ymax = def_epa_max, 
             fill= color_cw['red'], alpha = .1, color = 'transparent') +
    annotate('rect', xmin = (off_epa_max + off_epa_min) / 2, xmax = off_epa_max,
             ymin = (def_epa_max + def_epa_min) / 2, ymax = def_epa_max, 
             fill= color_cw['blue'], alpha = .1, color = 'transparent')  +
    annotate('rect', xmin = off_epa_min, xmax = (off_epa_max + off_epa_min) / 2,
             ymin = def_epa_min, ymax = (def_epa_max + def_epa_min) / 2, 
             fill= color_cw['blue'], alpha = .1, color = 'transparent') +
    # add team logos
    geom_nfl_logos(aes(team_abbr = posteam), width = 0.08, alpha = 0.75, angle = -1*rotation) +
    scale_y_reverse(limits = c(def_epa_max, def_epa_min), breaks = ab_intercepts, labels = scales::number_format(accuracy = 0.01)) +  
    scale_x_continuous(limits = c(off_epa_min, off_epa_max), breaks = ab_intercepts, labels = scales::number_format(accuracy = 0.01)) +
    coord_equal() +
    # add axis labels
    labs(
      x = 'Offense EPA/play',
      y = 'Defense EPA/play'
    ) +
    theme_cw_dark +
    theme(axis.text.x = element_text(angle=(-1 * rotation), hjust = 0.5, margin = margin(t = -5)),
          axis.text.y = element_text(angle=(-1 * rotation), hjust = 0.4, vjust= -.005, margin = margin(r = -5)),
          axis.title.x = element_text(size = 12,
                                      #angle=(-1 * rotation + 45),
                                      vjust = 0.5, 
                                      margin = margin(t = 10),
                                      face = 'bold'),
          axis.title.y = element_text(size = 12,
                                      angle=(-1 * rotation - 45),
                                      hjust = 0.5,
                                      margin = margin(r = 10),
                                      face = 'bold'), 
          plot.margin = margin(1, .5, .5, 0, unit = 'in'), 
          plot.background = element_rect(color = 'transparent', 
                                         fill = 'transparent'),
          panel.background = element_rect(color = 'transparent',
                                          fill = 'transparent'),
          panel.border = element_rect(color = 'transparent'),
          panel.grid.minor = element_blank()) +
    # hack together a title and subtitle
    # geom_text(aes(x = .3, y = -0.3), label = glue('{year} NFL Team Tiers'), angle = -1 * rotation, vjust = -2.5, fontface = 'bold', size = 4, family = 'Montserrat', color = color_cw[5]) +
    # geom_text(aes(x = .3, y = -0.25), label = glue('Offense and defense EPA per play through week {n_week}'), angle = -1 * rotation, vjust = -2.5, fontface = 'bold', size = 4, family = 'Montserrat', color = color_cw[5]) +
    # hack together a few chart guides (ie, 'Good D, Bad D')
    geom_richtext(aes(x = off_epa_max*0.75, y = def_epa_max*0.75, label = 'Good O, Bad D'), angle = -1 * rotation, size = 3, family = 'Montserrat', fontface = 'bold', color = 'black', fill = color_cw['blue'], label.size = 0)  +
    geom_richtext(aes(x = off_epa_min*0.75, y = def_epa_min*0.75, label = 'Bad O, Good D'), angle = -1 * rotation,  size = 3, family = 'Montserrat',  fontface = 'bold', color = 'black', fill = color_cw['blue'], label.size = 0)   +
    geom_richtext(aes(x = off_epa_min*0.75, y = def_epa_max*0.75, label = 'Bad O, Bad D'), angle = -1 * rotation, size = 3, family = 'Montserrat', fontface = 'bold', color = 'black',  fill = color_cw['red'], label.size = 0)  +
    geom_richtext(aes(x = off_epa_max*0.75, y = def_epa_min*0.75, label = 'Good O, Good D'), angle = -1 * rotation,  size = 3, family = 'Montserrat', fontface = 'bold', color = 'black', fill = color_cw['green'], label.size = 0) 
  
  p_background <- ggplot() + 
    labs(title = glue('{current_season} NFL Team Tiers'),
         subtitle = glue('Offense and defense EPA per play through week {n_week}')) + 
    theme_cw_dark +
    theme(panel.background = element_rect(fill = color_cw[1], 
                                          color = 'transparent'),
          plot.title = element_text(family = 'Chivo',
                                    face = 'bold', 
                                    size = 16))
  
  # save plot
  png(glue('plots/desktop/team_tiers/epa_drive_team_tiers_diamond_{year}.png'), res = 300, width = 6, height = 6, units = 'in', bg = color_cw[1])
  
  print(p_background)
  print(p, vp=viewport(angle=rotation,  
                       width = unit(6, 'in'), 
                       height = unit(6, 'in'))) |> 
    suppressWarnings()
  
  dev.off()
  
  
  
  
  p <- chart_all |> 
    ggplot(aes(x = early_down_off_epa, y = def_early_down_epa)) +
    # geom_image(aes(image = team_logo_espn), size = 0.05, asp = 16/9) +
    geom_grob(data = chart_all,
              aes(
                x = early_down_off_epa,
                y = def_early_down_epa,
                label = grob_img_adj(team_logo_espn),
                vp.height = 0.08
              )) +
    geom_hline(yintercept = mean(chart_all$def_early_down_epa, na.rm = T), color = 'red', linetype = 'dashed') +
    geom_vline(xintercept =  mean(chart_all$early_down_off_epa, na.rm = T), color = 'red', linetype = 'dashed') +
    labs(x = 'Offense early down EPA/play',
         y = 'Defense early down EPA/play',
         # caption = 'Data: @nflscrapR',
         title = glue('{current_season} NFL Team Tiers'),
         subtitle = glue('Offense and defense early down EPA per play through week {n_week}')) +
    geom_abline(slope=off_def_slope, intercept=ab_intercepts, alpha=.2) +
    scale_y_reverse() +
    theme_cw_dark +
    theme(
      axis.title.y = element_text(angle = 90),
      legend.position = c(0.99, 0.99),
      legend.justification = c(1, 1) ,
      plot.title = element_text(size = 16),
      #panel.grid.minor = element_blank()
    )
  
  brand_plot(p, asp = 16/10, save_name = glue('plots/desktop/team_tiers/early_down_team_tiers_{current_season}.png'), data_home = 'Data: @nflfastR', fade_borders = '')
  
  # Pass v Rush EPA
  p <- chart_all |> 
    ggplot(aes(x = epa_per_pass, y = epa_per_rush)) +
    # geom_image(aes(image = team_logo_espn), size = 0.05, asp = 16/9) +
    geom_grob(data = chart_all,
              aes(
                x = epa_per_pass,
                y = epa_per_rush,
                label = grob_img_adj(team_logo_espn),
                vp.height = 0.08
              )) +
    geom_hline(yintercept = mean(chart_all$epa_per_rush), color = 'red', linetype = 'dashed') +
    geom_vline(xintercept =  mean(chart_all$epa_per_pass), color = 'red', linetype = 'dashed') +
    labs(x = 'Pass EPA/play',
         y = 'Rush EPA/play',
         # caption = 'Data: @nflscrapR',
         title = glue('{current_season} NFL Offense Team Tiers'),
         subtitle = glue('Offense passing and rushing EPA per play through week {n_week}')) +
    geom_abline(slope=pass_rush_slope, intercept=ab_intercepts, alpha=.2) +
    theme_cw_dark +
    theme(
      axis.title.y = element_text(angle = 90),
      legend.position = c(0.99, 0.99),
      legend.justification = c(1, 1) ,
      plot.title = element_text(size = 16),
      #panel.grid.minor = element_blank()
    )
  
  brand_plot(p, asp = 16/10, save_name = glue('plots/desktop/team_tiers/team_off_pass_and_rush_tiers_{current_season}.png'), data_home = 'Data: @nflfastR', fade_borders = '')
  
  # Need to add Defense pass and rush epa/play
  
  p <- chart_all |> 
    ggplot(aes(x = def_epa_per_pass, y = def_epa_per_rush)) +
    # geom_image(aes(image = team_logo_espn), size = 0.05, asp = 16/9) +
    geom_grob(data = chart_all,
              aes(
                x = def_epa_per_pass,
                y = def_epa_per_rush,
                label = grob_img_adj(team_logo_espn),
                vp.height = 0.08
              )) +
    geom_hline(yintercept = mean(chart_all$def_epa_per_rush), color = 'red', linetype = 'dashed') +
    geom_vline(xintercept =  mean(chart_all$def_epa_per_pass), color = 'red', linetype = 'dashed') +
    labs(x = 'Defense Pass EPA/play',
         y = 'Defense Rush EPA/play',
         # caption = 'Data: @nflscrapR',
         title = glue('{current_season} NFL Defense Team Tiers'),
         subtitle = glue('Defense passing and rushing EPA per play through week {n_week}')) +
    geom_abline(slope=pass_rush_slope, intercept=ab_intercepts, alpha=.2) +
    scale_x_reverse() +
    scale_y_reverse() +
    theme_cw_dark +
    theme(
      axis.title.y = element_text(angle = 90),
      legend.position = c(0.99, 0.99),
      legend.justification = c(1, 1) ,
      plot.title = element_text(size = 16),
      #panel.grid.minor = element_blank()
    )
  
  brand_plot(p, asp = 16/10, save_name = glue('plots/desktop/team_tiers/team_def_pass_and_rush_tiers_{current_season}.png'), data_home = 'Data: @nflfastR', fade_borders = '')
  
  
  
  # Adjust EPA --------------------------------------------------------------
  library(glmnet)
  opp_adj_y <- chart_all$off_epa
  opp_adj_x <- chart_all[, c('posteam', 'off_epa', 'def_epa')]
  opp_adjustments <- glmnet(opp_adj_x, opp_adj_y, alpha = 0)
  
  if (max(pbp$week) >= 10) {
    # https://www.opensourcefootball.com/posts/2020-08-20-adjusting-epa-for-strenght-of-opponent/
    epa_data <- pbp |>
      filter(posteam != '') |> 
      # dplyr::filter(!is.na(epa), !is.na(ep), !is.na(posteam), play_type == 'pass' | play_type == 'run') |>
      dplyr::filter(!is.na(posteam)) |>
      dplyr::group_by(game_id, season, week, posteam, home_team) |>
      dplyr::summarise(
        off_epa = mean(epa, na.rm = T),
      ) |>
      dplyr::left_join(pbp |>
                         filter(play_type == 'pass' | play_type == 'run') |>
                         dplyr::group_by(game_id, season, week, defteam, away_team) |>
                         dplyr::summarise(def_epa = mean(epa, na.rm = T)),
                       by = c('game_id', 'posteam' = 'defteam', 'season', 'week'),
                       all.x = T
      ) |>
      dplyr::mutate(opponent = ifelse(posteam == home_team, away_team, home_team)) |>
      dplyr::select(game_id, season, week, home_team, away_team, posteam, opponent, off_epa, def_epa)
    
    offense <- pbp |>
      filter(!is.na(epa) & !is.na(posteam)) |> 
      group_by(posteam, season) |>
      summarize(
        n_pass=sum(pass, na.rm = T),
        n_rush=sum(rush, na.rm = T),
        epa_per_pass=sum(epa*pass, na.rm = T)/n_pass,
        epa_per_rush=sum(epa*rush, na.rm = T)/n_rush,
        success_per_pass=sum(pass*epa>0, na.rm = T)/n_pass,
        success_per_rush=sum(rush*epa>0, na.rm = T)/n_rush,
        off_epa=mean(epa, na.rm = T),
        off_success=mean(success, na.rm = T)
      )
    
    # Construct opponent dataset and lag the moving average of their last ten games.
    opponent_data <- epa_data |>
      dplyr::select(-opponent) |>
      dplyr::rename(
        opp_off_epa = off_epa,
        opp_def_epa = def_epa
      ) |>
      dplyr::group_by(posteam) |>
      dplyr::arrange(season, week) |>
      dplyr::mutate(
        opp_def_epa = pracma::movavg(opp_def_epa, n = time_series - 1, type = 's'),
        opp_def_epa = dplyr::lag(opp_def_epa),
        opp_off_epa = pracma::movavg(opp_off_epa, n = time_series - 1, type = 's'),
        opp_off_epa = dplyr::lag(opp_off_epa)
      )
    
    # Merge opponent data back in with the weekly epa data
    epa_data <- epa_data |>
      left_join(
        opponent_data,
        by = c('game_id', 'season', 'week', 'home_team', 'away_team', 'opponent' = 'posteam'),
        all.x = TRUE
      )
    
    epa_data <- epa_data |>
      dplyr::left_join(epa_data |>
                         dplyr::filter(posteam == home_team) |>
                         dplyr::group_by(season, week) |>
                         dplyr::summarise(
                           league_mean = mean(off_epa + def_epa)
                         ) |>
                         dplyr::ungroup() |>
                         dplyr::group_by(season) |>
                         dplyr::mutate(
                           league_mean = lag(pracma::movavg(league_mean, n = as.integer(time_series), type = 's'), ) # We lag because we need to know the league mean up to that point in the season
                         ),
                       by = c('season', 'week'),
                       all.x = TRUE
      )
    
    # Adjust EPA
    epa_data <- epa_data |>
      dplyr::mutate(
        off_adjustment_factor = ifelse(!is.na(league_mean), league_mean - opp_def_epa, 0),
        def_adjustment_factor = ifelse(!is.na(league_mean), league_mean - opp_off_epa, 0),
        adjusted_off_epa = off_epa + off_adjustment_factor,
        adjusted_def_epa = def_epa + def_adjustment_factor,
      )
    
    chart_all_adj_epa <- epa_data |> 
      filter(season == current_season) |> 
      arrange(posteam) |> 
      group_by(posteam) |> 
      summarize(
        adjusted_off_epa = mean(adjusted_off_epa, na.rm = T),
        adjusted_def_epa = mean(adjusted_def_epa, na.rm = T)
      ) |> 
      # filter(row_number() == n()) |> 
      left_join(teams_colors_logos, by = c('posteam' = 'team_abbr'))
    
    p <- chart_all_adj_epa |> 
      ggplot(aes(x = adjusted_off_epa, y = adjusted_def_epa)) +
      # geom_image(aes(image = team_logo_espn), size = 0.05, asp = 16/10) +
      geom_hline(yintercept = mean(chart_all_adj_epa$adjusted_def_epa, na.rm = T), color = 'red', linetype = 'dashed') +
      geom_vline(xintercept =  mean(chart_all_adj_epa$adjusted_off_epa, na.rm = T), color = 'red', linetype = 'dashed') +
      geom_grob(aes(
        x = adjusted_off_epa,
        y = adjusted_def_epa,
        label = grob_img_adj(team_logo_espn),
        vp.height = 0.08
      )) +
      labs(x = 'Adj. Offense EPA/play',
           y = 'Adj. Defense EPA/play',
           # caption = 'Data: @nflscrapR',
           title = glue('{current_season} NFL Adjusted Team Tiers'),
           subtitle = glue('Offense and defense adjusted EPA per play through week {n_week}\nAdjusted for previous matchups')) +
      geom_abline(slope=off_def_slope, intercept=ab_intercepts, alpha=.2) +
      scale_y_reverse() +
      theme_cw_dark +
      theme(
        axis.title.y = element_text(angle = 90),
        legend.position = c(0.99, 0.99),
        legend.justification = c(1, 1) ,
        plot.title = element_text(size = 16),
        #panel.grid.minor = element_blank()
      )
    
    brand_plot(p, asp = 16/10, save_name = glue('plots/desktop/team_tiers/team_tiers_adj_{current_season}.png'), data_home = 'Data: @nflfastR', fade_borders = '')
  }
  
  
  # Next week match-ups -----------------------------------------------------
  if (n_week < 17) {
    matchup_chart_all <- chart_all |>
      left_join(
        matchup_df |> 
          filter(week == n_week + 1) |> 
          select(posteam, oppteam, weekday, gametime),
        by = c('posteam')
      ) |> 
      left_join(chart_all |> 
                  rename(
                    opp_n_pass = n_pass,
                    opp_n_rush = n_rush,
                    opp_epa_per_pass = epa_per_pass,
                    opp_epa_per_rush = epa_per_rush,
                    opp_success_per_pass = success_per_pass,
                    opp_success_per_rush = success_per_rush,
                    opp_off_epa = off_epa,
                    opp_def_n_pass = def_n_pass,
                    opp_def_n_rush = def_n_rush,
                    opp_def_epa_per_pass = def_epa_per_pass,
                    opp_def_epa_per_rush = def_epa_per_rush,
                    opp_def_success_per_pass = def_success_per_pass,
                    opp_def_success_per_rush = def_success_per_rush,
                    opp_def_epa = def_epa,
                    opp_def_success = def_success,
                    opp_team_name = team_name,
                    opp_team_id = team_id,
                    opp_team_nick = team_nick,
                    opp_team_color = team_color,
                    opp_team_color2 = team_color2,
                    opp_team_color3 = team_color3,
                    opp_team_color4 = team_color4,
                    opp_team_logo_wikipedia = team_logo_wikipedia,
                    opp_team_logo_espn = team_logo_espn
                  ),
                by = c('oppteam' = 'posteam')
      ) |>
      filter(!is.na(oppteam ))
    
    p <- matchup_chart_all |> 
      ggplot(aes(x = off_epa, y = opp_def_epa)) +
      # geom_image(aes(image = team_logo_espn), size = 0.05, asp = 16/9) +
      geom_grob(aes(
        x = off_epa,
        y = opp_def_epa,
        label = grob_img_adj(team_logo_espn),
        vp.height = 0.08
      )) +
      geom_hline(yintercept = mean(matchup_chart_all$opp_def_epa), color = 'red', linetype = 'dashed') +
      geom_vline(xintercept =  mean(matchup_chart_all$off_epa), color = 'red', linetype = 'dashed') +
      labs(x = 'Offense EPA/play',
           y = 'Opponent Defense EPA/play',
           # caption = 'Data: @nflscrapR',
           title = glue('{current_season} NFL Team Tiers Matchups through Week {n_week}'),
           subtitle = glue('Team offense and week {n_week + 1} opponent defense EPA per play')) +
      geom_abline(slope=off_def_slope, intercept=ab_intercepts, alpha=.2) +
      theme_cw_dark +
      theme(
        axis.title.y = element_text(angle = 90),
        legend.position = c(0.99, 0.99),
        legend.justification = c(1, 1) ,
        plot.title = element_text(size = 16),
        #panel.grid.minor = element_blank()
      )
    
    brand_plot(p, asp = 16/10, save_name = glue('plots/desktop/team_tiers/matchup_team_tiers_{current_season}.png'), data_home = 'Data: @nflfastR', fade_borders = '')
    
    # Passing matchup
    p <- matchup_chart_all |> 
      ggplot(aes(x = epa_per_pass, y = opp_def_epa_per_pass)) +
      # geom_image(aes(image = team_logo_espn), size = 0.05, asp = 16/9) +
      geom_grob(aes(
        x = epa_per_pass,
        y = opp_def_epa_per_pass,
        label = grob_img_adj(team_logo_espn),
        vp.height = 0.08
      )) +
      geom_hline(yintercept = mean(matchup_chart_all$opp_def_epa_per_pass), color = 'red', linetype = 'dashed') +
      geom_vline(xintercept =  mean(matchup_chart_all$epa_per_pass), color = 'red', linetype = 'dashed') +
      labs(x = 'Offense Pass EPA/play',
           y = 'Opponent Defense Pass EPA/play',
           # caption = 'Data: @nflscrapR',
           title = glue('{current_season} NFL Team Tiers Passing Matchups through Week {n_week}'),
           subtitle = glue('Team passing offense and week {n_week + 1} opponent passing defense EPA per play')) +
      geom_abline(slope=off_def_slope, intercept=ab_intercepts, alpha=.2) +
      theme_cw_dark +
      theme(
        axis.title.y = element_text(angle = 90),
        legend.position = c(0.99, 0.99),
        legend.justification = c(1, 1) ,
        plot.title = element_text(size = 16),
        #panel.grid.minor = element_blank()
      )
    
    brand_plot(p, asp = 16/10, save_name = glue('plots/desktop/team_tiers/matchup_pass_team_tiers_{current_season}.png'), data_home = 'Data: @nflfastR', fade_borders = '')
    
    # Rushing matchup
    p <- matchup_chart_all |> 
      ggplot(aes(x = epa_per_rush, y = opp_def_epa_per_rush)) +
      # geom_image(aes(image = team_logo_espn), size = 0.05, asp = 16/9) +
      geom_grob(aes(
        x = epa_per_rush,
        y = opp_def_epa_per_rush,
        label = grob_img_adj(team_logo_espn),
        vp.height = 0.08
      )) +
      geom_hline(yintercept = mean(matchup_chart_all$opp_def_epa_per_rush), color = 'red', linetype = 'dashed') +
      geom_vline(xintercept =  mean(matchup_chart_all$epa_per_rush), color = 'red', linetype = 'dashed') +
      labs(x = 'Offense Rush EPA/play',
           y = 'Opponent Defense Rush EPA/play',
           # caption = 'Data: @nflscrapR',
           title = glue('{current_season} NFL Team Tiers Rushing Matchups through Week {n_week}'),
           subtitle = glue('Team rushing offense and week {n_week + 1} opponent rushing defense EPA per play')) +
      geom_abline(slope=off_def_slope, intercept=ab_intercepts, alpha=.2) +
      theme_cw_dark +
      theme(
        axis.title.y = element_text(angle = 90),
        legend.position = c(0.99, 0.99),
        legend.justification = c(1, 1) ,
        plot.title = element_text(size = 16),
        #panel.grid.minor = element_blank()
      )
    
    brand_plot(p, asp = 16/10, save_name = glue('plots/desktop/team_tiers/matchup_rush_team_tiers_{current_season}.png'), data_home = 'Data: @nflfastR', fade_borders = '')
  }
  end_time <- Sys.time()
  print(end_time - start_time)
}