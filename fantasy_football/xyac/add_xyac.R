add_xyac <- function(pbp) {
  
  if (nrow(pbp) == 0) {
    message("Nothing to do. Return empty data frame.")
  } else {
    # testing only
    # pbp <- g
    
    pbp <- pbp %>% dplyr::select(-tidyselect::any_of(drop.cols.xyac))
    
    # for joining at the end
    pbp <- pbp %>%
      dplyr::mutate(index = 1:dplyr::n())
    
    # prepare_xyac_data helper function shown below
    passes <- prepare_xyac_data(pbp) %>%
      filter(.data$valid_pass == 1, .data$distance_to_goal != 0)
    
    if (!nrow(passes) == 0) {
      # initialize xyac_model to avoid R CMD check note
      xyac_model <- NULL
      suppressWarnings(
        # load the model from github because it is too big for the package
        try(
          load(url("https://github.com/guga31bb/nflfastR-data/blob/master/models/xyac_model.Rdata?raw=true")),
          silent = TRUE
        )
      )
      
      if (!is.null(xyac_model)) {
        xyac_vars <-
          stats::predict(
            xyac_model,
            as.matrix(passes %>% xyac_model_select())
          ) %>%
          tibble::as_tibble() %>%
          dplyr::rename(prob = "value") %>%
          dplyr::bind_cols(
            purrr::map_dfr(seq_along(passes$index), function(x) {
              tibble::tibble(
                "yac" = -5:70,
                "index" = passes$index[[x]],
                "distance_to_goal" = passes$distance_to_goal[[x]],
                "season" = passes$season[[x]],
                "week" = passes$week[[x]],
                "home_team" = passes$home_team[[x]],
                "posteam" = passes$posteam[[x]],
                "roof" = passes$roof[[x]],
                "half_seconds_remaining" = dplyr::if_else(
                  passes$half_seconds_remaining[[x]] <= 6,
                  0,
                  passes$half_seconds_remaining[[x]] - 6
                ),
                "down" = as.integer(passes$down[[x]]),
                "ydstogo" = as.integer(passes$ydstogo[[x]]),
                "original_ydstogo" = as.integer(passes$ydstogo[[x]]),
                "posteam_timeouts_remaining" = passes$posteam_timeouts_remaining[[x]],
                "defteam_timeouts_remaining" = passes$defteam_timeouts_remaining[[x]],
                "original_spot" = passes$yardline_100[[x]],
                "original_ep" = passes$ep[[x]],
                "air_epa" = passes$air_epa[[x]],
                "air_yards" = passes$air_yards[[x]]
              )
            })
          ) %>%
          dplyr::group_by(.data$index) %>%
          dplyr::mutate(
            max_loss = dplyr::if_else(.data$distance_to_goal < 95, -5, .data$distance_to_goal - 99),
            max_gain = dplyr::if_else(.data$distance_to_goal > 70, 70, .data$distance_to_goal),
            cum_prob = cumsum(.data$prob),
            prob = dplyr::case_when(
              # truncate probs at loss greater than max loss
              .data$yac == .data$max_loss ~ .data$cum_prob,
              # same for gains bigger than possible
              .data$yac == .data$max_gain ~ 1 - dplyr::lag(.data$cum_prob, 1),
              TRUE ~ .data$prob
            ),
            # get end result for each possibility
            yardline_100 = .data$distance_to_goal - .data$yac
          ) %>%
          dplyr::filter(.data$yac >= .data$max_loss, .data$yac <= .data$max_gain) %>%
          dplyr::select(-.data$cum_prob) %>%
          dplyr::mutate(
            posteam_timeouts_pre = .data$posteam_timeouts_remaining,
            defeam_timeouts_pre = .data$defteam_timeouts_remaining,
            gain = .data$original_spot - .data$yardline_100,
            turnover = dplyr::if_else(.data$down == 4 & .data$gain < .data$ydstogo, as.integer(1), as.integer(0)),
            down = dplyr::if_else(.data$gain >= .data$ydstogo, 1, .data$down + 1),
            ydstogo = dplyr::if_else(.data$gain >= .data$ydstogo, 10, .data$ydstogo - .data$gain),
            # possession change if 4th down failed
            down = dplyr::if_else(.data$turnover == 1, as.integer(1), as.integer(.data$down)),
            ydstogo = dplyr::if_else(.data$turnover == 1, as.integer(10), as.integer(.data$ydstogo)),
            # flip yardline_100 and timeouts for turnovers
            yardline_100 = dplyr::if_else(.data$turnover == 1, as.integer(100 - .data$yardline_100), as.integer(.data$yardline_100)),
            posteam_timeouts_remaining = dplyr::if_else(.data$turnover == 1,
                                                        .data$defeam_timeouts_pre,
                                                        .data$posteam_timeouts_pre),
            defteam_timeouts_remaining = dplyr::if_else(.data$turnover == 1,
                                                        .data$posteam_timeouts_pre,
                                                        .data$defeam_timeouts_pre),
            # ydstogo can't be bigger than yardline
            ydstogo = dplyr::if_else(.data$ydstogo >= .data$yardline_100, as.integer(.data$yardline_100), as.integer(.data$ydstogo))
          ) %>%
          dplyr::ungroup() %>%
          nflfastR::calculate_expected_points() %>%
          dplyr::group_by(.data$index) %>%
          dplyr::mutate(
            ep = dplyr::case_when(
              .data$yardline_100 == 0 ~ 7,
              .data$turnover == 1 ~ -1 * .data$ep,
              TRUE ~ ep
            ),
            epa = .data$ep - .data$original_ep,
            wt_epa = .data$epa * .data$prob,
            wt_yardln = .data$yardline_100 * .data$prob,
            med = dplyr::if_else(
              cumsum(.data$prob) > .5 & dplyr::lag(cumsum(.data$prob) < .5), .data$yac, as.integer(0)
            )
          ) %>%
          dplyr::summarise(
            xyac_epa = sum(.data$wt_epa) - dplyr::first(.data$air_epa),
            xyac_mean_yardage = (dplyr::first(.data$original_spot) - dplyr::first(.data$air_yards)) - sum(.data$wt_yardln),
            xyac_median_yardage = max(.data$med),
            xyac_success = sum((.data$ep > .data$original_ep) * .data$prob),
            xyac_fd = sum((.data$gain >= .data$original_ydstogo) * .data$prob),
            .groups = "drop_last"
          ) %>%
          dplyr::ungroup()
        
        pbp <- pbp %>%
          dplyr::left_join(xyac_vars, by = "index") %>%
          dplyr::select(-.data$index)
        
        message("added xyac variables")
        
      } else {# means xyac_model isn't available
        message("This function needs to download the model data from GitHub. Please check your Internet connection and try again!")
        pbp <- pbp %>% dplyr::select(-.data$index)
      }
    } else {# means no valid pass plays in the pbp
      pbp <- pbp %>%
        dplyr::mutate(
          xyac_epa = NA_real_,
          xyac_mean_yardage = NA_real_,
          xyac_median_yardage = NA_real_,
          xyac_success = NA_real_,
          xyac_fd = NA_real_
        ) %>%
        dplyr::select(-.data$index)
      message("No non-NA values for xyac calculation detected. xyac variables set to NA")
    }
    
    # on old versions of dplyr, a .groups column is created, which we don't want
    pbp <- pbp %>% dplyr::select(-tidyselect::any_of(".groups"))
  }
  
  return(pbp)
}