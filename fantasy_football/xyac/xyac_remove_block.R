dplyr::ungroup() %>% nflfastR::calculate_expected_points() %>% 
  dplyr::group_by(.data$index) %>% dplyr::mutate(ep = dplyr::case_when(.data$yardline_100 == 
  0 ~ 7, .data$turnover == 1 ~ -1 * .data$ep, TRUE ~ 
  ep), epa = .data$ep - .data$original_ep, wt_epa = .data$epa * 
  .data$prob, wt_yardln = .data$yardline_100 * .data$prob, 
  med = dplyr::if_else(cumsum(.data$prob) > 0.5 & dplyr::lag(cumsum(.data$prob) <
    0.5), .data$yac, as.integer(0))) %>% dplyr::summarise(xyac_epa = sum(.data$wt_epa) -
  dplyr::first(.data$air_epa), xyac_mean_yardage = (dplyr::first(.data$original_spot) -
  dplyr::first(.data$air_yards)) - sum(.data$wt_yardln),
  xyac_median_yardage = max(.data$med), xyac_success = sum((.data$ep > 
    .data$original_ep) * .data$prob), xyac_fd = sum((.data$gain >= 
    .data$original_ydstogo) * .data$prob), .groups = "drop_last") %>%