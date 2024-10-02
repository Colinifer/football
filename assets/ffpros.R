plot_fp_dist <- function(fp_dist_df) {
  season <- min(fp_dist_df$season)
  ranking_type_name <- fp_dist_df$metadata$ranking_type_name
  week <- min(fp_dist_df$week)
  
  player_short_names <- fp_dist_df$player_short_name
  
  dp_density_df <- fp_dist_df |>
    group_by(player_name, player_short_name) %>%
    summarise(ecr = median(ecr),
              density = density(result)$y[which.max(density(result)$y)]) |> 
    arrange(ecr) |> 
    head(25)
  
  fp_dist_plot <- fp_dist_df |> 
    arrange(ecr) |> 
    head(min(fp_dist_df$total_experts)*25) |> 
    # filter(player_name %in% c("Tyreek Hill", "CeeDee Lamb", "Cooper Kupp")) |>
    ggplot(aes(result, fill = player_short_name, alpha=0.2)) + 
    geom_density() +
    geom_text_repel(
      data = dp_density_df,
      aes(x = ecr, 
          y = density + 0.025,
          label = player_name),
      direction = "y",
      segment.color = "transparent",
      size = 4,
      color = color_cw["white"]
    ) +
    theme_cw_dark + 
    theme(plot.caption = element_text(hjust = 1),
          # legend.position = "none"
    ) +
    labs(title="Fantasy Point Distributions",
         subtitle = glue("Season: {season}, Week: {week}"),
         x = "FFPros ECR",
         y = "Density", 
         caption = "Data: FantasyPros")
  
  return(fp_dist_plot)
}

clean_fp_ranks <- function(fp_ranks,
                           fantasy_league = "Beep Boop",
                           path="") {
  fp_clean_df <- fp_ranks$ecr |> 
    mutate(
      total_experts = fp_ranks$metadata$total_experts,
      season = fp_ranks$metadata$year,
      scoring = fp_ranks$metadata$scoring,
      ranking_type = fp_ranks$metadata$ranking_type_name,
      week = case_when(
        ranking_type == "weekly" ~ as.numeric(fp_ranks$metadata$week),
        TRUE ~ NA
      ),
      date = as.POSIXct(fp_ranks$metadata$last_updated_ts, origin = "1970-01-01", tz = "UTC") |> 
        as.Date(format = "%Y-%m-%d")
    ) |> 
    select(
      date,
      season,
      ranking_type,
      week,
      scoring,
      total_experts,
      everything()
    ) |> 
    left_join(
      ff_player_ids,
      by = "fantasypros_id"
    ) |> 
    left_join(
      fantasy_rosters |> 
        filter(league == fantasy_league) |> 
        select(
          player_id, 
          franchise_id,
          self
        ) |> 
        mutate(player_id = as.character(player_id)),
      by = c("espn_id" = "player_id")
    )
  
  if (path != "") {
    print(glue("Saving in {path}"))
    
    fseason = fp_clean_df$season |> min()
    fweek = fp_clean_df$week |> min()
    
    try(
      fp_clean_df |> 
        write_csv(glue("{path}/{fseason}_{fweek}_ecr.csv"))
    )
  }
  
  return(fp_clean_df)
}

create_fp_dist_df <- function(fp_clean_df,
                              print_plot = TRUE) {
  
  worst_rank <- fp_clean_df |> 
    filter(self == TRUE) |> 
    pull(worst) |> 
    max()
  
  fp_dist_df <- fp_clean_df |>
    filter(rank < 200 
           & (
             is.na(franchise_id)
             | self == TRUE
           )
           & ecr < worst_rank
    ) |>
    # select(player_name, franchise_id, self) |> 
    rowwise() |>
    mutate(result = list(rgbeta(
      n = total_experts,
      mean = ecr,
      var = sd,
      min = best,
      max = worst,
      # player = player_name
    ))) |>
    ungroup() |> 
    mutate(row_id = row_number()) |> 
    unnest(cols = c(result))
  
  return(fp_dist_df)
}

create_fp_dist_rankings <- function(fp_dist_df,
                                    print_plot = TRUE) {
  
  worst_rank <- fp_dist_df |> 
    filter(self == TRUE) |> 
    pull(worst) |> 
    max()
  
  worst_ecr <- fp_dist_df |> 
    filter(self == TRUE) |> 
    pull(ecr) |> 
    max()
  
  fp_dist_rankings <- fp_dist_df |> 
    filter(result < worst_ecr) |> 
    group_by(player_name, position, team, rank, total_experts, ecr, sd, best, worst, self) |> 
    summarise(
      n_better = n()
    ) |> 
    mutate(
      pct_better = n_better/total_experts
    ) |> 
    arrange(ecr, desc(pct_better))
  
  # print plot and return data
  if (print_plot == TRUE) {
    print(plot_fp_dist(fp_dist_df))
  }
  return(fp_dist_rankings)
}

update_fp_ranks_db <- function(fp_clean_df,
                               con = fx.db_con(x.host = 'localhost')) {
  on.exit(dbDisconnect(con))
  
  table <- 'ffpros_ecr_rankings'
  
  overwrite_data <- fp_clean_df |> 
    select(date, season, ranking_type, week) |>
    unique()
  
  sql_list_date <- paste0("IN (\'", paste(overwrite_data$date, collapse = "', '"), "\')")
  
  sql_list_season <- paste0("IN (\'", paste(overwrite_data$season, collapse = "', '"), "\')")
  
  sql_list_rankingtype <- paste0("IN (\'", paste(overwrite_data$ranking_type, collapse = "', '"), "\')")
  
  sql_list_week <- paste0("IN (\'", paste(overwrite_data$week, collapse = "', '"), "\')")
  
  if (is.na(overwrite_data$week)) {
    sql_list_week <- "IS NULL"
    print(sql_list_week)
  }
  
  if (dbExistsTable(con, table) == FALSE) {
    dbCreateTable(con,
                  table,
                  fp_clean_df)
  }
  
  if (dbExistsTable(con, table) == TRUE) {
    query <- glue(
      '
        DELETE
        FROM "{table}"
        WHERE
          date {sql_list_date}
          AND season {sql_list_season}
          AND ranking_type {sql_list_rankingtype}
          AND week {sql_list_week};
      '
    )
    print(query)
    
    dbExecute(con, query)
    
    dbWriteTable(con,
                 table,
                 fp_clean_df,
                 append = TRUE)
  }
}
