update_cfb_db_mod <- function (dbdir = ".", dbname = "cfb_pbp_db", tblname = "cfbfastR_pbp", 
          force_rebuild = FALSE, db_connection = NULL) 
{
  rule_header("Update cfbfastR Play-by-Play Database")
  if (!is_installed("DBI") | !is_installed("purrr") | (!is_installed("RSQLite") & 
                                                       is.null(db_connection))) {
    cli::cli_abort("{my_time()} | Packages {.val DBI}, {.val RSQLite} and {.val purrr} required for database communication. Please install them.")
  }
  if (any(force_rebuild == "NEW")) {
    cli::cli_abort("{my_time()} | The argument {.val 'force_rebuild = NEW'} is only for internal usage!")
  }
  if (!(is.logical(force_rebuild) | is.numeric(force_rebuild))) {
    cli::cli_abort("{my_time()} | The argument {.val 'force_rebuild'} has to be either logical or numeric!")
  }
  if (!dir.exists(dbdir) & is.null(db_connection)) {
    cli::cli_alert_danger("{my_time()} | Directory {.file {dbdir}} doesn't exist yet. Try creating...")
    dir.create(dbdir)
  }
  if (is.null(db_connection)) {
    connection <- DBI::dbConnect(RSQLite::SQLite(), glue::glue("{dbdir}/{dbname}"))
  }
  else {
    connection <- db_connection
  }
  if (!DBI::dbExistsTable(connection, tblname)) {
    build_cfb_db_mod(tblname, connection, rebuild = "NEW")
  }
  else if (DBI::dbExistsTable(connection, tblname) & all(force_rebuild != 
                                                         FALSE)) {
    build_cfb_db_mod(tblname, connection, rebuild = force_rebuild)
  }
  user_message("Checking for missing completed games...", 
               "todo")
  completed_games <- load_games() %>% dplyr::filter(.data$year >= 
                                                      2014) %>% dplyr::arrange(.data$week) %>% dplyr::pull(.data$game_id)
  missing <- get_missing_cfb_games(completed_games, connection, 
                                   tblname)
  if (length(missing) > 16) {
    build_cfb_db_mod(tblname, connection, show_message = FALSE, 
                 rebuild = as.numeric(unique(stringr::str_sub(missing, 
                                                              1, 4))))
    missing <- get_missing_cfb_games(completed_games, connection, 
                                     tblname)
  }
  message_completed("Database update completed", in_builder = TRUE)
  cli::cli_alert_info("{my_time()} | Path to your db: {.file {DBI::dbGetInfo(connection)$dbname}}")
  if (is.null(db_connection)) 
    DBI::dbDisconnect(connection)
  rule_footer("DONE")
}

build_cfb_db_mod <- function(tblname = "cfbfastR_pbp", db_conn, rebuild = FALSE, show_message = TRUE) {
  
  valid_seasons <- load_games() %>%
    dplyr::filter(.data$year >= 2014) %>%
    dplyr::group_by(.data$year) %>%
    dplyr::summarise() %>%
    dplyr::ungroup()
  
  if (all(rebuild == TRUE)) {
    cli::cli_ul("{my_time()} | Purging the complete data table {.val {tblname}} in your connected database...")
    DBI::dbRemoveTable(db_conn, tblname)
    seasons <- valid_seasons %>% dplyr::pull("year")
    cli::cli_ul("{my_time()} | Starting download of {length(seasons)} seasons between {min(seasons)} and {max(seasons)}...")
  } else if (is.numeric(rebuild) & all(rebuild %in% valid_seasons$year)) {
    string <- paste0(rebuild, collapse = ", ")
    if (show_message){cli::cli_ul("{my_time()} | Purging {string} season(s) from the data table {.val {tblname}} in your connected database...")}
    DBI::dbExecute(db_conn, glue::glue_sql("DELETE FROM {`tblname`} WHERE season IN ({vals*})", vals = rebuild, .con = db_conn))
    seasons <- valid_seasons %>% dplyr::filter(.data$year %in% rebuild) %>% dplyr::pull("year")
    cli::cli_ul("{my_time()} | Starting download of the {string} season(s)...")
  } else if (all(rebuild == "NEW")) {
    cli::cli_alert_info("{my_time()} | Can't find the data table {.val {tblname}} in your database. Will load the play by play data from scratch.")
    seasons <- valid_seasons %>% dplyr::pull("year")
    cli::cli_ul("{my_time()} | Starting download of {length(seasons)} seasons between {min(seasons)} and {max(seasons)}...")
  } else {
    seasons <- NULL
    cli::cli_alert_danger("{my_time()} | At least one invalid value passed to argument {.val force_rebuild}. Please try again with valid input.")
  }
  
  if (!is.null(seasons)) {
    # this function lives in R/utils.R
    load_cfb_pbp(seasons, dbConnection = db_conn, tablename = tblname, qs = FALSE)
  }
}
