update_maria_db <-
  function (dbdir = ".",
            dbname = "pbp_db",
            tblname = "nflfastR_pbp",
            force_rebuild = FALSE,
            db_connection = NULL)
  {
    rule_header("Update nflfastR Play-by-Play Database")
    if (!requireNamespace("DBI", quietly = TRUE) |
        (!requireNamespace("RMariaDB",
                           quietly = TRUE) &
         is.null(db_connection))) {
      usethis::ui_stop(
        "Packages {usethis::ui_value('DBI')} and {usethis::ui_value('RMariaDB')} needed for database communication. Please install them."
      )
    }
    if (any(force_rebuild == "NEW")) {
      usethis::ui_stop(
        "The argument {usethis::ui_value('force_rebuild = NEW')} is only for internal usage!"
      )
    }
    if (!(is.logical(force_rebuild) | is.numeric(force_rebuild))) {
      usethis::ui_stop(
        "The argument {usethis::ui_value('force_rebuild')} has to be either logical or numeric!"
      )
    }
    if (!dir.exists(dbdir) & is.null(db_connection)) {
      usethis::ui_oops("Directory {usethis::ui_path(dbdir)} doesn't exist yet. Try creating...")
      dir.create(dbdir)
    }
    if (is.null(db_connection)) {
      connection <-
        DBI::dbConnect(
          RMariaDB::MariaDB(),
          host = ifelse(fromJSON(
            readLines("http://api.hostip.info/get_json.php",
                      warn = F)
          )$ip == ip,
          local,
          ip),
          port = "3306",
          user = db_user,
          password = "VYgIlmCeNg3wIoLe",
          dbname = proj_name,
          # database = "football",
          # Server = "localhost\\SQLEXPRESS",
          # Database = "datawarehouse",
          Trusted_Connection = "True"
        )
    }
    else {
      connection <- db_connection
    }
    if (!DBI::dbExistsTable(connection, tblname)) {
      build_db(tblname, connection, rebuild = "NEW")
    }
    else if (DBI::dbExistsTable(connection, tblname) &
             all(force_rebuild !=
                 FALSE)) {
      build_db(tblname, connection, rebuild = force_rebuild)
    }
    usethis::ui_todo("Checking for missing completed games...")
    completed_games <-
      readRDS(
        url(
          "https://github.com/leesharpe/nfldata/blob/master/data/games.rds?raw=true"
        )
      ) %>%
      dplyr::filter(
        .data$season >= 1999,
        !is.na(.data$result),!.data$game_id %in% c("1999_01_BAL_STL", "2000_06_BUF_MIA",
                                                   "2000_03_SD_KC")
      ) %>% dplyr::arrange(.data$gameday) %>%
      dplyr::pull(.data$game_id)
    missing <- get_missing_games(completed_games, connection,
                                 tblname)
    if (length(missing) > 16) {
      build_db(tblname,
               connection,
               show_message = FALSE,
               rebuild = as.numeric(unique(stringr::str_sub(missing,
                                                            1, 4))))
      missing <- get_missing_games(completed_games, connection,
                                   tblname)
    }
    if (length(missing) > 0) {
      if (!requireNamespace("furrr", quietly = TRUE)) {
        is_installed_furrr <- FALSE
        usethis::ui_info(
          "Package {usethis::ui_value('furrr')} not installed. Can't use parallel processing. Please consider installing it."
        )
        usethis::ui_info("Will go on sequentially...")
      }
      else {
        is_installed_furrr <- TRUE
      }
      if (is_installed_furrr == TRUE & length(missing) < 5) {
        is_installed_furrr <- FALSE
      }
      new_pbp <- build_nflfastR_pbp(missing, pp = is_installed_furrr,
                                    rules = FALSE)
      if (nrow(new_pbp) == 0) {
        usethis::ui_oops("Raw data of new games are not yet ready. Please try again in about 10 minutes.")
      }
      else {
        usethis::ui_todo("Appending new data to database...")
        DBI::dbWriteTable(connection, tblname, new_pbp,
                          append = TRUE)
      }
    }
    message_completed("Database update completed", in_builder = TRUE)
    usethis::ui_info("Path to your db: {usethis::ui_path(DBI::dbGetInfo(connection)$dbname)}")
    DBI::dbDisconnect(connection)
    rule_footer("DONE")
  }
