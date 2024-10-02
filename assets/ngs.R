library(httr)

# NFL Pro

url <- "https://pro.nfl.com/api/stats/players-offense/receiving/season"

cookies <- httr::set_cookies(
  "nfl_web_sdk_plugin_storage" = ",percentPageViewed|lastRecPage|nfl pro:stats:receiving:season leaders|string,percentPageViewed|percentPageViewed|100|number,percentPageViewed|initialPageViewed|100|number,percentPageViewed|maxPageViewed|1172|number",
  "nflcs.prod.crossDomainStorageCleared" = "true",
  "nflcs.prod.keyStoreDomainSyncList"	= "id.nfl.com",
  "nflcs.prod.nfl.user"	= "eyJjYWxsSWQiOiIwMzliZTFmNzVlMzg0MDJiOGIxYWQ2MGM0NDY1MGRiZCIsImVycm9yQ29kZSI6MCwiYXBpVmVyc2lvbiI6MiwidGltZSI6IjIwMjQtMDktMjBUMDI6NDc6MTkuMTQyWiIsInJlZ2lzdGVyZWRUaW1lc3RhbXAiOjE1ODQ4MDA1OTYwMDAsIlVJRCI6IjA5NTkwY2Q3MTA0ZDMxNmU0OWZmZDI5NDRmMTEzOWM1IiwiVUlEU2lnbmF0dXJlIjoiMk95NjROQVdiNHlMQVFpZG10VlhtV3l4RFdNPSIsInNpZ25hdHVyZVRpbWVzdGFtcCI6IjE3MjY4MDA0MzkiLCJjcmVhdGVkIjoiMjAyMC0wMy0yMVQxNDoyMzoxNi42NzFaIiwiY3JlYXRlZFRpbWVzdGFtcCI6MTU4NDgwMDU5NjAwMCwiZGF0YSI6eyJvd25JZCI6eyJjb25uZWN0aW9ucyI6W3siZmlkbzJDcmVkZW50aWF…ZhdWx0UmVnU2NyZWVuU2V0IjoiTWFpblNjcmVlblNldCIsImRlZmF1bHRNb2JpbGVSZWdTY3JlZW5TZXQiOiJNYWluU2NyZWVuU2V0Iiwic2Vzc2lvbkV4cGlyYXRpb24iOi0yLCJyZW1lbWJlclNlc3Npb25FeHBpcmF0aW9uIjoxNTc3ODQ3NiwiYXBpRG9tYWluIjoidXMxLmdpZ3lhLmNvbSIsImVuYWJsZWRQcm92aWRlcnMiOiIqIiwibGFuZyI6ImVuIiwic3RvcmFnZURvbWFpbk92ZXJyaWRlIjoiYXV0aC1pZC5uZmwuY29tIiwiY3VzdG9tRXZlbnRNYXAiOnsiZXZlbnRNYXAiOlt7ImV2ZW50cyI6IioiLCJhcmdzIjpbbnVsbF19XX0sIkFQSUtleSI6IjRfOWlKVmtUeXJPek1KbFV1NjZaQlJLZyJ9LCJvcGVyYXRpb24iOiIvYWNjb3VudHMuZ2V0QWNjb3VudEluZm8ifQ=="
)

headers <- httr::add_headers(
  `User-Agent` = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:129.0) Gecko/20100101 Firefox/129.0",
  `Authorization` = "Bearer eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJjbGllbnRJZCI6ImU1MzVjN2MwLTgxN2YtNDc3Ni04OTkwLTU2NTU2ZjhiMTkyOCIsImNsaWVudEtleSI6IjRjRlVXNkRtd0pwelQ5TDdMckczcVJBY0FCRzVzMDRnIiwiaXNzIjoiTkZMIiwiZGV2aWNlSWQiOiJlODQxNzkzYi00YTY3LTRjYzEtOGJjNi1kOWZjMDgxMDIwZDAiLCJwbGFucyI6W3sicGxhbiI6ImZyZWUiLCJleHBpcmF0aW9uRGF0ZSI6IjIwMjUtMDktMzAiLCJzb3VyY2UiOiJORkwiLCJzdGFydERhdGUiOiIyMDI0LTA5LTI5Iiwic3RhdHVzIjoiQUNUSVZFIiwidHJpYWwiOmZhbHNlfSx7InBsYW4iOiJORkxfUExVU19QUkVNSVVNIiwicHVyY2hhc2VDaGFubmVsIjoiIiwiYmlsbGluZ1R5cGUiOiJzZWFzb25hbCIsImV4cGlyYXRpb25EYXRlIjoiMjAyNS0wOS0wMyIsImV4dGVybmFsU3Vic2NyaXB0aW9uSWQiOiI2NDk0NTQxMjMiLCJzb3VyY2UiOiJXRUIiLCJzdGFydERhdGUiOiIyMDIzLTA5LTExIiwic3RhdHVzIjoiQUNUSVZFIiwidHJpYWwiOnRydWV9LHsicGxhbiI6Ik5GTF9QTFVTX1BSRU1JVU0iLCJwdXJjaGFzZUNoYW5uZWwiOiIiLCJiaWxsaW5nVHlwZSI6InNlYXNvbmFsIiwiZXhwaXJhdGlvbkRhdGUiOiIyMDI1LTA5LTAzIiwiZXh0ZXJuYWxTdWJzY3JpcHRpb25JZCI6IjY0OTQ1NDEyMyIsInNvdXJjZSI6IldFQiIsInN0YXJ0RGF0ZSI6IjIwMjMtMDktMTEiLCJzdGF0dXMiOiJBQ1RJVkUiLCJ0cmlhbCI6dHJ1ZX1dLCJEaXNwbGF5TmFtZSI6IldFQl9ERVNLVE9QX0RFU0tUT1AiLCJOb3RlcyI6IiIsImZvcm1GYWN0b3IiOiJERVNLVE9QIiwibHVyYUFwcEtleSI6IlNaczU3ZEJHUnhiTDcyOGxWcDdEWVEiLCJwbGF0Zm9ybSI6IkRFU0tUT1AiLCJwcm9kdWN0TmFtZSI6IldFQiIsInJvbGVzIjpbImNvbnRlbnQiLCJleHBlcmllbmNlIiwiZm9vdGJhbGwiLCJ1dGlsaXRpZXMiLCJ0ZWFtcyIsInBsYXkiLCJsaXZlIiwiaWRlbnRpdHkiLCJuZ3Nfc3RhdHMiLCJwYXltZW50c19hcGkiLCJuZ3NfdHJhY2tpbmciLCJuZ3NfcGxhdGZvcm0iLCJuZ3NfY29udGVudCIsIm5nc19jb21iaW5lIiwibmdzX2FkdmFuY2VkX3N0YXRzIiwibmZsX3BybyIsImVjb21tIiwibmZsX2lkX2FwaSIsImZyZWUiLCJORkxfUExVU19QUkVNSVVNIiwiTkZMX1BMVVNfUFJFTUlVTSJdLCJjaXR5IjoieW9yayIsImNvdW50cnlDb2RlIjoiVVMiLCJkbWFDb2RlIjoiNTY2IiwiaG1hVGVhbXMiOlsiMTA0MDAzMjUtNDhkZS0zZDZhLWJlMjktOGY4Mjk0MzdmNGM4IiwiMTA0MDM3MDAtYjkzOS0zY2JkLTNkMTYtMjRkNGQ2NzQyZmEyIiwiMTA0MDM5MDAtODI1MS02ODkyLWQ4MWMtNDM0ODUyNWMyZDQ3Il0sInJlZ2lvbiI6IlBBIiwiemlwQ29kZSI6IjE3NDA4IiwiYnJvd3NlciI6IkZpcmVmb3giLCJjZWxsdWxhciI6ZmFsc2UsImVudmlyb25tZW50IjoicHJvZHVjdGlvbiIsInVpZCI6IjA5NTkwY2Q3MTA0ZDMxNmU0OWZmZDI5NDRmMTEzOWM1IiwiZXhwIjoxNzI3NjUwMDA1fQ.IQ5tUBNQ65_o5DRRpdA2-5zM44DdoOp_zAnzYObgAoc")

url <- glue::glue("https://pro.nfl.com/api/stats/players-offense/receiving/season") |> 
  httr::modify_url(
    query = list(
      limit = "35",
      offset = "0",
      page = "1",
      sortKey = "ydsPG",
      sortValue = "DESC",
      season = "2024",
      qualifiedReceiver = "true",
      seasonType = "REG"
    )
  )


game_id <- schedule_df |> 
  filter(week == 2 &
           home_team == "PHI") |> 
  pull(old_game_id)

url <- glue::glue("https://pro.nfl.com/api/stats/gamecenter") |> 
  httr::modify_url(
    query = list(
      gameId = game_id
    )
  )

res <- httr::GET(
  url, 
  headers,
  config = cookies
)

teams <- c("home", "visitor")

map_df(
  teams,
  ~ jsonlite::fromJSON(httr::content(res, "text")) |> 
    pluck("passers") |> 
    pluck(.x)
) |> 
  flatten() |> 
  tibble() |> 
  write_csv(glue("~/Downloads/{game_id}_passers.csv"))

# jsonlite::fromJSON(httr::content(res, "text"))$receivers |> 
#   tibble() |> 
#   write_csv("~/Downloads/season_receiving_stats.csv")



get_nflpro_single_game_table <- function(table_type = 'passing', week = 1, season = 2024) {
  
  
  ### take a two second break in between calls
  ### only need this step if you're running this function repeatedly
  Sys.sleep(sample(2:7, 1))
  
  headers <- httr::add_headers(
    `User-Agent` = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:129.0) Gecko/20100101 Firefox/129.0",
    `Authorization` = "Bearer eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJjbGllbnRJZCI6ImU1MzVjN2MwLTgxN2YtNDc3Ni04OTkwLTU2NTU2ZjhiMTkyOCIsImNsaWVudEtleSI6IjRjRlVXNkRtd0pwelQ5TDdMckczcVJBY0FCRzVzMDRnIiwiaXNzIjoiTkZMIiwiZGV2aWNlSWQiOiJlODQxNzkzYi00YTY3LTRjYzEtOGJjNi1kOWZjMDgxMDIwZDAiLCJwbGFucyI6W3sicGxhbiI6ImZyZWUiLCJleHBpcmF0aW9uRGF0ZSI6IjIwMjUtMDktMjUiLCJzb3VyY2UiOiJORkwiLCJzdGFydERhdGUiOiIyMDI0LTA5LTI1Iiwic3RhdHVzIjoiQUNUSVZFIiwidHJpYWwiOmZhbHNlfSx7InBsYW4iOiJORkxfUExVU19QUkVNSVVNIiwicHVyY2hhc2VDaGFubmVsIjoiIiwiYmlsbGluZ1R5cGUiOiJzZWFzb25hbCIsImV4cGlyYXRpb25EYXRlIjoiMjAyNS0wOS0wMyIsImV4dGVybmFsU3Vic2NyaXB0aW9uSWQiOiI2NDk0NTQxMjMiLCJzb3VyY2UiOiJXRUIiLCJzdGFydERhdGUiOiIyMDIzLTA5LTExIiwic3RhdHVzIjoiQUNUSVZFIiwidHJpYWwiOnRydWV9LHsicGxhbiI6Ik5GTF9QTFVTX1BSRU1JVU0iLCJwdXJjaGFzZUNoYW5uZWwiOiIiLCJiaWxsaW5nVHlwZSI6InNlYXNvbmFsIiwiZXhwaXJhdGlvbkRhdGUiOiIyMDI1LTA5LTAzIiwiZXh0ZXJuYWxTdWJzY3JpcHRpb25JZCI6IjY0OTQ1NDEyMyIsInNvdXJjZSI6IldFQiIsInN0YXJ0RGF0ZSI6IjIwMjMtMDktMTEiLCJzdGF0dXMiOiJBQ1RJVkUiLCJ0cmlhbCI6dHJ1ZX1dLCJEaXNwbGF5TmFtZSI6IldFQl9ERVNLVE9QX0RFU0tUT1AiLCJOb3RlcyI6IiIsImZvcm1GYWN0b3IiOiJERVNLVE9QIiwibHVyYUFwcEtleSI6IlNaczU3ZEJHUnhiTDcyOGxWcDdEWVEiLCJwbGF0Zm9ybSI6IkRFU0tUT1AiLCJwcm9kdWN0TmFtZSI6IldFQiIsInJvbGVzIjpbImNvbnRlbnQiLCJleHBlcmllbmNlIiwiZm9vdGJhbGwiLCJ1dGlsaXRpZXMiLCJ0ZWFtcyIsInBsYXkiLCJsaXZlIiwiaWRlbnRpdHkiLCJuZ3Nfc3RhdHMiLCJwYXltZW50c19hcGkiLCJuZ3NfdHJhY2tpbmciLCJuZ3NfcGxhdGZvcm0iLCJuZ3NfY29udGVudCIsIm5nc19jb21iaW5lIiwibmdzX2FkdmFuY2VkX3N0YXRzIiwibmZsX3BybyIsImVjb21tIiwibmZsX2lkX2FwaSIsImZyZWUiLCJORkxfUExVU19QUkVNSVVNIiwiTkZMX1BMVVNfUFJFTUlVTSJdLCJjaXR5IjoicGhpbGFkZWxwaGlhIiwiY291bnRyeUNvZGUiOiJVUyIsImRtYUNvZGUiOiI1MDQiLCJobWFUZWFtcyI6WyIxMDQwMzcwMC1iOTM5LTNjYmQtM2QxNi0yNGQ0ZDY3NDJmYTIiXSwicmVnaW9uIjoiUEEiLCJ6aXBDb2RlIjoiMTkxNDYiLCJicm93c2VyIjoiRmlyZWZveCIsImNlbGx1bGFyIjpmYWxzZSwiZW52aXJvbm1lbnQiOiJwcm9kdWN0aW9uIiwidWlkIjoiMDk1OTBjZDcxMDRkMzE2ZTQ5ZmZkMjk0NGYxMTM5YzUiLCJleHAiOjE3MjcyMzgyNDZ9.0PPHeDHn8izwDbTLVCOZ8Wp2OSQOF3jw8nACR6_S3aE")
  
  ### vector to map week to appropriate text
  season_length = ifelse(season < 2021, 17, 18)
  week_slug_vec = c(paste0('WEEK_', 1:season_length), 'WC', 'DIV', 'CONF', 'SB')
  
  ### proper table url
  url_modifier = ifelse(table_type == 'defending', 'defense/overview', paste0('players-offense/', table_type))
  
  params <- list(
    "season" = season,
    "week" = week_slug_vec[week],
    "limit" = "3997"
  )
  
  url <- glue::glue("https://pro.nfl.com/api/stats/{url_modifier}/week") |> 
    httr::modify_url(
      query = params
    )
  
  data <- httr::GET(
    url = url,
    headers
  ) |> 
    httr::content(as = 'parsed') |> 
    (function(i) i[[gsub('ing', 'ers', table_type)]])() |> 
    dplyr::bind_rows() |> 
    dplyr::mutate(
      season = season,
      table_type = table_type,
      week = week
    )
  
  return(data)
}

# passing, rushing, receiving, and defending table_type
pass_df <- get_nflpro_single_game_table(
  table_type = "passing",
  week = 1,
  season = 2024
)

rec_df <- get_nflpro_single_game_table(
  table_type = "receiving",
  week = 1,
  season = 2024
)

rush_df <- get_nflpro_single_game_table(
  table_type = "rushing",
  week = 1,
  season = 2024
)

def_df <- get_nflpro_single_game_table(
  table_type = "defending",
  week = 1,
  season = 2024
)


pass_df


# Next Gen Stats
ngs_highlights <- function(season, existing_plays=NULL) {
  
  offset <- 0
  limit <- 16
  
  highlights_df <- data.frame()
  
  params <- list(
    "limit" = limit,
    "season" = season
  )
  
  url <- glue::glue("https://nextgenstats.nfl.com/api/plays/highlights") |> 
    httr::modify_url(
      query = params
    )
  
  # cookies <- c(
  #   "nflcs.prod.crossDomainStorageCleared" = "true",
  #   "nflcs.prod.keyStoreDomainSyncList"	= "id.nfl.com",
  #   "nflcs.prod.nfl.user"	="eyJkYXRhIjoiIiwiZXhwIjoxNzI2OTQ0MzE3MTA4LCJmaXJzdE5hbWUiOiIiLCJnaWd5YVVJRCI6IiIsImdpZ3lhVUlEU2lnbmF0dXJlIjoiIiwiaGFzaGVkRW1haWwiOiIiLCJsYXN0TmFtZSI6IiJ9"
  # )
  
  headers <- c(
    "Host" = "nextgenstats.nfl.com",
    "credentials" = "include",
    "User-Agent" = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:129.0) Gecko/20100101 Firefox/129.0",
    "Accept" = "application/json, text/plain, */*",
    "Referer" = "https://nextgenstats.nfl.com/highlights/play-list"
  )
  
  if(is.null(existing_plays)) {
    while (TRUE) {
      
      print(params$offset)
      print(params$limit)
      print(url)
      
      res <- httr::GET(
        url,
        httr::add_headers(headers),
        httr::set_cookies(cookies)
      )
      
      print(glue("Status: {httr::status_code(res)}"))
      
      if (httr::status_code(res) == 200) {
        data <- jsonlite::fromJSON(httr::content(res, "text"))
        
        highlights_df <- data |> 
          pluck("highlights") |> 
          tibble() |> 
          mutate(season = season) |> 
          select(season,
                 game_id = gameId, 
                 play_id = playId) |> 
          bind_rows(highlights_df)
        
        if (nrow(highlights_df) == data$total) {
          break
        }
        
        # re-write limit based on offset to reduce calls and query everything
        params$offset <- offset + limit
        params$limit <- data |> 
          pluck("total") - params$offset
        
        url <- glue::glue("https://nextgenstats.nfl.com/api/plays/highlights") |> 
          httr::modify_url(
            query = params
          )
        
        Sys.sleep(5)
        
      } else {
        print(res$headers)
        break
      }
    }
  }
  return(highlights_df)
}

update_ngs_highlights_db <- function(highlights_df,
                                     con = fx.db_con(x.host = 'localhost')) {
  on.exit(dbDisconnect(con))
  
  table <- 'ngs_highlights'
  
  overwrite_data <- highlights_df |> 
    select(season, game_id) |>
    unique()
  
  sql_list_season <- paste0("IN (\'", paste(unique(overwrite_data$season), collapse = "', '"), "\')")
  
  sql_list_gameid <- paste0("IN (\'", paste(unique(overwrite_data$game_id), collapse = "', '"), "\')")
  
  if (dbExistsTable(con, table) == FALSE) {
    dbCreateTable(con,
                  table,
                  highlights_df)
  }
  if (dbExistsTable(con, table) == TRUE) {
    
    query <- glue(
      '
        DELETE
        FROM "{table}"
        WHERE
          season {sql_list_season}
          AND game_id {sql_list_gameid};
      '
    )
    print(query)
    dbExecute(con, query)
    
    dbWriteTable(con,
                 table,
                 highlights_df,
                 append = TRUE)
  }
}

highlights_df <- ngs_highlights(2024)

highlights_df

update_ngs_highlights_db(highlights_df)

ngs_get_play <- function(game_id, play_id) {
  
  play_df <- data.frame()
  
  params <- list(
    gameId = game_id,
    playId = play_id
  )
  
  url <- glue::glue("https://nextgenstats.nfl.com/api/highlights/tracking/game/play/withBall/min") |> 
    httr::modify_url(
      query = params
    )
  
  cookies <- c(
    "nflcs.prod.crossDomainStorageCleared" = "true",
    "nflcs.prod.keyStoreDomainSyncList" = "id.nfl.com",
    "nflcs.prod.nfl.user" = "eyJkYXRhIjoiIiwiZXhwIjoxNzI2OTQ0MzE3MTA4LCJmaXJzdE5hbWUiOiIiLCJnaWd5YVVJRCI6IiIsImdpZ3lhVUlEU2lnbmF0dXJlIjoiIiwiaGFzaGVkRW1haWwiOiIiLCJsYXN0TmFtZSI6IiJ9"
  )
  
  headers <- c(
    "Host" = "nextgenstats.nfl.com",
    "credentials" = "include",
    "User-Agent" = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:129.0) Gecko/20100101 Firefox/129.0",
    "Accept" = "application/json, text/plain, */*",
    "Referer" = "https://nextgenstats.nfl.com/highlights/play-list"
  )
  
  print(url)
  
  print("while")
  res <- httr::GET(
    url,
    httr::add_headers(headers),
    httr::set_cookies(cookies)
  )
  
  httr::status_code(res)
  
  if (httr::status_code(res) == 200) {
    data <- jsonlite::fromJSON(httr::content(res, "text"))
    
    play_events <- data.frame(t(sapply(data$submittedPlay, c))) |>
      janitor::clean_names() |>
      select(-game_event_source) |> 
      pivot_longer(cols = c("start", "play_start", "play_end", "end")) |>
      mutate(value = format(strptime(value, format = "%Y-%m-%dT%H:%M:%OS"), digits = 1L))
    
    play_df <- data$awayTrackingData |> 
      tibble() |> 
      janitor::clean_names() |> 
      unnest(player_tracking_data) |> 
      bind_rows(
        data$homeTrackingData |> 
          tibble() |> 
          janitor::clean_names() |> 
          unnest(player_tracking_data)
      ) |> 
      bind_rows(
        data$ballTrackingData |> 
          tibble() |> 
          janitor::clean_names() |> 
          mutate(
            display_name = "football"
          )
      ) |> 
      mutate(
        game_id = as.character(data$gameId),
        play_id = as.character(data$gsisPlayId),
        time = format(strptime(time, format = "%Y-%m-%dT%H:%M:%OS"), digits = 3L)
      ) |> 
      left_join(
        play_events,
        by = c("game_id", "play_id" = "gsis_play_id", "time" = "value")
      )
    
  }
  return(play_df)
}
# ngs_get_play(game_id = 2024092209, 1245)
