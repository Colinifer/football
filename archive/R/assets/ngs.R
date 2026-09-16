library(httr)
library(jsonlite)

# NFL Pro

url <- "https://pro.nfl.com/api/stats/players-offense/receiving/season"

nflpro_auth_cookie <- fromJSON("config.json")$NFLPRO_COOKIE
nflpro_auth_token <- fromJSON("config.json")$NFLPRO_AUTH

nflpro_cookies <- httr::set_cookies(
  "nfl_web_sdk_plugin_storage" = ",percentPageViewed|lastRecPage|nfl pro:stats:receiving:season leaders|string,percentPageViewed|percentPageViewed|100|number,percentPageViewed|initialPageViewed|100|number,percentPageViewed|maxPageViewed|1172|number",
  "nflcs.prod.crossDomainStorageCleared" = "true",
  "nflcs.prod.keyStoreDomainSyncList"	= "id.nfl.com",
  "nflcs.prod.nfl.user"	= nflpro_auth_cookie
)

nflpro_headers <- httr::add_headers(
  `User-Agent` = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:129.0) Gecko/20100101 Firefox/129.0",
  `Authorization` = nflpro_auth_token,
  `Cache-Control`= "max-age=60",
  `Sec-Fetch-Dest`= "empty",
  `Sec-Fetch-Mode`= "cors",
  `Sec-Fetch-Site`= "same-origin",
  `Priority`= "u=0"
)

url <- glue::glue("https://pro.nfl.com/api/secured/stats/players-offense/receiving/season") |> 
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



get_nflpro_single_game_table <- function(
    token = nflpro_auth_token,
    table_type = 'passing', 
    week = 1, 
    season = nflreadr::most_recent_season()
    ) {
  
  
  ### take a two second break in between calls
  ### only need this step if you're running this function repeatedly
  Sys.sleep(sample(2:7, 1))
  
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
  
  url <- glue::glue("https://pro.nfl.com/api/secured/stats/{url_modifier}/season") |> 
    httr::modify_url(
      query = params
    )
  
  print(url)
  
  data <- httr::GET(
    url = url,
    httr::add_headers(Authorization = token)
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

get_nflpro_pbp <- function(
    token = nflpro_auth_token, 
    old_game_id
) {
  
  
  ### take a two second break in between calls
  ### only need this step if you're running this function repeatedly
  Sys.sleep(sample(2:7, 1))
  
  params <- list(
    "gameId" = old_game_id
  )
  
  url <- glue::glue("https://pro.nfl.com/api/secured/plays/playlist/game") |> 
    httr::modify_url(
      query = params
    )
  
  print(url)
  
  data <- httr::GET(
    url = url,
    httr::add_headers(Authorization = token)
  ) |> 
    httr::content(as = 'text') |> 
    fromJSON(simplifyDataFrame = TRUE) |> 
    as_tibble() |> 
    flatten() |> 
    as_tibble()
  
  return(data)
}

# passing, rushing, receiving, and defending table_type
pass_df <- get_nflpro_single_game_table(
  token = nflpro_auth_token,
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

nflpro_pbp_df <- get_nflpro_pbp(
  token = nflpro_auth_token,
  old_game_id = 2024122600
)


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
