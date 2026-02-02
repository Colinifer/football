library(nflfastR)
library(tidyverse)
library(odbc)
library(RPostgres)

proj_name <- "football"

db_con <- function(
    driver = RPostgres::Postgres(),
    host = Sys.getenv("ip"),
    port = Sys.getenv("postgres_port"),
    user = Sys.getenv("db_user"),
    password = Sys.getenv("db_password"),
    dbname = proj_name)
{
  con <- dbConnect(
    driver,
    host = host,
    # host = ifelse(
    #   get_ip() == Sys.getenv('ip'),
    #   Sys.getenv('local'),
    #   x.host
    # ),
    port = port,
    user = user,
    password = password,
    dbname = dbname,
    # database = "football",
    # Server = "localhost\\SQLEXPRESS",
    # Database = "datawarehouse",
    NULL
  )
  return(con)
}

get_year <- function(x = as.Date(Sys.Date())) {
  # print(x)
  if (x %>% format("%m") %>% as.integer() > 08) {
    year <- x %>% format("%Y") %>% as.integer()
    return(year)
  }
  if (x %>% format("%m") %>% as.integer() <= 08) {
    year <- x %>% format("%Y") %>% as.integer() - 1
    return(year)
  }
}

current_season <- get_year()

# con_obj <- db_con(host = "host.docker.internal")
con_obj <- db_con(host = "localhost")
# con_obj <- db_con(
  # host = "68.163.50.241",
  # port = "4433"
# )
table_name <- "nflfastR_pbp"

pbp_df <- tbl(con_obj, table_name) |> 
  filter(season == current_season) |> 
  collect()

player_stats_season_df <- nflfastR::calculate_stats(
  seasons = current_season,
  summary_level = "season",
  stat_type = "player",
  season_type = "REG",
  pbp = pbp_df
)

# Check if table exists before attempting to delete
if (dbExistsTable(con_obj, "player_stats_season")) {
  query <- glue::glue_sql(
    "DELETE FROM
      nflfastR_player_stats_season
    WHERE
      season = {current_season}
    ", .con = con_obj)
  dbExecute(con_obj, query)
}

# Upload data, creating table 'player_stats_season' if it doesn't exist, and appending data
dbWriteTable(
  con_obj,
  "nflfastR_player_stats_season",
  player_stats_season_df,
  append = TRUE,
  row.names = FALSE
)

player_stats_week_df <- nflfastR::calculate_stats(
  seasons = current_season,
  summary_level = "week",
  stat_type = "player",
  season_type = "REG",
  pbp = pbp_df
)

# Check if table exists before attempting to delete
if (dbExistsTable(con_obj, "player_stats_week")) {
  query <- glue::glue_sql(
    "DELETE FROM
      nflfastR_player_stats_week
    WHERE
      season = {current_season}
    ", .con = con_obj)
  dbExecute(con_obj, query)
}

# Upload data, creating table 'player_stats_week' if it doesn't exist, and appending data
dbWriteTable(
  con_obj,
  "nflfastR_player_stats_week",
  player_stats_week_df,
  append = TRUE,
  row.names = FALSE
)

team_stats_season_df <- nflfastR::calculate_stats(
  seasons = current_season,
  summary_level = "season",
  stat_type = "team",
  season_type = "REG",
  pbp = pbp_df
)

# Check if table exists before attempting to delete
if (dbExistsTable(con_obj, "team_stats_season")) {
  query <- glue::glue_sql(
    "DELETE FROM
      nflfastR_team_stats_season
    WHERE
      season = {current_season}
    ", .con = con_obj)
  dbExecute(con_obj, query)
}

# Upload data, creating table 'team_stats_season' if it doesn't exist, and appending data
dbWriteTable(
  con_obj,
  "nflfastR_team_stats_season",
  team_stats_season_df,
  append = TRUE,
  row.names = FALSE
)

team_stats_week_df <- nflfastR::calculate_stats(
  seasons = current_season,
  summary_level = "week",
  stat_type = "team",
  season_type = "REG",
  pbp = pbp_df
)

# Check if table exists before attempting to delete
if (dbExistsTable(con_obj, "team_stats_week")) {
  query <- glue::glue_sql(
    "DELETE FROM
      nflfastR_team_stats_week
    WHERE
      season = {current_season}
    ", .con = con_obj)
  dbExecute(con_obj, query)
}

# Upload data, creating table 'team_stats_week' if it doesn't exist, and appending data
dbWriteTable(
  con_obj,
  "nflfastR_team_stats_week",
  team_stats_week_df,
  append = TRUE,
  row.names = FALSE
)

dbDisconnect(con_obj)