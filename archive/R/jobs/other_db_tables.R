con <- dbConnect(
  RPostgres::Postgres(),
  host = ifelse(
    fromJSON(
      readLines("http://api.hostip.info/get_json.php",
                warn = F)
    )$ip == Sys.getenv('ip'),
    Sys.getenv('local'),
    Sys.getenv('ip')
  ),
  port = Sys.getenv('postgres_port'),
  user = Sys.getenv('db_user'),
  password = Sys.getenv('db_password'),
  dbname = proj_name,
  # database = "football",
  # Server = "localhost\\SQLEXPRESS",
  # Database = "datawarehouse",
  NULL
)

schedule_df <- fast_scraper_schedules(1999:2020)
roster_df <- fast_scraper_roster(1999:2020)
draft_df <- read_csv(url('https://github.com/nflverse/nfldata/raw/master/data/draft_picks.csv'))

RPostgres::dbWriteTable(
  con,
  'schedule',
  schedule_df)

RPostgres::dbWriteTable(
  con,
  'rosters',
  roster_df)

RPostgres::dbWriteTable(
  con,
  'draft',
  draft_df)

dbDisconnect(con)