library(arrow)

lapply(1999:2020, function(yr){
  pbp <- readRDS(url(
    glue(
      'https://github.com/guga31bb/nflfastR-data/blob/master/data/play_by_play_{yr}.rds?raw=true'
    )
  ))
  saveRDS(pbp, glue('data/schedules/sched_{yr}.rds'))
  schedule <- readRDS(url(
    glue(
      'https://github.com/guga31bb/nflfastR-data/blob/master/schedules/sched_{yr}.rds?raw=true'
    )
  ))
  saveRDS(schedule, glue('data/schedules/sched_{yr}.rds'))
})

all_pbp_df <-
  do.call(rbind, lapply(dir(glue('data/pbp'), 
                            full = T) %>% 
                          .[which(grepl('.rds', .) &
                                    !grepl('xyac', .))], 
                        function(x){
                          print(x)
                          readRDS(x)})
  )

write_feather(all_pbp_df, "data/pbp/pbp_all", compression = "uncompressed")




# Feather -----------------------------------------------------------------

# create function for partition directories and download parquet files
get_data <- function(yr){
  dir.create(glue('data/pbp/fastr/{yr}'))
  
  download.file(
    glue::glue('https://github.com/guga31bb/nflfastR-data/blob/master/data/play_by_play_{yr}.parquet?raw=true'),
    glue('data/pbp/fastr/{yr}/pbp_{yr}.parquet'),
    mode = 'wb'
  )
}

# create folder and download for each year
lapply(2020, get_data)

pbp_ds <- open_dataset('data/pbp/fastr', partitioning = 'year')

# Create parquet files for xyac
# create function for partition directories and download parquet files
get_xyac_data <- function(yr){
  dir.create(glue('data/pbp/xyac/{yr}'))
  
  pbp <- readRDS(
    glue('data/pbp/xyac_play_by_play_{yr}.rds')
  )
  
  write_parquet(pbp, glue('data/pbp/xyac/{yr}/xyac_pbp_{yr}.parquet'))
}

# create folder and download for each year
lapply(2006:2020, get_xyac_data)

# open connection as arrow
xyac_ds <- open_dataset('data/pbp/xyac', partitioning = 'year')




# Samples -----------------------------------------------------------------

# Subset of years
tic()
pbp_ds %>% 
  select(year, play_type, yards_gained, epa, penalty, season) %>%
  filter(year %in% c(1999, 2019),
         play_type %in% c('run', 'pass'), penalty == 0) %>% 
  collect() %>% 
  group_by(season, play_type) %>% 
  summarize(
    avg_yds = mean(yards_gained, na.rm = TRUE),
    avg_epa = mean(epa, na.rm = TRUE),
    n = n()
  ) 
toc()


# All years
tic()
pbp_ds %>% 
  select(year, play_type, yards_gained, epa, penalty, season) %>%
  filter(play_type %in% c("run", "pass"), penalty == 0) %>% 
  collect() %>% 
  group_by(season, play_type) %>% 
  summarize(
    avg_yds = mean(yards_gained, na.rm = TRUE),
    avg_epa = mean(epa, na.rm = TRUE),
    n = n()
  ) 
toc()