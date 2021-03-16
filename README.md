README
================

## Setup/Installation

Most of this repo relies on a personal package I created and
nflfastR(‘<https://github.com/mrcaseb/nflfastR>’) but with some simple
setup, most of the scripts will automatically render visualizations
from NFL data.

## Create standard session objects/variables

The functions below help to load standard data and create standard
environment variables used in other scripts.

``` r
fx.setdir(proj_name)
#> [1] "Goose (iMac) is ready for some football"

year <- fx.get_year()

fx.get_sleeper_api_players()
# source("fantasy_football/ff_init.R")
fx.get_espn_players()

# nflfastR data
roster_df <-
  readRDS(
    url(
      'https://github.com/guga31bb/nflfastR-data/blob/master/roster-data/roster.rds?raw=true'
    )
  ) %>%
  as_tibble()

schedule_df <- fast_scraper_schedules(seasons = year, pp = TRUE)
#> ℹ You have passed only 1 season(s) to parallel processing.
#>   Please note that the initiating process takes a few seconds
#>   and consider using `pp = FALSE` for a small number of seasons.

schedule_df %>% 
  saveRDS(glue('data/schedules/sched_{year}.rds'))
  
matchup_df <- schedule_df %>% 
  mutate(posteam = home_team,
         oppteam = away_team) %>%
  select(
    game_id,
    season,
    game_type,
    week,
    gameday,
    weekday,
    gametime,
    posteam,
    oppteam,
    away_team,
    home_team,
    away_score,
    home_score,
    home_result,
    stadium,
    location,
    roof,
    surface,
    old_game_id
  ) %>% rbind(
    schedule_df %>%
      mutate(posteam = away_team,
             oppteam = home_team) %>%
      select(
        game_id,
        season,
        game_type,
        week,
        gameday,
        weekday,
        gametime,
        posteam,
        oppteam,
        away_team,
        home_team,
        away_score,
        home_score,
        home_result,
        stadium,
        location,
        roof,
        surface,
        old_game_id
      )
  ) %>% arrange(old_game_id)

sr_games_df <- readRDS('data/schedules/sportradar/games_2020.rds')

# source('data/master_sr_pbp.R')
pbp_df <- readRDS(glue('data/pbp/play_by_play_{year}.rds'))
pbp_df %>% select(game_date) %>% arrange(game_date) %>%  unique() %>%  tail()
#> # A tibble: 6 x 1
#>   game_date 
#>   <chr>     
#> 1 2020-12-20
#> 2 2020-12-21
#> 3 2020-12-25
#> 4 2020-12-26
#> 5 2020-12-27
#> 6 2020-12-28
pbp_df %>% select(game_id) %>% unique() %>% tail()
#> # A tibble: 6 x 1
#>   game_id        
#>   <chr>          
#> 1 2020_16_IND_PIT
#> 2 2020_16_LA_SEA 
#> 3 2020_16_NYG_BAL
#> 4 2020_16_PHI_DAL
#> 5 2020_16_TEN_GB 
#> 6 2020_16_BUF_NE
```

Loading parquet/arrow files is fastest for multiple seasons of data.

``` r
part_ds <- open_dataset('data/part/sportradar', partitioning = 'year')
pbp_ds <- open_dataset('data/pbp/fastr', partitioning = 'year')
xyac_ds <- open_dataset('data/pbp/xyac', partitioning = 'year')
sr_pbp_df <- readRDS('data/pbp/sportradar/sr_pbp_2020.rds')
```

Load the custom/person GG theme for plots


## Plots

Team wins over/under expected
<img src="https://raw.githubusercontent.com/Colinifer/football/master/plots/desktop/team_wins/wins_above_expected_scatter_2020.png" width="100%" style="display: block; margin: auto;" />


QB Passing Efficiency as a function of distance
<img src="https://raw.githubusercontent.com/Colinifer/football/master/plots/desktop/qb_cpoe_vs_dot/qb_cpoe_vs_dot_2020.png" width="100%" style="display: block; margin: auto;" />


QB CPOE and EPA by game
<img src="https://raw.githubusercontent.com/Colinifer/football/master/plots/desktop/qb_epa_vs_cpoe/qb_epa_vs_cpoe_2020.png" width="100%" style="display: block; margin: auto;" />


QB Passing Efficiency compared to O-line pass block win rate
<img src="https://raw.githubusercontent.com/Colinifer/football/master/plots/desktop/qb_passing/pb_pass_dakota_pbwr_2020.png" width="100%" style="display: block; margin: auto;" />
