README
================

## Setup/Installation

Most of this repo relies on a personal package I created ![InitR](https://github.com/Colinifer/initR) and
![nflfastR](https://github.com/mrcaseb/nflfastR) but with some simple
setup, most of the scripts will automatically render visualizations
from NFL data.

## Create standard session objects/variables

The functions below help to load standard data and create standard
environment variables used in other scripts.

``` r
fx.get_sleeper_api_players()
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
```

Loading parquet/arrow files is fastest for multiple seasons of data.

``` r
part_ds <- open_dataset('data/part/sportradar', partitioning = 'year')
pbp_ds <- open_dataset('data/pbp/fastr', partitioning = 'year')
xyac_ds <- open_dataset('data/pbp/xyac', partitioning = 'year')
sr_pbp_df <- readRDS('data/pbp/sportradar/sr_pbp_2020.rds')
```

## Plots

Team wins over/under expected
<img src="https://raw.githubusercontent.com/Colinifer/football/master/plots/desktop/team_wins/wins_above_expected_scatter_2020.png" width="100%" style="display: block; margin: auto;" />


QB Passing Efficiency as a function of throwing distance (air yards)
<img src="https://raw.githubusercontent.com/Colinifer/football/master/plots/desktop/qb_cpoe_vs_dot/qb_cpoe_vs_dot_2020.png" width="100%" style="display: block; margin: auto;" />


QB CPOE (completion percentage over expected) and EPA (expected points added) by game
<img src="https://raw.githubusercontent.com/Colinifer/football/master/plots/desktop/qb_epa_vs_cpoe/qb_epa_vs_cpoe_2020.png" width="100%" style="display: block; margin: auto;" />


QB Passing Efficiency compared to ESPN's O-line pass block win rate
<img src="https://raw.githubusercontent.com/Colinifer/football/master/plots/desktop/qb_passing/pb_pass_dakota_pbwr_2020.png" width="100%" style="display: block; margin: auto;" />
