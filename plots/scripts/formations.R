#Load packages
library(nflreadr)
library(scales)
library(tidyverse)


#create dataset
con <- fx.db_con(x.host = 'localhost')

players_df <- tbl(con, 'nflfastR_players') |> 
  collect()

part_df <- tbl(con, 'nflfastR_participation') |> 
  collect()

pbp_df <- tbl(con, 'nflfastR_pbp') |> 
  filter(season >= 2016) |> 
  collect()

dbDisconnect(con)

pbp_withpart <- left_join(pbp_df, part_df, by = c('old_game_id' = 'old_game_id', 'play_id' = 'play_id'))

part_df |> 
  filter(!is.na(offense_formation)) |> 
  group_by(possession_team) |> 
  count()

part_df |> 
  select(old_game_id, 
         play_id,
         offense_players,
         # defense_players,
         NULL
         ) |> 
  mutate(offense_players = case_when(offense_players == '' ~ as.character(NA),
                                     TRUE ~ offense_players)) |> 
  separate(offense_players,
           paste0("player_", 1:11), 
           sep=";", 
           remove=FALSE) |> 
  mutate(player = str_count(offense_players, ";")+1) |>
  select(-offense_players) |> 
  pivot_longer(cols = contains('player_'), values_to = 'gsis_id') |> 
  select(-name) |> 
  left_join(
    players_df |> 
      filter(!is.na(gsis_id)) |> 
      select(gsis_id,
             player_name = display_name,
             position_group,
             position),
    by = c('gsis_id')
  )
  

pbp_withpart |> 
  filter(season == 2021 & 
            !is.na(offense_formation) & 
           play_type %in% c('run', 'pass') &
           wp <= 0.8 & ep >= .2) |> 
  group_by(possession_team,
           offense_formation, 
           play_type) |> 
  count() |> 
  left_join(
    pbp_withpart |> 
      filter(season == 2021 & 
               !is.na(offense_formation) & 
               play_type %in% c('run', 'pass') &
               wp <= 0.8 & ep >= .2) |> 
      group_by(possession_team) |> 
      count(name = 'total'),
    by = 'possession_team'
  ) |> 
  mutate(n = n/total) |> 
  ggplot(aes(x = n, y = offense_formation, fill = play_type)) + 
  geom_bar(stat = 'identity') + 
  facet_wrap(~possession_team, nrow = 4) + 
  scale_x_continuous(limits = c(0,1),
                     labels = scales::percent)

#create chart
pbp_withpart |> 
  filter(!is.na(offense_personnel), !is.na(epa), !is.na(xpass),
         play_type == "run" | play_type == "pass",
         qb_kneel != 1, qb_spike != 1,
         defteam == "DAL",
         season == 2021, down == 3, ydstogo <= 10) |>
  mutate(parsons_on_field = str_detect(defense_players, '00-0036932')) |>
  ggplot(aes(ydstogo, epa, color = parsons_on_field, fill = parsons_on_field)) +
  geom_smooth(method = 'loess', span = 0.85) +
  theme_light() +
  theme(text = element_text(size = 17)) +
  scale_x_reverse(breaks= scales::pretty_breaks()) +
  labs(title = "3rd down defense, with and without Parsons",
       subtitle = "Dallas defensive results on 3rd down, 2021",
       y = "EPA by opposing offense",
       x = "Yards to go on 3rd down",
       caption = "Data from nflfastR + nflreadr | Chart by @CowboysStats")
