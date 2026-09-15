library(ffscrapr)
library(dplyr)
library(tidyr)

pbp_preprocess <- ep_preprocess(pbp_df)
  
pbp_predict <- ep_predict(pbp_preprocess)

ffopportunity <- ep_summarize(pbp_predict)

ffopportunity |> 
  filter(!is.na(player_id)) |> 
  group_by(player_id) |> 
  summarise(full_name = first(full_name),
            position = first(position),
            games = n(),
            total_fantasy_points = sum(total_fantasy_points, na.rm = T),
            total_fantasy_points_exp = sum(total_fantasy_points_exp, na.rm = T)
  ) |> 
  mutate(total_fantasy_points_exp_pg = total_fantasy_points_exp / games,
         total_fantasy_points_diff = total_fantasy_points - total_fantasy_points_exp) |> 
  arrange(total_fantasy_points_diff) |> 
  filter(position %in% c("RB", "TE", "WR")) |> 
  left_join(
    beepboop_rosters |> 
      select(
        franchise_name,
        espn_id = player_id,
        player_name
      ) |> 
      left_join(
        dp_playerids() |> 
          select(
            espn_id,
            player_id = gsis_id
          ) |> 
          filter(!is.na(espn_id)) |> 
          mutate(
            espn_id = as.numeric(espn_id)
          ),
        by = c("espn_id")
      ),
    by = c("player_id"),
    relationship = "many-to-many"
  ) |> 
  filter(is.na(franchise_name)) |> 
  arrange(desc(total_fantasy_points_exp))

beepboop <- espn_connect(season = current_season, 
                          league_id = initR::fantasy_key |> 
                            pull(league_id) |> 
                            nth(1),
                         espn_s2 = espn_s2,
                         swid = swid
)

beepboop

beepboop_rosters <- ffscrapr::ff_rosters(beepboop)

kepler <- espn_connect(season = current_season, 
                         league_id = initR::fantasy_key |> 
                           pull(league_id) |> 
                           nth(2),
                         espn_s2 = espn_s2,
                         swid = swid
)

kepler

kepler_rosters <- ffscrapr::ff_rosters(kepler)

ffopportunity

player_values <- dp_values("values-players.csv")

# The values are stored by fantasypros ID since that's where the data comes from. 
# To join it to our rosters, we'll need playerID mappings.

player_ids <- dp_playerids() %>% 
  select(espn_id,fantasypros_id) %>% 
  filter(!is.na(espn_id),!is.na(fantasypros_id))

# We'll be joining it onto rosters, so we can trim down the values dataframe
# to just IDs, age, and values

player_values <- player_values %>% 
  left_join(player_ids, by = c("fp_id" = "fantasypros_id")) %>% 
  select(espn_id,age,ecr_1qb,ecr_pos,value_1qb)

# we can join the roster's player_ids on the values' espn_id, with a bit of a type conversion first
beepboop_values <- beepboop_rosters %>% 
  mutate(player_id = as.character(player_id)) %>% 
  left_join(player_values, by = c("player_id"="espn_id")) %>% 
  arrange(franchise_id,desc(value_2qb))

head(beepboop_values)

value_summary <- beepboop_values %>% 
  group_by(franchise_id,franchise_name,pos) %>% 
  summarise(total_value = sum(value_2qb,na.rm = TRUE)) %>%
  ungroup() %>% 
  group_by(franchise_id,franchise_name) %>% 
  mutate(team_value = sum(total_value)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = pos, values_from = total_value) %>% 
  arrange(desc(team_value)) %>% 
  select(franchise_id,franchise_name,team_value,QB,RB,WR,TE)

kepler_values <- kepler_rosters %>% 
  mutate(player_id = as.character(player_id)) %>% 
  left_join(player_values, by = c("player_id"="espn_id")) %>% 
  arrange(franchise_id,desc(value_1qb))

head(kepler_values)

value_summary <- kepler_values %>% 
  group_by(franchise_id,franchise_name,pos) %>% 
  summarise(total_value = sum(value_1qb,na.rm = TRUE)) %>%
  ungroup() %>% 
  group_by(franchise_id,franchise_name) %>% 
  mutate(team_value = sum(total_value)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = pos, values_from = total_value) %>% 
  arrange(desc(team_value)) %>% 
  select(franchise_id,franchise_name,team_value,QB,RB,WR,TE)

value_summary

value_summary_pct <- value_summary %>% 
  mutate_at(c("team_value","QB","RB","WR","TE"),~.x/sum(.x)) %>% 
  mutate_at(c("team_value","QB","RB","WR","TE"),round, 3)

value_summary_pct

age_summary <- beepboop_values %>% 
  filter(pos %in% c("QB","RB","WR","TE")) %>% 
  group_by(franchise_id,pos) %>% 
  mutate(position_value = sum(value_1qb,na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(weighted_age = age*value_1qb/position_value,
         weighted_age = round(weighted_age, 1)) %>% 
  group_by(franchise_id,franchise_name,pos) %>% 
  summarise(count = n(),
            age = sum(weighted_age,na.rm = TRUE)) %>% 
  pivot_wider(names_from = pos,
              values_from = c(age,count))

age_summary
