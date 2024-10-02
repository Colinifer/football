# https://www.statology.org/principal-components-analysis-in-r/

part_df <- nflreadr::load_participation(include_pbp = T)

prcomp_df <- part_df |> 
  filter(!is.na(play_type) & 
           play_type %in% c('run', 'pass')) |> 
  select(posteam, qtr, game_seconds_remaining, down, ydstogo, yardline_100, play_type, offense_formation, offense_personnel,
         run_location, run_gap, pass_location, air_yards, qb_dropback, no_huddle) |> 
  select(-play_type, -offense_formation, -offense_personnel, -pass_location, -run_location, -run_gap)

prcomp_df <- team_stats |> 
  filter(season_type == 'REG') |> 
  # select(-season, -season_type, -games, -sacks, -sack_yards, -sack_fumbles, -sack_fumbles_lost, -interceptions)
  select(team, contains('epa'))

df <- as_tibble(unclass(prcomp_df), stringsAsFactors = TRUE)

df <- df |> 
  column_to_rownames(var = 'team')

pr_results <- prcomp(df
       ,scale = TRUE)

biplot(pr_results, scale = 0)

  

# EPA Composition ---------------------------------------------------------

epa_per_play <- function(x) {
  var <- x / qb_dropback
  return(var)
}

epa_vars <-
  pbp_df |> select(game_id, posteam, contains('epa')) |> names()

pbp_df |> 
  mutate(passer_player_id = ifelse(qb_scramble == 1, rusher_player_id, passer_player_id), 
         passer_player_name = ifelse(qb_scramble == 1, rusher_player_name, passer_player_name),
         qb_throwaway = ifelse(qb_dropback == 1 & qb_scramble == 0 & sack == 0 & is.na(receiver_player_id), 1, 0)) |> 
  filter(qb_dropback == 1 & week <= 13) |> 
  # select(game_id, posteam, passer_player_id, 
  #        passer_player_name, qb_dropback, complete_pass, 
  #        air_yards, yards_after_catch, yards_gained) |> 
  group_by(passer_player_id, passer_player_name, posteam) |>
  mutate(epa_targeted_pass = ifelse(qb_scramble == 0 & sack == 0 & qb_throwaway == 0, epa, NA),
         scramble_epa = ifelse(qb_scramble == 1, epa, NA), 
         sack_epa = ifelse(sack == 1, epa, NA), 
         int_epa = ifelse(interception == 1, epa, NA), 
         incompletion_epa = ifelse(incomplete_pass == 1, epa, NA), 
         completion_epa = ifelse(complete_pass == 1, epa, NA), 
         penalty_epa = ifelse(penalty == 1, epa, NA)) |> 
  select(passer_player_id, qb_dropback, qb_scramble, sack, qb_throwaway, interception, complete_pass, incomplete_pass, penalty, epa,
         epa_targeted_pass, scramble_epa, sack_epa, int_epa, incompletion_epa, completion_epa, penalty_epa
         ) |>
  # mutate(throw_selection_vaa = (cpoe * (air_epa + xyac_epa)) - ((1-cpoe) * epa) ) |> 
  # select(passer_player_id, complete_pass, incomplete_pass, throw_selection_vaa) |> 
  summarise(epa = mean(epa, na.rm = T), 
            epa_targeted_pass = mean(epa_targeted_pass, na.rm = T),
            scramble_epa = mean(scramble_epa, na.rm = T),
            sack_epa = mean(sack_epa, na.rm = T),
            int_epa = mean(int_epa, na.rm = T),
            incompletion_epa = mean(incompletion_epa, na.rm = T),
            completion_epa = mean(completion_epa, na.rm = T),
            penalty_epa = mean(penalty_epa, na.rm = T),
            across(.fns = sum, na.rm = T)) |> 
  # filter(qb_dropback > 200)
  mutate(
    epa_targeted_pass = epa_targeted_pass / qb_dropback,
    scramble_rate = qb_scramble / qb_dropback,
    sack_rate = sack / qb_dropback,
    interception_rate = interception / qb_dropback,
    completion_rate = complete_pass / qb_dropback,
    penalty_rate = penalty / qb_dropback
  ) |> 
  ungroup() |> 
  group_by(posteam) |> 
  # filter for all QB1s
  arrange(-qb_dropback) |>
  filter(row_number() == 1) |> 
  # 
  ungroup() |> 
  mutate(
    scramble_vaa = scramble_epa - mean(pbp_df$epa[pbp_df$qb_scramble == 1]),
    sack_vaa = sack_epa - mean(sack_epa),
    int_vaa = int_epa - mean(int_epa),
    incompletion_vaa = incompletion_epa - mean(incompletion_epa),
    completion_vaa = completion_epa - mean(completion_epa),
    penalty_vaa = penalty_epa - mean(penalty_epa),
  ) |> 
  select(passer_player_name, qb_dropback, epa, contains('vaa'), everything()) |> 
  arrange(-epa)
  
  