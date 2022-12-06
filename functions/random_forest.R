library(randomForest)
library(caTools)
library(caret)

con <- fx.db_con(x.host = 'localhost')
rf_all <-  tbl(con, 'nflfastR_player_stats')  |> 
  filter(season == current_season) |> 
  collect() |> 
  left_join(tbl(con, 'nflfastR_rosters') |> 
              filter(season == current_season) |>
              collect() |>
              select(season, gsis_id, position),
            by = c('season', 'player_id' = 'gsis_id')) |>
  select(season, player_id, player_name, position, 7:ncol(player_stats)) |> 
  filter(position %in% c('QB', 'WR', 'RB', 'TE') & 
           fantasy_points_half_ppr > 5) |> 
  mutate(position = as.factor(position))
dbDisconnect(con)

rf_all

#make this example reproducible
set.seed(1)

#use 70% of dataset as training set and 30% as test set
sample <- sample.split(rf_all$player_id, SplitRatio = 0.7)
rf_train  <- subset(rf_all, sample == TRUE)
rf_test   <- subset(rf_all, sample == FALSE)

# Run the random forest classification
rf_pos <- randomForest(position ~ carries + rushing_yards + rushing_fumbles + rushing_tds + targets +
               receptions + receiving_yards + receiving_yards_after_catch + receiving_fumbles + 
               receiving_tds + receiving_air_yards + attempts + completions + passing_yards + 
               passing_air_yards + interceptions + passing_tds, 
             data=rf_train)

importance(rf_pos)

caret::confusionMatrix(predict(rf_pos, rf_test), rf_test$position)

rf_test |> 
  mutate(predicted_pos = predict(rf_pos, dplyr::cur_data())) |> 
  select(season, player_name, player_id, position, predicted_pos, fantasy_points_half_ppr, everything()) |> 
  filter(position != predicted_pos) |> 
  arrange(desc(fantasy_points_half_ppr)) |> 
  as_tibble()

rf_df <- rf_train |> 
  select(1:4) |> 
  cbind(as.data.frame(rf_pos$votes)) |> 
  as_tibble()

rf_df |> 
  filter(position == 'TE') |> 
  arrange(desc(WR))

rf_df |> 
  filter(position == 'RB') |> 
  arrange(desc(WR))

rf_df |> 
  filter(position == 'QB') |> 
  arrange(QB)

print(rf_pos)

importance(rf_pos,type = 2)


# Count fantasy teams for replacement level
f_teams <- 10

# Total exp players
# QB
(f_teams*1.5) + 
  # RB
  (f_teams*7) + 
  # WR
  (f_teams*7) + 
  # TE
  (f_teams*1.5)


replacement_ranks <- player_stats |>
  left_join(roster_df |>
              select(gsis_id, position),
            by = c('player_id' = 'gsis_id')) |> 
  filter(position %in% c('QB', 'WR', 'RB', 'TE')) |> 
  group_by(position) |> 
  mutate(half_ppr_pg = fantasy_points_half_ppr / games) |> 
  arrange(desc(half_ppr_pg)) |> 
  mutate(rank = order(half_ppr_pg, decreasing=T)) |> 
  select(position, half_ppr_pg, rank) |> 
  filter((position == 'QB' & rank <= (f_teams*1.5)) | 
           (position == 'WR' & rank <= (f_teams*7)) | 
           (position == 'RB' & rank <= (f_teams*7)) | 
           (position == 'TE' & rank <= (f_teams*2))) |> 
  ggplot(aes(half_ppr_pg, fill = position, color = position)) + 
  geom_density(alpha = 0.1)
  
