pbp_df |> 
  filter(qb_dropback == 1 & !is.na(passer_player_id)) |> 
  select(passer_player_name, contains('epa')) |> 
  group_by(passer_player_name) %>%
  summarise(across(names(select(pbp_df, contains('epa'))), sum),
            .groups = 'drop') |> arrange(-total_epa)

waterfalls::waterfall(pbp_df |> 
            filter(qb_dropback == 1 & !is.na(passer_player_id)) |> 
            select(passer_player_name, contains('epa')) |> 
            group_by(passer_player_name) %>%
            summarise(across(names(select(pbp_df, contains('epa'))), sum),
                      .groups = 'drop') |> arrange(-epa))
