con <- fx.db_con(x.host = 'localhost')
# tbl(con, 'nflfastR_pbp') |>
team_challenge_vector <- teams_colors_logos |> 
  mutate(team_city = str_remove(team_name,
                                glue(' {team_nick}'))) |> 
  select(
    team = team_name
  ) |> rbind(
    teams_colors_logos |> 
      mutate(team_city = str_remove(team_name,
                                    glue(' {team_nick}'))) |> 
      select(
        team = team_city
      )
  ) |> 
  as_tibble() |> 
  mutate(team = paste0(team, ' challenged')) |> 
  pull(team)

pbp_df |> 
  filter(replay_or_challenge == 1 &
           replay_or_challenge_result %in% c('reversed', 'upheld')) |>
  select(
    game_id,
    replay_or_challenge_result,
    desc,
    home_team,
    home_coach,
    away_team,
    away_coach,
    posteam,
    defteam
  ) |> 
  left_join(
    teams_colors_logos |> 
      mutate(team_city = str_remove(team_name,
                                    glue(' {team_nick}'))) |> 
      select(home_team = team_abbr, home_team_city = team_city),
    by = c('home_team')
  ) |> 
  left_join(
    teams_colors_logos |> 
      mutate(team_city = str_remove(team_name,
                                    glue(' {team_nick}'))) |> 
      select(away_team = team_abbr, away_team_city = team_city),
    by = c('away_team')
  ) |> 
  mutate(challenge_desc = case_when(grepl('The Replay Official',
                                          desc) == FALSE ~ sub(".*)\\. ", "", sub(" challenged.*", "", desc)),
         TRUE ~ as.character(NA)),
          challenge_desc = case_when(
            length(challenge_desc) > 20 ~ sub(".*\\. ", "", challenge_desc),
         TRUE ~ challenge_desc)#,
         # challenge_desc = case_when(
         #   grepl('YAC', challenge_desc) == TRUE ~ sub(".* ", "", challenge_desc),
         #   TRUE ~ challenge_desc)
         ) |>
  mutate(challenge_team = case_when(
    grepl('The Replay Official', desc) == FALSE & grepl(home_team_city, challenge_desc) == TRUE ~ home_team,
    grepl('The Replay Official', desc) == FALSE & grepl(away_team_city, challenge_desc) == TRUE ~ away_team,
    TRUE ~ as.character(NA)
  )) |> 
  select(challenge_team, challenge_desc) |> 
  filter(!is.na(challenge_desc))
  # collect() |> 
  mutate(challenge_team = case_when(
    grepl(team_challenge_vector, desc) == TRUE ~ 
  ),
         is_challenge = 1,
         win_challenge = ifelse(replay_or_challenge_result == 'reversed', 1, 0))


# 9 instances since 1999 of coaches challenges being denied