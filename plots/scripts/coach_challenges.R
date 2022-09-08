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
  unique() |> 
  as_tibble() |> 
  mutate(team = paste0(team, ' challenged')) |> 
  pull(team)

fx.challenge_team <- function(x.challenge_desc = challenge_desc, 
         x.home_team_city = home_team_city, 
         x.home_team = home_team,
         x.away_team_city = away_team_city, 
         x.away_team = away_team){
  if(grepl(x.home_team_city, x.challenge_desc) == TRUE) {
    return(x.home_team)
  }
  if(grepl(x.away_team_city, x.challenge_desc) == TRUE) {
    return(x.away_team)
  }
}

current_coaches = pbp_df |> 
  select(coach = home_coach) |> 
  rbind(pbp_df |> 
          select(coach = away_coach)) |> 
  unique() |> 
  pull(coach)

tbl(con, 'nflfastR_pbp') |> 
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
  collect() |> 
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
  group_by(desc) |> 
  mutate(
    challenge_team = case_when(
      grepl(home_team_city, challenge_desc) == TRUE ~ home_team,
      grepl(away_team_city, challenge_desc) == TRUE ~ away_team,
      TRUE ~ as.character(NA),
    ),
    challenge_coach = case_when(
      challenge_team == home_team ~ home_coach,
      challenge_team == away_team ~ away_coach
    ),
    is_challenge = 1,
    win_challenge = ifelse(replay_or_challenge_result == 'reversed', 1, 0)
  ) |> 
  ungroup() |> 
  group_by(challenge_coach) |> 
  summarise(
    total_challenges = sum(is_challenge, na.rm = T),
    challenges_won = sum(win_challenge, na.rm = T)
  ) |> 
  mutate(
    challenge_win_pct = challenges_won / total_challenges
  ) |> 
  filter(challenge_coach %in% current_coaches) |> 
  arrange(-challenge_win_pct)


# 9 instances since 1999 of coaches challenges being denied