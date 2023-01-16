# https://fivethirtyeight.com/features/which-nfl-running-backs-got-the-best-chances-in-week-1/
# https://ffopportunity.ffverse.com/reference/ep_build.html

# install.packages("ffopportunity", repos = c("https://ffverse.r-universe.dev", getOption("repos")))

library(ffopportunity)


ep <- pbp_df |> 
  ep_preprocess() |> 
  ep_predict() |> 
  ep_summarize()

ep |>
  filter(!is.na(player_id)) |> 
  select(
    week,
    season:position,
    total_fantasy_points,
    rush_fantasy_points_exp,
    rush_fantasy_points,
    rec_fantasy_points_exp,
    rec_fantasy_points,
    pass_fantasy_points_exp,
    pass_fantasy_points,
    total_fantasy_points_exp,
    total_fantasy_points_diff
  ) |>
  group_by(player_id, full_name, position) |>
  summarise(
    games = n(),
    rush_fantasy_points_exp = sum(rush_fantasy_points_exp, na.rm = T),
    rush_fantasy_points = sum(rush_fantasy_points, na.rm = T),
    rec_fantasy_points_exp = sum(rec_fantasy_points_exp, na.rm = T),
    rec_fantasy_points = sum(rec_fantasy_points, na.rm = T),
    pass_fantasy_points_exp = sum(pass_fantasy_points_exp, na.rm = T),
    pass_fantasy_points = sum(pass_fantasy_points, na.rm = T),
    total_fantasy_points = sum(total_fantasy_points, na.rm = T),
    total_fantasy_points_exp = sum(total_fantasy_points_exp, na.rm = T)
  ) |>
  mutate(
    rush_fantasy_points_diff = rush_fantasy_points - rush_fantasy_points_exp,
    rec_fantasy_points_diff = rec_fantasy_points - rec_fantasy_points_exp,
    pass_fantasy_points_diff = pass_fantasy_points - pass_fantasy_points_exp,
    total_fantasy_points_diff = total_fantasy_points - total_fantasy_points_exp,
  ) |> 
  left_join(
    fx.ff_free_agents(player_stats, 'Beep Boop') |> 
      select(player_id, franchise_name) |> 
      mutate(roster_status = 'free_agent') |> 
      rbind(
        fantasy_rosters |> 
          filter(league == 'Beep Boop' & franchise_name != 'Love Daktually') |> 
          select(espn_id = player_id, franchise_name) |> 
          left_join(roster_df |> 
                      mutate(espn_id = as.numeric(espn_id)) |> 
                      select(espn_id, player_id = gsis_id),
                    by = 'espn_id') |> 
          mutate(roster_status = 'on_roster') |> 
          rbind(
            fantasy_rosters |>
              filter(league == 'Beep Boop' &
                       franchise_name == 'Love Daktually') |>
              select(espn_id = player_id, franchise_name) |>
              left_join(
                roster_df |>
                  mutate(espn_id = as.numeric(espn_id)) |>
                  select(espn_id, player_id = gsis_id),
                by = 'espn_id'
              ) |>
              mutate(roster_status = 'available')
          ) |> 
          select(player_id, roster_status, franchise_name)
      ) |> 
      filter(!is.na(player_id)) |> 
      left_join(sleeper_players() |> 
                  filter(status == 'Active' & 
                           !is.na(gsis_id)) |> 
                  select(gsis_id, injury_status),
                by = c('player_id' = 'gsis_id')),
    by = 'player_id'
  ) |> 
  mutate(total_fantasy_points_exp_ppg = total_fantasy_points_exp / games,
         total_fantasy_points = total_fantasy_points / games) |> 
  filter(!is.na(player_id)) |>
  select(# week,
         roster_status,
         franchise_name,
         full_name,
         position,
         injury_status,
         games,
         total_fantasy_points_exp_ppg,
         total_fantasy_points,
         total_fantasy_points_exp,
         total_fantasy_points,
         total_fantasy_points_diff) |> 
  arrange(-total_fantasy_points_exp) |> 
  filter(position %in% c('WR', 'TE', 'RB')) |> 
  ungroup() |> 
  mutate(rk = rank(-total_fantasy_points_exp)) |> 
  filter(roster_status != 'on_roster')
