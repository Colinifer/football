library(ggcorrplot)

# https://underdognetwork.com/football/correlation-at-ceiling-outcomes-between-teammates-and-their-opponents
# https://github.com/SportsInfoSolutions/AnalyticsChallenge2021/blob/e99f6f4902ab259ab1966035840817a601e0d383/Submissions/bradcongelio%40gmail.com/bradcongelio-SIS-challenge.R
# https://github.com/pablolopez2733/Sports_Viz/blob/57b06825661ec8fcdb4d342c78859c7f8a286fb1/NFL/R/corr_run_pass.R
# https://www.fantasylabs.com/articles/undervalued-nfl-dfs-correlations/

con <- fx.db_con(x.host = 'localhost')
schedule_df <- tbl(con, 'nflfastR_schedule') |> 
  filter(season >= 2020) |> 
  collect()
dbDisconnect(con)

matchup_df <- schedule_df |> 
  mutate(posteam = home_team,
         oppteam = away_team) |>
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
    home_result = result,
    stadium,
    location,
    roof,
    surface,
    old_game_id
  ) |> 
  rbind(
    schedule_df |> 
      mutate(posteam = away_team,
             oppteam = home_team) |>
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
        home_result = result,
        stadium,
        location,
        roof,
        surface,
        old_game_id
      )
  ) |> 
  arrange(old_game_id)

player_stats_weekly |> 
  left_join(
    roster_df |> 
      select(season, gsis_id, position),
    by = c('player_id' = 'gsis_id', 'season')
  ) |> 
  select(player_id, player_name, position, 
         recent_team, game_id) |> 
  left_join(
    pbp_df |> 
      select(game_id, 
             recent_team = posteam, 
             opp_team = defteam) |> 
      na.omit() |> 
      unique()
  )

scoring_history <- ffscrapr::ff_scoringhistory(conn = ff_conn_beep_boop, 
                                               season = c(2020:2022)
                                               )

corr_ready_df <- scoring_history |> 
  filter(pos %in% c('QB', 'WR', 'TE', 'RB')) |> 
  group_by(team, week, pos) |> 
  arrange(season, week, team, pos, -points) |> 
  mutate(position = glue('{pos}{row_number()}'),
         n = row_number()) |> 
  filter((pos == 'QB' & n <= 1) | 
           (pos == 'WR' & n <= 3) | 
           (pos == 'RB' & n <= 2) |
           (pos == 'TE' & n <= 1)) |> 
  ungroup() |> 
  select(season, week, team, position, points) |> 
  rbind(
    scoring_history |> 
      filter(pos %in% c('QB', 'WR', 'TE', 'RB')) |> 
      left_join(
        matchup_df |> 
          select(season, week, team = posteam, oppteam),
        by = c('season', 'week', 'team')
      ) |> 
      group_by(oppteam, week, pos) |> 
      arrange(season, week, oppteam, pos, -points) |> 
      mutate(pos = glue('Opp_{pos}'),
             position = glue('{pos}{row_number()}'),
             n = row_number()) |> 
      filter((pos == 'Opp_QB' & n <= 1) | 
               (pos == 'Opp_WR' & n <= 3) | 
               (pos == 'Opp_RB' & n <= 2) |
               (pos == 'Opp_TE' & n <= 1)) |> 
      ungroup() |> 
      select(season, week, team = oppteam, position, points)
  ) |> 
  pivot_wider(names_from = position, values_from = points) |> 
  # filter(team == 'PHI') |> 
  select(Opp_WR3, Opp_WR2, Opp_WR1, Opp_TE1, Opp_RB2, Opp_RB1, Opp_QB1,
         WR3, WR2, WR1, TE1, RB2, RB1, QB1) %>%
  replace(is.na(.), 0)

corr_df <- corr_ready_df |> 
  cor() |> 
  round(2)

corr_pmat_df <- corr_ready_df |> 
  cor_pmat()

p <- corr_df |> 
  ggcorrplot(hc.order = FALSE, 
             lab = TRUE, 
             # p.mat = corr_pmat_df, 
             insig = "blank",
             ggtheme = theme_cw_dark,
             tl.cex = 14,
             ) + 
  scale_fill_gradient2(mid = color_cw['white'], 
                       high = color_cw['green']) + 
  scale_x_discrete(limits=rev) + 
  theme_cw_dark + 
  labs(
    title = 'Positional Correlations in Fantasy Football',
    subtitle = '2020 through 2022',
    y = ''
  ) + 
  theme(
    plot.title = element_text(size = 18),
    plot.subtitle = element_text(size = 16)
  )

brand_plot(p, asp = 1/1, base_size = 8, save_name = 'plots/desktop/fantasy_correlations.png', data_home = 'Data: @nflfastR', fade_borders = '')

# Melt to reshape data

# library(reshape2)
# melted_corr_df <- melt(corr_df)
# 
# ggplot(data = melted_corr_df, aes(x=Var1, y=Var2, fill=value)) + 
#   geom_tile() + 
#   scale_fill_viridis(option = 'A', direction = 1)


def_player_stats <- pbp_df |> 
  calculate_player_stats_def(weekly = TRUE)

def_player_stats |> 
  group_by(team) |> 
  summarise(across(def_tackles:def_penalty_yards, sum, na.rm = TRUE)) |> 
  select(team, def_interceptions, def_sacks) |> 
  arrange(-def_interceptions)
