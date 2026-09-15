team_snap_ui <- pageWithSidebar(
  headerPanel('Team Snaps'),
  sidebarPanel(
    selectInput('rx.season', 'Season', vars),
    selectInput('rx.team', 'Team', vars),
    selectInput('pos_group', 'Position', vars, selected = vars[[2]])
  ),
  mainPanel(
    plotOutput('plot1')
  )
)


participation_df <- load_participation(seasons = 2023) |> 
  mutate(season = 2023)

pbp <- pbp_df

weekly_team_snaps <- pbp |> 
  filter(play_type %in% c('pass', 'run')) |> 
  select(team = posteam,
         oppteam = defteam,
         week) |> 
  group_by(team, oppteam, week) |> 
  count(name = 'team_weekly_offense_snaps') |> 
  left_join(
    pbp |> 
      filter(play_type %in% c('pass', 'run')) |> 
      select(team = defteam, week) |> 
      group_by(team, week) |> 
      count(name = 'team_weekly_defense_snaps'),
    by = c('team', 'week')
  )

total_team_snaps <- pbp |> 
  filter(play_type %in% c('pass', 'run')) |> 
  select(team = posteam) |> 
  group_by(team) |> 
  count(name = 'team_total_offense_snaps') |> 
  left_join(
    pbp |> 
      filter(play_type %in% c('pass', 'run')) |> 
      select(team = defteam) |> 
      group_by(team) |> 
      count(name = 'team_total_defense_snaps'),
    by = c('team')
  )

offense_snaps <- participation_df |>
  select(season,
         game_id = nflverse_game_id,
         play_id,
         offense_players) |>
  separate(
    col = offense_players,
    sep = ';',
    into = paste('offense_on', as.character(c(1:11)), sep = '_')
  ) |> 
  pivot_longer(offense_on_1:offense_on_11,
               names_to = "players_on",
               values_to = "player") |> 
  filter(!is.na(player) & player != '') |> 
  suppressWarnings() |> 
  left_join(pbp |> 
              select(season,
                     season_type,
                     game_id,
                     week,
                     play_id,
                     posteam,
                     defteam,
                     play_type), 
            by = c('season',
                   'game_id', 
                   'play_id')) |> 
  filter(play_type %in% c('pass', 'run')) |> 
  rename(team = posteam,
         oppteam = defteam) |> 
  group_by(season, season_type, game_id, 
           week, team, oppteam, player, #play_type,
           NULL
  ) |> 
  count(name = 'snaps') |> 
  mutate(snap_type = 'offense') |> 
  left_join(roster_df |> 
              select(season, gsis_id, position, depth_chart_position, full_name), 
            by = c('season', 'player'='gsis_id'))

defense_snaps <- participation_df |> 
  select(season,
         game_id = nflverse_game_id,
         play_id,
         defense_players
  ) |> 
  separate(col = defense_players, 
           sep = ';',
           into = paste('defense_on', as.character(c(1:11)), sep = '_') 
  ) |> 
  pivot_longer(defense_on_1:defense_on_11,
               names_to = "players_on",
               values_to = "player") |> 
  filter(!is.na(player) & player != '') |> 
  suppressWarnings() |> 
  left_join(pbp |> 
              select(season,
                     season_type,
                     game_id,
                     week,
                     play_id,
                     posteam,
                     defteam,
                     play_type), 
            by = c('season',
                   'game_id', 
                   'play_id')) |> 
  filter(play_type %in% c('pass', 'run')) |> 
  rename(team = defteam,
         oppteam = posteam) |> 
  group_by(season, season_type, game_id, 
           week, team, oppteam, player, #play_type,
           NULL
           ) |> 
  count(name = 'snaps') |> 
  mutate(snap_type = 'defense') |> 
  left_join(roster_df |> 
              select(season, gsis_id, position, depth_chart_position, full_name), 
            by = c('season', 'player'='gsis_id'))

player_legend_titles <- offense_snaps |> 
  rbind(defense_snaps) |> 
  group_by(player, full_name, snap_type) |> 
  summarise(total_snaps = sum(snaps, na.rm = T)) |> 
  mutate(player_legend = glue('{full_name} ({total_snaps})')) |> 
  ungroup() |> 
  arrange(desc(total_snaps))

player_snaps <- offense_snaps |> 
  rbind(defense_snaps) |> 
  left_join(
    player_legend_titles,
    by = c('player', 'full_name', 'snap_type')) |> 
  left_join(
    weekly_team_snaps,
    by = c('team', 'oppteam', 'week')
  ) |> 
  left_join(
    total_team_snaps,
    by = c('team')
  ) |> 
  mutate(
    weekly_snap_share = case_when(snap_type == 'offense' ~ snaps / team_weekly_offense_snaps,
                           snap_type == 'defense' ~ snaps / team_weekly_defense_snaps),
    total_snap_share = case_when(snap_type == 'offense' ~ total_snaps / team_total_offense_snaps,
                                 snap_type == 'defense' ~ total_snaps / team_total_defense_snaps)
  ) |> 
  arrange(desc(total_snaps))


pos_oline <- c('C', 'G', 'T')
pos_skill <- c('RB', 'TE', 'WR')
pos_dline <- c('NT', 'DT', 'DE', 'OLB')
pos_lb <- c('MLB', 'ILB')
pos_secondary <- c('CB', 'FS', 'SS')

x.team <- c('DAL')
x.position <- c(pos_oline, pos_skill)

# https://stackoverflow.com/questions/14821064/line-break-when-no-data-in-ggplot2
# https://twitter.com/josephjefe/status/1617928226608934915

# group_by position types
# create row_number
# color by rank 1,2,3
{
  position_groups <- c('C' = 'oline',
                       'RB' = 'skill',
                       'NT' = 'dline',
                       'MLB' = 'lb',
                       'CB' = 'secondary')
  
  player_ranks <- player_snaps |> 
    filter(team %in% x.team & 
             depth_chart_position %in% x.position & 
             weekly_snap_share > .1) |> 
    ungroup() |> 
    select(player, team, total_snaps) |> 
    unique() |> 
    group_by(team) |> 
    mutate(snap_rank = row_number()) |> 
    ungroup()
  
  data <- player_snaps |> 
    filter(team %in% x.team & 
             depth_chart_position %in% x.position# & 
             # total_snap_share > .05
           ) |> 
    select(season, season_type, game_id, team, oppteam, depth_chart_position, player, 
           player_legend, week, weekly_snap_share) |> 
    left_join(
      player_ranks |> 
        select(1,2,4),
      by = c('player', 'team')
    ) |> 
    ungroup() |> 
    filter(snap_rank <= 9)
  
  cols <- data |> 
    select(team, snap_rank, player_legend) |> 
    unique() |> 
    pull(snap_rank)
  
  labs <- data |> 
    select(team, snap_rank, player_legend) |> 
    unique() |> 
    pull(player_legend)
  
  p <- data |> 
    mutate(oppteam = fct_reorder(oppteam, week)) |> 
    # arrange(week) %>% 
    ggplot(aes(x = oppteam, 
               y = weekly_snap_share, 
               color = factor(snap_rank), 
               group = factor(player_legend),
               NULL
    )) + 
    geom_line(linewidth=1, 
              #aes(color = team, alpha = 1/snap_rank),
              NULL
              ) + 
    geom_point(size=1,
               NULL
               ) + 
    # nflplotR::scale_color_nfl(type = "secondary") + 
    facet_wrap(vars(depth_chart_position)) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    # scale_color_binned() + 
    scale_color_discrete(name = 'Player (Snaps)', labels = labs) +
    # scale_color_viridis(name ='Player (Snaps)',
    #                    # values = cols,
    #                    # labels = labs,
    #                    discrete = TRUE,
    #                    option = 'A',
    #                    direction = -1
    #                    ) +
    # scale_color_manual(
    #   values = hue_pal()(cols),
    #   labels = labs
    # ) +
    theme_cw_dark + 
    labs(title = glue('Snap Share by Week'),
         subtitle = glue('Teams: {toString(x.team)} 
                       Positions: {toString(x.position)} 
                       Seasons: {current_season}'),
         tag = x.team,
         x = 'Week',
         y = 'Snap Share',
         color = glue('Player (Snaps)')) + 
    theme(
      strip.text = element_blank(),
      axis.text.x = nflplotR::element_nfl_logo(size = 0.5),
      plot.tag = nflplotR::element_nfl_logo(size = 1.5, hjust = 1, vjust = 1),
      plot.tag.position = c(1, 1),
      legend.position = 'right',
      NULL
    )
  
  brand_plot(
    p,
    asp = 16 / 10,
    save_name = glue('plots/desktop/team_snaps/{x.team}_{position_groups[x.position[1]]}_snaps_{current_season}.png'),
    data_home = 'Data: @nflfastR',
    fade_borders = ''
  )
}

# Notes:
# Filter if position is offense or defense (Grant Calcaterra bug)
# Fix team facet issue
# Filter players w/ >= .1 snap share in any game
