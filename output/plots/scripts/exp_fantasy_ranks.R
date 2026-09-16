
ep_df <- ep_load()

plot_exp_fantasy_pts <- function(x.year, x.team, ep_df = ep_df, pbp = pbp_df) {
  
  ep <- ep_df |> 
    filter(!is.na(full_name) &
             posteam == x.team) |> 
    inner_join(pbp |> 
                 filter(!is.na(posteam)) |> 
                 select(posteam,
                        game_id,
                        defteam) |> 
                 unique(),
               by = c('posteam', 'game_id')) |> 
    arrange(desc(total_fantasy_points_exp)) |> 
    select(season, posteam, defteam, full_name, position, week,
           total_fantasy_points_exp, total_fantasy_points,
           rec_attempt, rec_air_yards, rec_yards_gained_exp,
           rush_attempt, rush_yards_gained_exp,
           contains('_exp'))

  labs <- ep |>  
    select(full_name, posteam) |> 
    unique()
  
  weekly_team_exp <- ep |> 
    select(season,
           player = full_name,
           team = posteam,
           oppteam = defteam,
           week,
           position,
           total_fantasy_points_exp) |> 
    arrange(week, desc(total_fantasy_points_exp)) |> 
    group_by(team, week) |> 
    mutate(team_rank = row_number()) |> 
    filter(team_rank <= 7) |> 
    arrange(week, team, desc(total_fantasy_points_exp)) |> 
    # filter(team == 'ARI') |> 
    ungroup() |>
    tidyr::complete(
      season,
      team,
      nesting(week,
              oppteam),
      nesting(
        player,
        position)
    )
  
  # total_team_exp <- ep |> 
  #   select(team = posteam) |> 
  #   group_by(team) |> 
  #   count(name = 'team_total_offense_snaps') |> 
  #   left_join(
  #     pbp |> 
  #       select(team = defteam) |> 
  #       group_by(team) |> 
  #       count(name = 'team_total_defense_snaps'),
  #     by = c('team')
  #   )
  
  # x.year <- 2023
  # x.team <- 'ARI'
  
  p <- weekly_team_exp |> 
    filter(team == x.team) |> 
    ggplot(aes(x = week, 
               y = total_fantasy_points_exp, 
               color = player, 
               NULL
    )) + 
    geom_path(linewidth=1, 
              #aes(color = team, alpha = 1/snap_rank),
              NULL
    ) + 
    geom_point(size=1,
               NULL
    ) + 
    # nflplotR::scale_color_nfl(type = "secondary") + 
    facet_wrap(vars(position), ncol = 1) +
    scale_x_continuous(breaks = weekly_team_exp |> 
                         filter(team == x.team) |> 
                         select(week, oppteam) |> 
                         unique() |> 
                         arrange(week) |> 
                         # tidyr::complete(week = seq(max(week)), 
                         #                 fill = list(oppteam = 'BYE')) |> 
                         pull(week),
                       labels = weekly_team_exp |> 
                         filter(team == x.team) |> 
                         select(week, oppteam) |> 
                         unique() |> 
                         arrange(week) |> 
                         # tidyr::complete(week = seq(max(week)), 
                         #                 fill = list(oppteam = 'BYE')) |> 
                         pull(oppteam, name = week)
    ) +
    # scale_color_binned() + 
    scale_color_discrete(name = 'Player', labels = labs) +
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
    labs(title = glue('Expected Fantasy Points by Week'),
         subtitle = glue('Teams: {toString(x.team)} 
                         Seasons: {x.year}'),
         tag = x.team,
         x = 'Week',
         y = 'Expected Fantasy Points',
         color = glue('Player')) + 
    theme(
      axis.text.x = nflplotR::element_nfl_logo(size = 0.5),
      panel.grid.major.x = element_blank() ,
      plot.tag = nflplotR::element_nfl_logo(size = 1.5, hjust = 1, vjust = 1),
      plot.tag.position = c(1, 1),
      legend.position = 'right',
      NULL
    )
  
  brand_plot(
    p,
    asp = 16 / 10,
    save_name = glue('plots/desktop/team_fant_points/{x.year}/{x.team}.png'),
    data_home = 'Data: @nflfastR',
    fade_borders = ''
  )
}

x.teams <- pbp_df |> 
  filter(week == max(week) 
         & !is.na(posteam)
  ) |> 
  arrange(posteam) |> 
  pull(posteam) |> 
  unique()

map(x.teams,
     \(x) plot_exp_fantasy_pts(x.year = 2023,
                              x.team = x,
                              ep_df = ep_df,
                              pbp = pbp_df)
)
