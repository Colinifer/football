library(tidyverse)
library(nflfastR)

# choose seasons for which the plot shall be generated
# CPOE starts in 2006
season <- 2020

# load pbp for the choosen seasosn from nflfastR data repo
# can be multiple seasons as well
# lapply(2009:2020, function(season){
  pbp_df <-
    purrr::map_df(season, function(x) {
      readRDS(url(glue::glue("https://github.com/guga31bb/nflfastR-data/blob/master/data/play_by_play_{x}.rds?raw=true")))
    }) %>% decode_player_ids(fast = TRUE)
  
  # load roster data from nflfastR data repo
  roster_df <-
    readRDS(url("https://github.com/guga31bb/nflfastR-data/blob/master/roster-data/roster.rds?raw=true")) %>% 
    decode_player_ids(fast = TRUE)
  
  # compute cpoe grouped by air_yards
  epa_cpoe <-
    pbp_df %>%
    filter(!is.na(cpoe) & !is.na(epa) & !is.na(passer_player_id)) %>%
    group_by(game_id, passer_player_id) %>%
    summarise(cpoe = mean(cpoe), epa = mean(epa))
  
  # summarise cpoe using player ID (note that player ids are 'NA' for 'no_play' plays. 
  # Since we would filter those plays anyways we can use the id here)
  # The correct name is being joined using the roster data
  # first arranged by number of plays to filter the 30 QBs with most pass attempts
  # The filter is set to 30 because we want to have 6 columns and 5 rows in the facet
  summary_df <-
    pbp_df %>%
    filter(!is.na(cpoe) & !is.na(epa) & !is.na(passer_player_id)
    ) %>%
    group_by(passer_player_id) %>%
    summarise(plays = n(),
              total_cpoe = mean(cpoe),
              total_epa = mean(epa)
    ) %>%
    arrange(plays %>% desc()
    ) %>%
    left_join(pbp_df %>% 
                filter(!is.na(passer_player_id)
                ) %>% 
                select(passer_player_id, 
                       team = posteam
                ) %>% 
                unique(),
              by = c('passer_player_id')
    ) %>% 
    head(32) %>%
    arrange(total_cpoe %>% 
              desc()
    ) %>% 
    inner_join(
      as_tibble(roster_df) %>% 
        select(team = team.abbr, 
               first_name = teamPlayers.firstName, 
               last_name = teamPlayers.lastName, 
               gsis = teamPlayers.gsisId, 
               headshot_url = teamPlayers.headshot_url
        ) %>% 
        mutate(full_name = glue('{first_name} {last_name}')) %>% 
        select(-first_name, -last_name) %>% unique(),
      by = c('passer_player_id' = 'gsis', 'team')
    ) %>%
    mutate(# some headshot urls are broken. They are checked here and set to a default 
      headshot_url = dplyr::if_else(
        RCurl::url.exists(as.character(headshot_url)),
        as.character(headshot_url),
        'http://static.nfl.com/static/content/public/image/fantasy/transparent/200x200/default.png',
      )
    ) %>%
    left_join(epa_cpoe, by = 'passer_player_id') %>%
    left_join(
      teams_colors_logos %>% select(team_abbr, team_color, team_logo_espn),
      by = c('team' = 'team_abbr')
    )
  
  # create data frame used to add the logos
  # arranged by name because name is used for the facet
  colors_raw <-
    summary_df %>%
    group_by(passer_player_id) %>%
    summarise(team = first(team), name = first(full_name)) %>%
    left_join(
      teams_colors_logos %>% select(team_abbr, team_color),
      by = c("team" = "team_abbr")
    ) %>%
    arrange(name)
  
  # the below used smooth algorithm uses the parameter n as the number
  # of points at which to evaluate the smoother. When using color as aesthetics
  # we need exactly the same number of colors (-> n times the same color per player)
  n_eval <- 80
  colors <-
    as.data.frame(lapply(colors_raw, rep, n_eval)) %>%
    arrange(name)
  
  # mean data frame for the smoothed line of the whole league
  mean <-
    summary_df %>%
    summarise(league_cpoe = mean(total_cpoe), 
              league_epa = mean(total_epa)
              )
  
  summary_images_df <- 
    summary_df %>% 
    select(full_name, passer_player_id, headshot_url, team_logo_espn) %>% 
    unique()
  
  # create the plot. Set asp to make sure the images appear in the correct aspect ratio
  asp <- 16/16
  p <-
    summary_df %>%
    ggplot(aes(x = cpoe, y = epa)) +
    geom_point(aes(color = full_name), size = .2, alpha = 0.3) +
    geom_hline(yintercept = mean$league_epa, color = "red", linetype = "dashed") +
    geom_vline(xintercept =  mean$league_cpoe, color = "red", linetype = "dashed") +
    geom_point(alpha = 0.6, aes(color = team), size = 2) +
    scale_color_manual(values =  NFL_pri_dark,
                       name = "Team") +
    # scale_fill_manual(values =  NFL_pri,
    #                   name = "Team") +
    ggimage::geom_image(data = summary_images_df, aes(x = 48, y = -1, image = team_logo_espn),
                        size = .2, by = "width", asp = asp
    ) +
    ggimage::geom_image(data = summary_images_df, aes(x = -48, y = -1, image = headshot_url),
                        size = .2, by = "width", asp = asp
    ) +
    coord_cartesian(xlim = c(-50, 50), ylim = c(-1, 1)) + # 'zoom in'
    labs(
      x = "Completion Percentage Over Expectation\n(CPOE in percentage points)",
      y = "EPA per Pass Attempt",
      title = glue::glue("Passing Efficiency by Game {season}"),
      subtitle = ""
    ) +
    facet_wrap(vars(full_name), ncol = 8, scales = "fixed") +
    theme_cw +
    theme(
      axis.title = element_text(size = 8),
      axis.text = element_text(size = 6),
      axis.title.y = element_text(angle = 90),
      # panel.background = element_rect(fill = color_cw[3]),
      plot.title = element_text(size = 12, face = "bold"),
      plot.subtitle = element_text(size = 6),
      # panel.margin.y = ,
      legend.position = "none",
      legend.title = element_blank(),
      legend.text = element_blank(),
      strip.text = element_text(size = 4, hjust = 0.5, face = "bold")
    )
  
  # save the plot
  brand_plot(p, asp = 16/10, save_name = glue('plots/desktop/qb_cpoe_vs_dot_{season}.png'), data_home = 'Data: @nflfastR', fade_borders = '')
# })
