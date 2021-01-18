library(tidyverse)
library(nflfastR)
load(url('https://github.com/guga31bb/metrics/blob/master/dakota_model.rda?raw=true'))

# choose seasons for which the plot shall be generated
# CPOE starts in 2006
current_season <- year

# Load pbp for the chosen season from nflfastR data repo
# can be multiple seasons
# lapply(2007:2019, function(season){
  pbp_df <- pbp_ds %>% 
    filter(season >= current_season & 
             game_id != '2020_12_NO_DEN') %>% # THis game is pointless
    collect() %>% 
    decode_player_ids(fast = TRUE) %>% 
    mutate(defteam = ifelse(defteam == "LA", "LAR", defteam),
           posteam = ifelse(posteam == "LA", "LAR", posteam),
           posteam = ifelse(season < 2016 & posteam == 'LAR', 'STL', posteam),
           defteam = ifelse(season < 2016 & defteam == 'LAR', 'STL', defteam),
           posteam = ifelse(season < 2017 & posteam == 'LAC', 'SD', posteam),
           defteam = ifelse(season < 2017 & defteam == 'LAC', 'SD', defteam),
           posteam = ifelse(season < 2020 & posteam == 'LV', 'OAK', posteam),
           defteam = ifelse(season < 2020 & defteam == 'LV', 'OAK', defteam))
  
  # load roster data from nflfastR data repo
  roster_df <-
    readRDS(url("https://github.com/guga31bb/nflfastR-data/blob/master/roster-data/roster.rds?raw=true")) %>% 
    decode_player_ids(fast = TRUE) %>% 
    select(team = team.abbr,
           first_name = teamPlayers.firstName,
           last_name = teamPlayers.lastName,
           gsis = teamPlayers.gsisId,
           headshot_url = teamPlayers.headshot_url
    ) %>%
    mutate(full_name = glue('{first_name} {last_name}')) %>%
    select(-first_name, -last_name) %>% unique() %>% 
    mutate(# some headshot urls are broken. They are checked here and set to a default 
      headshot_url = dplyr::if_else(condition = full_name %in% c('Brett Basanez', 
                                                                 'JaMarcus Russell', 
                                                                 'J.P. Losman',
                                                                 'Marc Bulger',
                                                                 'Donovan McNabb',
                                                                 'Steve McNair',
                                                                 'Brad Johnson',
                                                                 'Andrew Walter',
                                                                 'Lem Burnham',
                                                                 'Cleo Lemon'), true = 'http://static.nfl.com/static/content/public/image/fantasy/transparent/200x200/default.png', false = as.character(headshot_url))
    )
  
  # compute cpoe grouped by air_yards
  epa_cpoe <-
    pbp_df %>%
    filter(!is.na(cpoe) & !is.na(epa) & !is.na(passer_player_id)) %>%
    group_by(game_id, passer_player_id) %>%
    summarise(cpoe = mean(cpoe), epa = mean(epa)) %>% 
    left_join(pbp_df %>%
                filter(!is.na(cpoe) &
                         !is.na(epa) & !is.na(passer_player_id)) %>%
                group_by(passer_player_id) %>%
                summarise(cpoe = mean(cpoe), epa_per_play = mean(epa)) %>%
                mutate(season_dakota = mgcv::predict.gam(dakota_model, .)) %>%
                select(-cpoe, -epa_per_play),
              by = c('passer_player_id')
    )
  
  post_season_qbs <- 
    pbp_df %>%
    filter(!is.na(cpoe) & 
             !is.na(epa) & 
             !is.na(passer_player_id) &
             week > 17
    ) %>%
    group_by(passer_player_id) %>%
    summarise(passer = first(passer),
              posteam = posteam %>% first(),
              pa = n(),
              total_cpoe = mean(cpoe),
              total_epa = mean(epa)
    ) %>%
    arrange(pa %>% desc()
    ) %>% 
    unique() %>% 
    # group_by(posteam) %>% 
    # slice(1) %>% 
    # head(32) %>% 
    pull(passer_player_id)
  
  # summarise cpoe using player ID (note that player ids are 'NA' for 'no_play' plays. 
  # Since we would filter those plays anyways we can use the id here)
  # The correct name is being joined using the roster data
  # first arranged by number of plays to filter the 30 QBs with most pass attempts
  # The filter is set to 30 because we want to have 6 columns and 5 rows in the facet
  summary_df <-
    pbp_df %>%
    filter(!is.na(cpoe) & 
             !is.na(epa) & 
             !is.na(passer_player_id) &
             passer_player_id %in% post_season_qbs
    ) %>% 
    group_by(game_id, week, passer_player_id) %>%
    summarise(pa = n(),
              total_cpoe = mean(cpoe),
              total_epa = mean(epa)
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
    group_by(passer_player_id) %>% 
    mutate(season_pa = sum(pa, na.rm = T)) %>% 
    ungroup %>% 
    arrange(-season_pa) %>% 
    filter(!is.na(team)) %>% 
    filter(passer_player_id %in% post_season_qbs) %>%
    left_join(
      sleeper_players_df %>%
        select(position, full_name, sportradar_id, gsis_id, espn_id, headshot_url),
      by = c('passer_player_id' = 'gsis_id')
    ) %>%
    # left_join(
    #   as_tibble(roster_df),
    #   by = c('passer_player_id' = 'gsis' , 'team')
    # ) %>%
    mutate(# some headshot urls are broken. They are checked here and set to a default 
      headshot_url = dplyr::if_else(RCurl::url.exists(as.character(headshot_url)), as.character(headshot_url), 'http://static.nfl.com/static/content/public/image/fantasy/transparent/200x200/default.png')
    ) %>% 
    left_join(epa_cpoe, by = c('passer_player_id', 'game_id')) %>% 
    arrange(season_dakota %>% desc()) %>% 
    mutate(lab_dakota = glue('DAKOTA: {season_dakota %>% round(3)}')) %>% 
    left_join(
      teams_colors_logos %>% select(team_abbr, team_color, team_logo_espn),
      by = c('team' = 'team_abbr')
    ) %>% mutate(season = game_id %>% substr(1, 4)) %>% 
    mutate_at(vars(season_dakota), funs(factor(., levels=unique(.))))
  
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
    select(full_name, passer_player_id, season_dakota, lab_dakota, headshot_url, team_logo_espn, season_pa) %>% 
    unique() %>% 
    arrange(season_dakota %>% desc())
  
  panel_label <- summary_images_df$full_name
  names(panel_label) <- summary_images_df$season_dakota
  
  grob_img_adj <- function(img_url, alpha = 1, whitewash = 0) {
    return(lapply(img_url, function(x) {
      if (is.na(x)) {
        return(NULL)
      } else {
        img <- magick::image_read(x)[[1]]
        img[1, , ] <- as.raw(255 - (255 - as.integer(img[1, , ])) * (1 - whitewash))
        img[2, , ] <- as.raw(255 - (255 - as.integer(img[2, , ])) * (1 - whitewash))
        img[3, , ] <- as.raw(255 - (255 - as.integer(img[3, , ])) * (1 - whitewash))
        img[4, , ] <- as.raw(as.integer(img[4, , ]) * alpha)
        return(grid::rasterGrob(image = magick::image_read(img)))
      }
    }))
  }
  
  # create the plot. Set asp to make sure the images appear in the correct aspect ratio
  asp <- 16/9.886
  p <-
    summary_df %>%
    ggplot(aes(x = cpoe,
               y = epa)) +
    geom_hline(
      yintercept = mean$league_epa,
      color = "red",
      linetype = "dashed"
    ) +
    geom_vline(
      xintercept =  mean$league_cpoe,
      color = "red",
      linetype = "dashed"
    ) +
    geom_point(aes(color = team),
               size = 1.2,
               shape = 16,
               alpha = 0.75) +
    scale_color_manual(values =  NFL_pri_dark,
                       name = "Team") +
    # geom_point(aes(fill = team), shape = 21, size = 1 , alpha = 0.1) +
    # geom_point(alpha = 0.2, aes(color = team), size = .5) +
    # scale_color_manual(values =  NFL_pri_dark,
    #                    name = "Team") +
    geom_point(
      data = summary_df %>%
        group_by(passer_player_id) %>%
        filter(row_number() == n()),
      aes(fill = team),
      color = color_cw[5],
      shape = 21,
      size = 1.2
    ) +
    scale_fill_manual(values =  NFL_pri_dark,
                      name = "Team") +
    geom_shadowtext(
      data = summary_images_df,
      aes(label = lab_dakota,
          x = 37.5,
          y = -1.23),
      color = color_cw[5],
      bg.color = color_cw[2],
      bg.r = .3,
      hjust = 1,
      family = "Montserrat",
      size = 1.4
    ) +
    geom_grob(data = summary_images_df,
              aes(
                x = 27,
                y = -.8,
                label = grob_img_adj(team_logo_espn, alpha = 1),
                vp.height = 0.25
              )) +
    # ggimage::geom_image(data = summary_images_df, aes(x = 26.5, y = -.8, image = team_logo_espn),
    #                     size = .25, by = "width", asp = asp
    # ) +
    # geom_grob(data = summary_images_df,
    #           aes(
    #             x = -26,
    #             y = -.9,
    #             label = grob_img_adj(headshot_url),
    #             vp.height = 0.45
    #           )) +
    ggimage::geom_image(
      data = summary_images_df,
      aes(x = -26, y = -.9, image = headshot_url),
      size = .45,
      by = "width",
      asp = 1/1
    ) +
    coord_cartesian(xlim = c(-35, 35), ylim = c(-1.25, 1.25)) + # 'zoom in'
    labs(
      x = 'Completion Percentage Over Expectation (CPOE in percentage points)',
      y = 'EPA per Pass Attempt',
      title = glue::glue('Passing Efficiency by Game {current_season}'),
      subtitle = "QB's with the most pass attempts on each team, ordered by @benbbaldwin's DAKOTA rating\nWhite Dot = Most Recent Game. Red Line = League Average."
    )
  
  p_desktop <- p +
    facet_wrap(~season_dakota, labeller = labeller(season_dakota = panel_label), ncol = 6) +
    # coord_fixed(ratio=35) +
    theme_cw +
    theme(
      axis.title = element_text(size = 8),
      axis.text = element_text(size = 5),
      axis.ticks = element_line(color = color_cw[5], size = 0.3),
      axis.ticks.length = unit(2, 'pt'),
      axis.title.y = element_text(angle = 90),
      plot.title = element_text(size = 12, face = "bold"),
      plot.subtitle = element_text(size = 6),
      # plot.margin = margin(1, 1, 1, 1, unit = "cm"),
      panel.background = element_rect(fill = color_cw[2]),
      panel.spacing.x = unit(1.25, "lines"),
      panel.spacing.y = unit(1, "lines"),
      legend.position = "none",
      legend.title = element_blank(),
      legend.text = element_blank(),
      strip.text = element_text(size = 4, hjust = 0.5, face = "bold")
    )
  
  p_mobile <- p +
    facet_wrap(~season_dakota, labeller = labeller(season_dakota = panel_label), ncol = 3) +
    theme_cw +
    theme(
      axis.title = element_text(size = 8),
      axis.text = element_text(size = 5),
      axis.ticks = element_line(color = color_cw[5], size = 0.3),
      axis.ticks.length = unit(2, 'pt'),
      axis.title.y = element_text(angle = 90),
      plot.title = element_text(size = 12, face = "bold"),
      plot.subtitle = element_text(size = 6),
      plot.margin = margin(.25, 1, .25, .25, unit = "cm"),
      panel.background = element_rect(fill = color_cw[2]),
      panel.spacing.x = unit(1.25, "lines"),
      panel.spacing.y = unit(.7, "lines"),
      legend.position = "none",
      legend.title = element_blank(),
      legend.text = element_blank(),
      strip.text = element_text(size = 4, hjust = 0.5, face = "bold")
    )
  
  # Desktop
  # save the plot
  brand_plot(p_desktop, asp = 16/10, save_name = glue('plots/desktop/qb_epa_vs_cpoe/post_qb_epa_vs_cpoe_{current_season}.png'), data_home = 'Data: @nflfastR', fade_borders = '')
  
  # Mobile
  # save the plot
  brand_plot(p_mobile, asp = 9/16, save_name = glue('plots/mobile/qb_epa_vs_cpoe/post_qb_epa_vs_cpoe_{current_season}.png'), data_home = 'Data: @nflfastR', fade_borders = '')
  
  rm(current_season, epa_cpoe, post_season_qbs, summary_df, colors_raw, n_eval, colors, mean, summary_images_df, panel_label, grob_img_adj, asp, p, p_desktop, p_mobile)
# })
