source('https://github.com/ajreinhard/data-viz/raw/master/ggplot/plot_SB.R')

library(nflfastR)


# Get data ----------------------------------------------------------------

# Get PBP and Roster data
pbp_df <- readRDS(url('https://github.com/guga31bb/nflfastR-data/blob/master/data/play_by_play_2020.rds?raw=true'))
roster_df <- readRDS(url('https://github.com/mrcaseb/nflfastR-roster/blob/master/data/nflfastR-roster.rds?raw=true')) %>% 
  left_join(readRDS("../GitHub/Cloned/ajrein-NFL/nflfastR ID mapping/gsis_map.rds"), by = c('teamPlayers.gsisId' = 'gsis')) %>% 
  mutate(ID = ifelse(is.na(ID), teamPlayers.gsisId, ID))

url <- "https://fantasyfootballcalculator.com/api/v1/adp/"
type = "standard"
team_count = 10
year = 2020
adp_df <- GET(glue(url, type, "?teams=", team_count, "&year=", year)) %>% content(as = "parsed", type = "application/json")
adp_df$players %>% 
  
# Test
adp_df$players %>% 
unlist(recursive = FALSE) %>%
as.data.frame %>%
select(starts_with("player_id"))

map_dfc(adp_df$players, `[`, 1)
#


# Clean data --------------------------------------------------------------

drop.cols.xyac <- c(
  "xyac_epa", "xyac_mean_yardage", "xyac_median_yardage", "xyac_success", "xyac_fd", ".groups"
)

prepare_xyac_data <- function(pbp) {
  
  # valid pass play: at least -15 air yards, less than 70 air yards, has intended receiver, has pass location
  passes <- pbp %>%
    make_model_mutations() %>%
    dplyr::mutate(
      receiver_player_name =
        stringr::str_extract(.data$desc, "(?<=((to)|(for))\\s[:digit:]{0,2}\\-{0,1})[A-Z][A-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?"),
      pass_middle = dplyr::if_else(.data$pass_location == "middle", 1, 0),
      air_is_zero = dplyr::if_else(.data$air_yards == 0, 1, 0),
      distance_to_sticks = .data$air_yards - .data$ydstogo,
      distance_to_goal = .data$yardline_100 - .data$air_yards,
      valid_pass = dplyr::if_else(
        (.data$complete_pass == 1 | .data$incomplete_pass == 1 | .data$interception == 1) &
          !is.na(.data$air_yards) & .data$air_yards >= -15 & .data$air_yards < 70 &
          !is.na(.data$receiver_player_name) & !is.na(.data$pass_location),
        1, 0
      )
    )
  return(passes)
}

### another helper function for getting the data ready
xyac_model_select <- function(pbp) {
  pbp %>%
    dplyr::select(
      "air_yards", "yardline_100", "ydstogo", "distance_to_goal",
      "down1", "down2", "down3", "down4", "air_is_zero", "pass_middle",
      "era2", "era3", "era4", "qb_hit", "home",
      "outdoors", "retractable", "dome", "distance_to_sticks"
    )
}


add_xyac2 <- function(pbp) {
  
  # testing only
  # pbp <- g
  
  pbp <- pbp %>% dplyr::select(-tidyselect::any_of(drop.cols.xyac))
  
  # for joining at the end
  pbp <- pbp %>%
    dplyr::mutate(index = 1:dplyr::n())
  
  # prepare_xyac_data helper function shown below
  passes <- prepare_xyac_data(pbp) %>%
    filter(.data$valid_pass == 1, .data$distance_to_goal != 0)
  
  if (!nrow(passes) == 0) {
    # initialize xyac_model to avoid R CMD check note
    xyac_model <- NULL
    suppressWarnings(
      # load the model from github because it is too big for the package
      try(
        load(url("https://github.com/guga31bb/nflfastR-data/blob/master/models/xyac_model.Rdata?raw=true")),
        silent = TRUE
      )
    )
    
    if (!is.null(xyac_model)) {
      xyac_vars <-
        stats::predict(
          xyac_model,
          as.matrix(passes %>% xyac_model_select())
        ) %>%
        tibble::as_tibble() %>%
        dplyr::rename(prob = "value") %>%
        dplyr::bind_cols(
          purrr::map_dfr(seq_along(passes$index), function(x) {
            tibble::tibble(
              "yac" = -5:70,
              "index" = passes$index[[x]],
              "distance_to_goal" = passes$distance_to_goal[[x]],
              "season" = passes$season[[x]],
              "week" = passes$week[[x]],
              "home_team" = passes$home_team[[x]],
              "posteam" = passes$posteam[[x]],
              "roof" = passes$roof[[x]],
              "half_seconds_remaining" = dplyr::if_else(
                passes$half_seconds_remaining[[x]] <= 6,
                0,
                passes$half_seconds_remaining[[x]] - 6
              ),
              "down" = as.integer(passes$down[[x]]),
              "ydstogo" = as.integer(passes$ydstogo[[x]]),
              "original_ydstogo" = as.integer(passes$ydstogo[[x]]),
              "posteam_timeouts_remaining" = passes$posteam_timeouts_remaining[[x]],
              "defteam_timeouts_remaining" = passes$defteam_timeouts_remaining[[x]],
              "original_spot" = passes$yardline_100[[x]],
              "original_ep" = passes$ep[[x]],
              "air_epa" = passes$air_epa[[x]],
              "air_yards" = passes$air_yards[[x]]
            )
          })
        ) %>%
        dplyr::group_by(.data$index) %>%
        dplyr::mutate(
          max_loss = dplyr::if_else(.data$distance_to_goal < 95, -5, .data$distance_to_goal - 99),
          max_gain = dplyr::if_else(.data$distance_to_goal > 70, 70, .data$distance_to_goal),
          cum_prob = cumsum(.data$prob),
          prob = dplyr::case_when(
            # truncate probs at loss greater than max loss
            .data$yac == .data$max_loss ~ .data$cum_prob,
            # same for gains bigger than possible
            .data$yac == .data$max_gain ~ 1 - dplyr::lag(.data$cum_prob, 1),
            TRUE ~ .data$prob
          ),
          # get end result for each possibility
          yardline_100 = .data$distance_to_goal - .data$yac
        ) %>%
        dplyr::filter(.data$yac >= .data$max_loss, .data$yac <= .data$max_gain) %>%
        dplyr::select(-.data$cum_prob) %>%
        dplyr::mutate(
          posteam_timeouts_pre = .data$posteam_timeouts_remaining,
          defeam_timeouts_pre = .data$defteam_timeouts_remaining,
          gain = .data$original_spot - .data$yardline_100,
          turnover = dplyr::if_else(.data$down == 4 & .data$gain < .data$ydstogo, as.integer(1), as.integer(0)),
          down = dplyr::if_else(.data$gain >= .data$ydstogo, 1, .data$down + 1),
          ydstogo = dplyr::if_else(.data$gain >= .data$ydstogo, 10, .data$ydstogo - .data$gain),
          # possession change if 4th down failed
          down = dplyr::if_else(.data$turnover == 1, as.integer(1), as.integer(.data$down)),
          ydstogo = dplyr::if_else(.data$turnover == 1, as.integer(10), as.integer(.data$ydstogo)),
          # flip yardline_100 and timeouts for turnovers
          yardline_100 = dplyr::if_else(.data$turnover == 1, as.integer(100 - .data$yardline_100), as.integer(.data$yardline_100)),
          posteam_timeouts_remaining = dplyr::if_else(.data$turnover == 1,
                                                      .data$defeam_timeouts_pre,
                                                      .data$posteam_timeouts_pre),
          defteam_timeouts_remaining = dplyr::if_else(.data$turnover == 1,
                                                      .data$posteam_timeouts_pre,
                                                      .data$defeam_timeouts_pre),
          # ydstogo can't be bigger than yardline
          ydstogo = dplyr::if_else(.data$ydstogo >= .data$yardline_100, as.integer(.data$yardline_100), as.integer(.data$ydstogo))
        )
      
      pbp <- pbp %>%
        dplyr::left_join(xyac_vars, by = "index") %>%
        dplyr::select(-.data$index)
      
      message("added xyac variables")
      
    } else {# means xyac_model isn't available
      message("This function needs to download the model data from GitHub. Please check your Internet connection and try again!")
      pbp <- pbp %>% dplyr::select(-.data$index)
    }
  } else {# means no valid pass plays in the pbp
    pbp <- pbp %>%
      dplyr::mutate(
        xyac_epa = NA_real_,
        xyac_mean_yardage = NA_real_,
        xyac_median_yardage = NA_real_,
        xyac_success = NA_real_,
        xyac_fd = NA_real_
      ) %>%
      dplyr::select(-.data$index)
    message("No non-NA values for xyac calculation detected. xyac variables set to NA")
  }
  
  # on old versions of dplyr, a .group column is created, which we don't want
  pbp <- pbp %>% dplyr::select(-tidyselect::any_of(".group"))
  
  return(pbp)
}


make_model_mutations <- function(pbp) {
  
  pbp <- pbp %>%
    dplyr::mutate(
      #for EP, CP, and WP model, xgb needs 0/1 for eras
      era0 = dplyr::if_else(.data$season <= 2001, 1, 0),
      era1 = dplyr::if_else(.data$season > 2001 & .data$season <= 2005, 1, 0),
      era2 = dplyr::if_else(.data$season > 2005 & .data$season <= 2013, 1, 0),
      era3 = dplyr::if_else(.data$season > 2013 & .data$season <= 2017, 1, 0),
      era4 = dplyr::if_else(.data$season > 2017, 1, 0),
      #for fg model, an era factor
      era = dplyr::case_when(
        .data$era0 == 1 ~ 0,
        .data$era1 == 1 ~ 1,
        .data$era2 == 1 ~ 2,
        .data$era3 == 1 | era4 == 1 ~ 3
      ),
      era = as.factor(.data$era),
      down1 = dplyr::if_else(.data$down == 1, 1, 0),
      down2 = dplyr::if_else(.data$down == 2, 1, 0),
      down3 = dplyr::if_else(.data$down == 3, 1, 0),
      down4 = dplyr::if_else(.data$down == 4, 1, 0),
      home = dplyr::if_else(.data$posteam == .data$home_team, 1, 0),
      model_roof = dplyr::if_else(is.na(.data$roof) | .data$roof == 'open' | .data$roof == 'closed', as.character('retractable'), as.character(.data$roof)),
      model_roof = as.factor(.data$model_roof),
      retractable = dplyr::if_else(.data$model_roof == 'retractable', 1, 0),
      dome = dplyr::if_else(.data$model_roof == 'dome', 1, 0),
      outdoors = dplyr::if_else(.data$model_roof == 'outdoors', 1, 0)
    )
  
  return(pbp)
}




fant_pt_avg_df <- pbp_df %>% 
  filter(pass_attempt==1 & season_type=='REG' & two_point_attempt==0) %>% 
  add_xyac2 %>% 
  select(season = season.x, game_id, play_id, receiver_id, yardline_100 = yardline_100.x, air_yards = air_yards.x, actual_yards_gained = yards_gained, complete_pass, cp, yac_prob = prob, gain) %>% 
  mutate(
    gain = ifelse(yardline_100==air_yards, yardline_100, gain),
    yac_prob = ifelse(yardline_100==air_yards, 1, yac_prob),
    PPR_points = 1 + gain/10 + ifelse(gain == yardline_100, 6, 0),
    catch_run_prob = cp * yac_prob,
    exp_PPR_points = PPR_points * catch_run_prob,
    actual_outcome = ifelse(actual_yards_gained==gain & complete_pass==1, 1, 0),
    actual_PPR_points = ifelse(actual_outcome==1, PPR_points, 0)
  ) %>% 
  group_by(game_id, play_id) %>% 
  mutate(target = ifelse(row_number()==1,1,0)) %>% 
  ungroup %>% 
  group_by(season, receiver_id) %>% 
  summarize(
    targets = sum(target, na.rm = T),
    catches = sum(actual_outcome, na.rm = T),
    PPR_pts = sum(actual_PPR_points, na.rm = T),
    exp_PPR_pts = sum(exp_PPR_points, na.rm = T)
  )

#fant_pt_avg_df %>% view

properLims <- function(vec) {
  labs <- labeling::extended(min(vec, na.rm = T), max(vec, na.rm = T), m = 5)
  gap <- diff(labs[1:2])
  plot_max <- ifelse(rev(labs)[1] < max(vec, na.rm = T), rev(labs)[1] + gap, rev(labs)[1])
  plot_min <- ifelse(labs[1] > min(vec, na.rm = T), labs[1] - gap, labs[1])
  return(c(plot_min,plot_max))
} 

grob_img_adj <- function(img_url,
                         alpha = 0,
                         whitewash = 0) {
  return(lapply(img_url, function(x) {
    if (is.na(x)) {
      return(NULL)
    } else{
      img <- image_read(x)[[1]]
      img[1, , ] <-
        as.raw(255 - (255 - as.integer(img[1, , ])) * (1 - whitewash))
      img[2, , ] <-
        as.raw(255 - (255 - as.integer(img[2, , ])) * (1 - whitewash))
      img[3, , ] <-
        as.raw(255 - (255 - as.integer(img[3, , ])) * (1 - whitewash))
      img[4, , ] <- as.raw(as.integer(img[4, , ]) * (1 - alpha))
      return(grid::rasterGrob(image = image_read(img)))
    }
  }))
}

p <- fant_pt_avg_df %>% 
  mutate(
    season = 2019,
    exp_PPR_diff = PPR_pts - exp_PPR_pts,
    exp_PPR_pts_targ = exp_PPR_pts/targets
  ) %>% 
  left_join(roster_df, by = c('receiver_id' = 'ID', 'season' = 'team.season')) %>% 
  mutate(abbr.name = paste0(substr(teamPlayers.firstName, 1, 1), '.', teamPlayers.lastName)) %>% 
  filter(!is.na(receiver_id) & targets >= 75 & teamPlayers.position == 'WR') %>% 
  ggplot(aes(x = PPR_pts, y = exp_PPR_pts, label = abbr.name)) +
  geom_grob(aes(x = PPR_pts, y = exp_PPR_pts, label = grob_img_adj(ESPN_logo_url(team.abbr), alpha = .4), vp.height = 0.06)) +
  # geom_text_repel(aes(label = abbr.name), color = 'darkblue', size = 1.3, family = font_SB, bg.color = 'white', bg.r = 0.2, point.padding = 0, min.segment.length = 10, box.padding = 0.1) +
  geom_text_repel(aes(label = abbr.name), color = 'darkblue', size = 1.3, point.padding = 0, min.segment.length = 10, box.padding = 0.1) +
  scale_x_continuous(limits = properLims) +
  scale_y_continuous(limits = properLims) +
  labs(title = 'Expected & Actual WR Fantasy Points, 2019',
       subtitle = 'min. 75 Targets',
       x = 'Actual PPR Fantasy Points',
       y = 'Expected\nPPR\nFantasy\nPoints') +
  theme_SB +
  theme(plot.margin = margin(c(7.5,10,7.5,7.5), unit = 'pt'))


brand_plot(p, save_name = 'exp PPR WR.png', data_home = 'Data: @nflfastR', fade_borders = 'tr')


#############
library(ggridges)
roster_df <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/roster-data/roster.rds')) %>% 
  left_join(readRDS("../GitHub/NFL-ajreinhard/nflfastR ID mapping/gsis_map.rds"), by = c('teamPlayers.gsisId' = 'gsis')) %>% 
  mutate(ID = ifelse(is.na(ID), teamPlayers.gsisId, ID))

fant_pt_dist_df <- pbp_df %>% 
  filter(pass_attempt==1 & season_type=='REG' & two_point_attempt==0 & !is.na(receiver_id) & sack == 0) %>% 
  add_xyac2 %>% 
  select(season = season.x, game_id, play_id, receiver_id, yardline_100 = yardline_100.x, air_yards = air_yards.x, actual_yards_gained = yards_gained, complete_pass, cp, yac_prob = prob, gain) %>% 
  mutate(
    gain = ifelse(yardline_100==air_yards, yardline_100, gain),
    yac_prob = ifelse(yardline_100==air_yards, 1, yac_prob),
    PPR_points = 1 + gain/10 + ifelse(gain == yardline_100, 6, 0),
    catch_run_prob = cp * yac_prob,
    exp_PPR_points = PPR_points * catch_run_prob,
    actual_outcome = ifelse(actual_yards_gained==gain & complete_pass==1, 1, 0),
    actual_PPR_points = ifelse(actual_outcome==1, PPR_points, 0),
    target = 0,
    game_played = 0
  )

incomplete_df <- fant_pt_dist_df %>% 
  mutate(
    gain = 0,
    PPR_points = 0,
    yac_prob = 0,
    exp_PPR_points = 0,
    complete_pass = 0,
    catch_run_prob = 1 - cp,
    actual_outcome = NA,
    actual_PPR_points = NA,
    target = 1
  ) %>% 
  distinct %>% 
  group_by(game_id, receiver_id) %>% 
  mutate(game_played = ifelse(row_number()==1,1,0)) %>% 
  ungroup

WR_rank_df <- rbind(incomplete_df, fant_pt_dist_df) %>% 
  group_by(season, receiver_id) %>% 
  summarize(
    tot_PPR = sum(actual_PPR_points, na.rm = T),
    tot_targ = sum(target),
    tot_gp = sum(game_played),
    PPR_pg = tot_PPR / tot_gp
  ) %>% 
  left_join(roster_df, by = c('receiver_id' = 'ID', 'season' = 'team.season')) %>% 
  filter(teamPlayers.position == 'WR' & tot_gp >= 8) %>% 
  arrange(-PPR_pg)

use_WR <- WR_rank_df %>% 
  slice(1:30) %>% 
  pull(receiver_id)

sampling_df <- rbind(incomplete_df, fant_pt_dist_df) %>% 
  filter(receiver_id %in% use_WR) %>% 
  select(season, game_id, play_id, receiver_id, catch_run_prob, PPR_points) %>% 
  group_by(game_id, play_id)


ptm <- proc.time()
sim_df <- do.call(rbind, lapply(1:10000, function(x) {
  sampling_df %>% 
    mutate(sim_res = sample(PPR_points, 1, prob = catch_run_prob)) %>% 
    select(season, game_id, play_id, receiver_id, sim_res) %>% 
    distinct %>% 
    group_by(season, receiver_id) %>% 
    summarize(szn_tot = sum(sim_res, na.rm = T), .groups = 'drop') %>% 
    return
}))
proc.time() - ptm



actual_perc <- WR_rank_df %>%
  ungroup %>% 
  slice(1:30) %>%
  select(season, receiver_id, sim_pg = PPR_pg) %>%
  mutate(sim = 0)

sim_perc <- sim_df %>% 
  left_join(WR_rank_df) %>% 
  mutate(
    sim_pg = szn_tot / tot_gp,
    sim = 1
  ) %>% 
  select(season, receiver_id, sim_pg, sim)

percentile_df <- rbind(actual_perc, sim_perc) %>% 
  group_by(season, receiver_id) %>% 
  mutate(perc = percent_rank(sim_pg)) %>% 
  filter(sim == 0) %>% 
  mutate(sim_pg = NULL, sim = NULL)

ptm <- proc.time()
p <- sim_df %>% 
  #slice(1:1000) %>% 
  left_join(WR_rank_df) %>% 
  left_join(percentile_df) %>% 
  mutate(
    receiver_id = factor(receiver_id, rev(use_WR)),
    abbr.name = paste0(substr(teamPlayers.firstName, 1, 1), '.', teamPlayers.lastName),
    pl_lab = paste0(abbr.name, '\n', number(perc * 100, accuracy = 0.1), ' perc.'),
    sim_pg = szn_tot / tot_gp
  ) %>% 
  group_by(receiver_id) %>% 
  mutate(label_quant = quantile(sim_pg, .05)) %>% 
  ungroup %>% 
  ggplot(aes(x = sim_pg, y = receiver_id)) +
  stat_density_ridges(
    aes(fill = factor(stat(quantile))),
    geom = 'density_ridges_gradient',
    quantiles = c(.1,.25,.75,.9),
    rel_min_height = 0.001,
    calc_ecdf = T,
    scale = 1.3,
    color = 'grey50',
    size = 0.3,
    show.legend = F
  ) +
  geom_shadowtext(aes(label = pl_lab, x = label_quant), color = 'darkblue', family = "DIN Condensed", nudge_y = 0.6, size = 1.7, hjust = 1, bg.color = 'white', bg.r = 0.2) +
  geom_spoke(aes(x = PPR_pg, angle = pi/2, radius = 0.5), color = 'darkblue') +
  geom_grob(data = WR_rank_df %>% slice(1:30), aes(x = PPR_pg, y = as.numeric(factor(receiver_id, rev(use_WR))) + 0.5, label = grob_img_adj(teamPlayers.headshot_url)), vp.height = 0.015) +
  scale_fill_manual(values = c(alpha(color_SB[1], 0.4),alpha(color_SB[4], 0.4),alpha('grey60', 0.4),alpha(color_SB[4], 0.4),alpha(color_SB[1], 0.4))) +
  scale_x_continuous(breaks = seq(0,32,4), minor_breaks = seq(0,32,2), expand = expansion(mult = c(0.03, 0.03))) +
  scale_y_discrete(expand = expansion(mult = c(0.01, 0), add = c(0, 1.6))) +
  labs(title = 'Distribution of Expected WR Fantasy Points, 2019',
       subtitle = 'Based on 10,000 Simulations',
       x = 'PPR Fantasy Points Per Game') +
  theme_SB + 
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_line('grey85', size = 0.3)
  )

brand_plot(p, asp = 9/16, save_name = 'PPR WR fant ridges.png', data_home = 'Data: @nflfastR', fade_borders = 'tlr')
proc.time() - ptm


### get a bunch of years
fant_pt_avg_df <- do.call(rbind, lapply(2006:2019, function(x) {
  readRDS(url(paste0('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_',x,'.rds'))) %>% 
    filter(pass_attempt==1 & season_type=='REG' & two_point_attempt==0 & !is.na(receiver_id) & sack == 0) %>% 
    add_xyac2 %>% 
    select(season = season.x, game_id, play_id, receiver_id, yardline_100 = yardline_100.x, air_yards = air_yards.x, actual_yards_gained = yards_gained, complete_pass, cp, yac_prob = prob, gain) %>% 
    mutate(
      gain = ifelse(yardline_100==air_yards, yardline_100, gain),
      yac_prob = ifelse(yardline_100==air_yards, 1, yac_prob),
      PPR_points = 1 + gain/10 + ifelse(gain == yardline_100, 6, 0),
      catch_run_prob = cp * yac_prob,
      exp_PPR_points = PPR_points * catch_run_prob,
      exp_yards = gain * catch_run_prob,
      actual_outcome = ifelse(actual_yards_gained==gain & complete_pass==1, 1, 0),
      actual_PPR_points = ifelse(actual_outcome==1, PPR_points, 0),
      target = 0,
      game_played = 0
    )  %>% 
    group_by(game_id, receiver_id) %>% 
    mutate(game_played = ifelse(row_number()==1,1,0)) %>% 
    ungroup %>% 
    group_by(game_id, play_id, receiver_id) %>% 
    mutate(target = ifelse(row_number()==1,1,0)) %>% 
    ungroup %>% 
    group_by(season, receiver_id) %>% 
    summarize(
      games = sum(game_played, na.rm = T),
      targets = sum(target, na.rm = T),
      catches = sum(actual_outcome, na.rm = T),
      yards = sum(ifelse(actual_outcome==1, gain, 0), na.rm = T),
      td = sum(ifelse(gain==yardline_100, actual_outcome, 0), na.rm = T),
      exp_catches = sum(ifelse(target==1, cp, NA), na.rm = T),
      exp_yards = sum(exp_yards, na.rm = T),
      exp_td = sum(ifelse(gain==yardline_100, catch_run_prob, 0), na.rm = T),
      PPR_pts = sum(actual_PPR_points, na.rm = T),
      exp_PPR_pts = sum(exp_PPR_points, na.rm = T)
    ) %>% 
    ungroup %>% 
    return
}))

saveRDS(fant_pt_avg_df, 'exp PPR Fantasy pts.rds')
fant_pt_avg_df <- readRDS('exp PPR Fantasy pts.rds') %>% 
  left_join(roster_df, by = c('receiver_id' = 'ID', 'season' = 'team.season'))

fant_pt_avg_df %>% 
  ungroup %>% 
  mutate(
    exp_pts_targ = exp_PPR_pts / targets
  ) %>% 
  filter(targets >= 100 & teamPlayers.position=='WR') %>% 
  arrange(-exp_pts_targ)

yoy_compare <- fant_pt_avg_df %>% 
  mutate(season = season + 1) %>% 
  right_join(fant_pt_avg_df, by = c('season', 'receiver_id'), suffix = c('_prev', '_curr')) %>% 
  filter(targets_prev >= 75 & targets_curr >= 75 & teamPlayers.position_curr=='WR')

rsq <- function (x, y) cor(x, y) ^ 2

rsq(yoy_compare$targets_prev,yoy_compare$targets_curr)

rsq(yoy_compare$PPR_pts_prev/yoy_compare$targets_prev,yoy_compare$PPR_pts_curr/yoy_compare$targets_curr)
rsq(yoy_compare$exp_PPR_pts_prev/yoy_compare$targets_prev,yoy_compare$exp_PPR_pts_curr/yoy_compare$targets_curr)
rsq(yoy_compare$exp_PPR_pts_prev/yoy_compare$targets_prev,yoy_compare$PPR_pts_curr/yoy_compare$targets_curr)
rsq(yoy_compare$exp_PPR_pts_curr/yoy_compare$targets_curr,yoy_compare$PPR_pts_curr/yoy_compare$targets_curr)


yearly_yac_tot <- do.call(rbind, lapply(2006:2019, function(x) {
  readRDS(url(paste0('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_',x,'.rds'))) %>% 
    filter(pass_attempt==1 & season_type=='REG' & two_point_attempt==0 & !is.na(receiver_id) & sack == 0) %>% 
    group_by(season, receiver_id) %>% 
    summarize(
      tot_yac = sum(yards_after_catch, na.rm = T),
      tot_xyac = sum(xyac_mean_yardage, na.rm = T)
    ) %>% 
    return
}))

yearly_cpoe <- do.call(rbind, lapply(2006:2019, function(x) {
  readRDS(url(paste0('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_',x,'.rds'))) %>% 
    filter(pass_attempt==1 & season_type=='REG' & two_point_attempt==0 & sack == 0) %>% 
    group_by(season, passer_id) %>% 
    summarize(
      passes = sum(pass_attempt, na.rm = T),
      cpoe = mean(cpoe, na.rm = T)
    ) %>% 
    return
}))

games_df <- readRDS(url('http://nflgamedata.com/games.rds')) %>% 
  left_join(readRDS('games_QB_sportradarId.rds'))

home_qb <- games_df %>% 
  filter(week == 1 & season >= 2007) %>% 
  select(season, team = home_team, passer_id = home_qb_id)

away_qb <- games_df %>% 
  filter(week == 1 & season >= 2007) %>% 
  select(season, team = away_team, passer_id = away_qb_id)

#MIA & TB had week 1 byes in 2016
#games_df %>% filter(week == 2 & season == 2016 & away_team == 'TB') %>% pull(away_qb_id)
#games_df %>% filter(week == 2 & season == 2016 & away_team == 'MIA') %>% pull(home_qb_id)

qb_prior_yr <- rbind(home_qb, away_qb, c(2016,'TB','32013030-2d30-3033-3135-30339e998e6f'),c(2016,'MIA','32013030-2d30-3033-3133-34353afe165e')) %>% 
  mutate(season = as.numeric(season)) %>% 
  left_join(
    yearly_cpoe %>%
      mutate(season = season - 1) %>% 
      rename(
        passes_prev = passes,
        cpoe_prev = cpoe
      )
  )

prior_yac_oe <- yearly_yac_tot %>% 
  mutate(
    season = season - 1,
    tot_yac_oe_prior = tot_yac - tot_xyac,
    tot_yac = NULL,
    tot_xyac = NULL
  )

yoy_compare_mod <- yoy_compare %>% 
  left_join(prior_yac_oe) %>% 
  left_join(qb_prior_yr, by = c('season', 'team.abbr_curr' = 'team')) %>% 
  mutate(
    avg_yac_oe_prev = tot_yac_oe_prior / catches_prev,
    avg_yac_oe_prev = ifelse(is.na(avg_yac_oe_prev), -3.5, avg_yac_oe_prev),
    PPR_pts_per_targ_curr = PPR_pts_curr / targets_curr,
    PPR_pts_per_targ_prev = PPR_pts_prev / targets_prev,
    exp_PPR_pts_per_targ_prev = exp_PPR_pts_prev / targets_prev,
    exp_PPR_pts_per_targ_curr = exp_PPR_pts_curr / targets_curr,
    cpoe_prev = ifelse(is.na(cpoe_prev), -2, cpoe_prev)
  )

mod_w_all <- glm(PPR_pts_per_targ_curr ~ exp_PPR_pts_per_targ_prev + avg_yac_oe_prev + cpoe_prev, data = yoy_compare_mod)
yoy_compare_mod$pred <- predict(mod_w_all, yoy_compare_mod)
summary(mod_w_all)

rsq(yoy_compare_mod$PPR_pts_per_targ_prev, yoy_compare_mod$PPR_pts_per_targ_curr)
rsq(yoy_compare_mod$pred, yoy_compare_mod$PPR_pts_per_targ_curr)
yoy_compare_mod$exp_PPR_pts_curr


library(ggpubr)


p3 <- ggplot(data = yoy_compare_mod, aes(x = exp_PPR_pts_per_targ_curr, y = PPR_pts_per_targ_curr)) +
  geom_point(alpha = .4, color = color_SB[1], pch = 16, size = 0.4) +
  geom_smooth(method = 'lm', se = F, color = 'darkblue', size = 0.6) +
  stat_poly_eq(formula = y~x, aes(label = ..rr.label..), color = 'darkblue', size = 3, label.y = 0.9, rr.digits = 3, parse = T) +
  scale_y_continuous(expand = expansion(mult = c(0,0)), limits = c(0.5, 3)) +
  scale_x_continuous(expand = expansion(mult = c(0,0)), limits = c(0.5, 3)) +
  labs(title = '3) Expected vs Actual',
       subtitle = NULL,
       x = 'Expected',
       y = 'Actual') +
  theme_SB +
  theme(
    axis.title.y = element_text(angle = 90),
    plot.title = element_text(size = 9),
    axis.line = element_line(size = 0.5, color = 'darkblue'),
    panel.border = element_rect(color = 'grey95', size = 0.1)
  ) +
  annotation_custom(make_gradient(deg = 270), ymin=2.8, ymax=Inf, xmin=-Inf, xmax=Inf) +
  annotation_custom(make_gradient(deg = 0), ymin=-Inf, ymax=Inf, xmin=2.8, xmax=Inf)

p1 <- ggplot(data = yoy_compare_mod, aes(x = PPR_pts_per_targ_prev, y = PPR_pts_per_targ_curr)) +
  geom_point(alpha = .4, color = color_SB[1], pch = 16, size = 0.4) +
  geom_smooth(method = 'lm', se = F, color = 'darkblue', size = 0.6) +
  stat_poly_eq(formula = y~x, aes(label = ..rr.label..), color = 'darkblue', size = 3, label.y = 0.9, rr.digits = 3, parse = T) +
  scale_y_continuous(expand = expansion(mult = c(0,0)), limits = c(0.5, 3)) +
  scale_x_continuous(expand = expansion(mult = c(0,0)), limits = c(0.5, 3)) +
  labs(title = '1) Actual FP Stability',
       subtitle = NULL,
       x = 'Prior',
       y = 'Current') +
  theme_SB +
  theme(
    axis.title.y = element_text(angle = 90),
    plot.title = element_text(size = 9),
    axis.line = element_line(size = 0.5, color = 'darkblue'),
    panel.border = element_rect(color = 'grey95', size = 0.1)
  ) +
  annotation_custom(make_gradient(deg = 270), ymin=2.8, ymax=Inf, xmin=-Inf, xmax=Inf) +
  annotation_custom(make_gradient(deg = 0), ymin=-Inf, ymax=Inf, xmin=2.8, xmax=Inf)

p2 <- ggplot(data = yoy_compare_mod, aes(x = exp_PPR_pts_per_targ_prev, y = exp_PPR_pts_per_targ_curr)) +
  geom_point(alpha = .4, color = color_SB[1], pch = 16, size = 0.4) +
  geom_smooth(method = 'lm', se = F, color = 'darkblue', size = 0.6) +
  stat_poly_eq(formula = y~x, aes(label = ..rr.label..), color = 'darkblue', size = 3, label.y = 0.9, rr.digits = 3, parse = T) +
  scale_y_continuous(expand = expansion(mult = c(0,0)), limits = c(0.5, 3)) +
  scale_x_continuous(expand = expansion(mult = c(0,0)), limits = c(0.5, 3)) +
  labs(title = '2) Mean Expected FP Stability',
       subtitle = NULL,
       x = 'Prior',
       y = 'Current') +
  theme_SB +
  theme(
    axis.title.y = element_text(angle = 90),
    plot.title = element_text(size = 9),
    axis.line = element_line(size = 0.5, color = 'darkblue'),
    panel.border = element_rect(color = 'grey95', size = 0.1)
  ) +
  annotation_custom(make_gradient(deg = 270), ymin=2.8, ymax=Inf, xmin=-Inf, xmax=Inf) +
  annotation_custom(make_gradient(deg = 0), ymin=-Inf, ymax=Inf, xmin=2.8, xmax=Inf)

p4 <- ggplot(data = yoy_compare_mod, aes(x = exp_PPR_pts_per_targ_prev, y = PPR_pts_per_targ_curr)) +
  geom_point(alpha = .4, color = color_SB[1], pch = 16, size = 0.4) +
  geom_smooth(method = 'lm', se = F, color = 'darkblue', size = 0.6) +
  stat_poly_eq(formula = y~x, aes(label = ..rr.label..), color = 'darkblue', size = 3, label.y = 0.9, rr.digits = 3, parse = T) +
  scale_y_continuous(expand = expansion(mult = c(0,0)), limits = c(0.5, 3)) +
  scale_x_continuous(expand = expansion(mult = c(0,0)), limits = c(0.5, 3)) +
  labs(title = '4) Prior Yr Exp vs Current Yr Actual',
       subtitle = NULL,
       x = 'Prior Exp',
       y = 'Current Act') +
  theme_SB +
  theme(
    axis.title.y = element_text(angle = 90),
    plot.title = element_text(size = 9),
    axis.line = element_line(size = 0.5, color = 'darkblue'),
    panel.border = element_rect(color = 'grey95', size = 0.1)
  ) +
  annotation_custom(make_gradient(deg = 270), ymin=2.8, ymax=Inf, xmin=-Inf, xmax=Inf) +
  annotation_custom(make_gradient(deg = 0), ymin=-Inf, ymax=Inf, xmin=2.8, xmax=Inf)


title <- grobTree(rectGrob(gp=gpar(fill='grey95',col = 'transparent')),textGrob('PPR Fantasy Points per Target, 2007-2019', gp=gpar(col='darkblue', fontfamily=font_SB, fontsize = 14), y = 0.8, x = 0.08, hjust = 0, vjust = 0.8), textGrob('min. 75 Targets', gp=gpar(col='darkblue', fontfamily=font_SB, fontsize = 8), x = 0.08, hjust = 0, y = 0.18, vjust = 0))
p <- grid.arrange(title, p1, p2, p3, p4, ncol = 2, nrow = 3, layout_matrix = matrix(c(1,1,2,3,4,5), nrow = 3, byrow = T), heights=unit(c(30, 1, 1), c('pt','null','null')))
brand_plot(p, save_name = 'exp WR fant pt corr.png', data_home = 'Data: @nflfastR')


p <- ggplot(data = yoy_compare_mod, aes(x = pred, y = PPR_pts_per_targ_curr)) +
  geom_point(alpha = .4, color = color_SB[1], pch = 16, size = 0.8) +
  geom_smooth(method = 'lm', se = F, color = 'darkblue', size = 0.6) +
  stat_poly_eq(formula = y~x, aes(label = ..rr.label..), color = 'darkblue', size = 4, label.y = 0.9, rr.digits = 3, parse = T) +
  scale_y_continuous(expand = expansion(mult = c(0,0)), limits = c(0.5, 3)) +
  scale_x_continuous(expand = expansion(mult = c(0,0)), limits = c(0.5, 3)) +
  labs(title = 'Can Prior Year CPOE & YACoe Help?',
       subtitle = 'Week 1 QB Prior Year CPOE and Prior Season WR YACoe, 2007-2019  |  min. 75 Targets',
       x = 'Model Prediction',
       y = 'Actual\nPPR FP\nper Targ') +
  theme_SB +
  theme(
    #axis.title.y = element_text(angle = 90),
    #plot.title = element_text(size = 9),
    axis.line = element_line(size = 0.5, color = 'darkblue'),
    panel.border = element_rect(color = 'grey95', size = 0.1)
  ) +
  annotation_custom(make_gradient(deg = 270), ymin=2.8, ymax=Inf, xmin=-Inf, xmax=Inf) +
  annotation_custom(make_gradient(deg = 0), ymin=-Inf, ymax=Inf, xmin=2.8, xmax=Inf)


brand_plot(p, save_name = 'exp WR fant pt model.png', data_home = 'Data: @nflfastR')

### welker seasons
roster_df %>% filter(teamPlayers.lastName == 'Welker') %>% pull(ID) %>% unique


welker_targ_dist <- do.call(rbind, lapply(2006:2015, function(x) {
  readRDS(url(paste0('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_',x,'.rds'))) %>% 
    filter(pass_attempt==1 & season_type=='REG' & two_point_attempt==0 & !is.na(receiver_id) & sack == 0 & receiver_id=='32013030-2d30-3032-3234-3237c5d6fb3d') %>% 
    add_xyac2 %>% 
    select(season = season.x, game_id, play_id, receiver_id, yardline_100 = yardline_100.x, air_yards = air_yards.x, actual_yards_gained = yards_gained, complete_pass, cp, yac_prob = prob, gain) %>% 
    mutate(
      gain = ifelse(yardline_100==air_yards, yardline_100, gain),
      yac_prob = ifelse(yardline_100==air_yards, 1, yac_prob),
      PPR_points = 1 + gain/10 + ifelse(gain == yardline_100, 6, 0),
      catch_run_prob = cp * yac_prob,
      exp_PPR_points = PPR_points * catch_run_prob,
      actual_outcome = ifelse(actual_yards_gained==gain & complete_pass==1, 1, 0),
      actual_PPR_points = ifelse(actual_outcome==1, PPR_points, 0),
      target = 0,
      game_played = 0
    )
}))

welker_incomplete_df <- welker_targ_dist %>% 
  mutate(
    gain = 0,
    PPR_points = 0,
    yac_prob = 0,
    exp_PPR_points = 0,
    complete_pass = 0,
    catch_run_prob = 1 - cp,
    actual_outcome = NA,
    actual_PPR_points = NA,
    target = 1
  ) %>% 
  distinct %>% 
  group_by(game_id, receiver_id) %>% 
  mutate(game_played = ifelse(row_number()==1,1,0)) %>% 
  ungroup

sampling_df <- rbind(welker_incomplete_df, welker_targ_dist) %>% 
  select(season, game_id, play_id, receiver_id, catch_run_prob, PPR_points) %>% 
  group_by(game_id, play_id)

ptm <- proc.time()
sim_df <- do.call(rbind, lapply(1:10000, function(x) {
  sampling_df %>% 
    filter(!is.na(catch_run_prob)) %>% 
    mutate(sim_res = sample(PPR_points, 1, prob = catch_run_prob)) %>% 
    select(season, game_id, play_id, receiver_id, sim_res) %>% 
    distinct %>% 
    group_by(season, receiver_id) %>% 
    summarize(szn_tot = sum(sim_res, na.rm = T), .groups = 'drop') %>% 
    return
}))
proc.time() - ptm

WR_rank_df <- rbind(welker_incomplete_df, welker_targ_dist) %>% 
  group_by(season, receiver_id) %>% 
  summarize(
    tot_PPR = sum(actual_PPR_points, na.rm = T),
    tot_targ = sum(target),
    tot_gp = sum(game_played),
    PPR_pg = tot_PPR / tot_gp
  ) %>% 
  left_join(roster_df, by = c('receiver_id' = 'ID', 'season' = 'team.season'))

actual_perc <- WR_rank_df %>%
  ungroup %>% 
  select(season, receiver_id, sim_pg = PPR_pg) %>%
  mutate(sim = 0)

sim_perc <- sim_df %>% 
  left_join(WR_rank_df) %>% 
  mutate(
    sim_pg = szn_tot / tot_gp,
    sim = 1
  ) %>% 
  select(season, receiver_id, sim_pg, sim)

percentile_df <- rbind(actual_perc, sim_perc) %>% 
  group_by(season, receiver_id) %>% 
  mutate(perc = percent_rank(sim_pg)) %>% 
  filter(sim == 0) %>% 
  mutate(sim_pg = NULL, sim = NULL)


ptm <- proc.time()
p <- sim_df %>% 
  #slice(1:5000) %>% 
  left_join(percentile_df) %>% 
  left_join(WR_rank_df) %>% 
  mutate(
    sim_pg = szn_tot / tot_gp,
    pl_lab = paste0(season, '\n', number(perc * 100, accuracy = 0.1), ' perc.')
  ) %>% 
  group_by(season) %>% 
  mutate(label_quant = quantile(sim_pg, .02)) %>% 
  ungroup %>% 
  ggplot(aes(x = sim_pg, y = factor(rev(season)))) +
  stat_density_ridges(
    aes(fill = factor(stat(quantile))),
    geom = 'density_ridges_gradient',
    quantiles = c(.1,.25,.75,.9),
    rel_min_height = 0.001,
    calc_ecdf = T,
    scale = 1.3,
    color = 'grey50',
    size = 0.3,
    show.legend = F
  ) +
  geom_shadowtext(aes(label = pl_lab, x = label_quant), color = 'darkblue', family = "DIN Condensed", nudge_y = 0.6, size = 1.7, hjust = 1, bg.color = 'white', bg.r = 0.2) +
  geom_spoke(aes(x = PPR_pg, angle = pi/2, radius = 0.5), color = 'darkblue') +
  geom_grob(data = WR_rank_df %>% slice(1:30), aes(x = PPR_pg, y = as.numeric(factor(rev(season))) + 0.5, label = grob_img_adj(ESPN_logo_url(team.abbr))), vp.height = 0.04) +
  scale_fill_manual(values = c(alpha(color_SB[1], 0.4),alpha(color_SB[4], 0.4),alpha('grey60', 0.4),alpha(color_SB[4], 0.4),alpha(color_SB[1], 0.4))) +
  scale_x_continuous(breaks = seq(0,32,4), minor_breaks = seq(0,32,2), expand = expansion(mult = c(0.03, 0.03))) +
  scale_y_discrete(expand = expansion(mult = c(0.01, 0), add = c(0, 1.6))) +
  labs(title = 'Wes Welker Distribution of Expected Fantasy Points',
       subtitle = 'Based on 10,000 Simulations',
       x = 'PPR Fantasy Points Per Game') +
  theme_SB + 
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_line('grey85', size = 0.3)
  )

brand_plot(p, save_name = 'PPR WR fant ridges - welker.png', data_home = 'Data: @nflfastR', fade_borders = 'tlr')
proc.time() - ptm


### bar for most exp fantasy point in a season

p <- fant_pt_avg_df %>% 
  ungroup %>% 
  arrange(-exp_PPR_pts) %>% 
  filter(teamPlayers.position=='WR') %>% 
  slice(1:30) %>% 
  mutate(
    pl_lab = paste0(row_number(), ') ',teamPlayers.displayName, ', ', season),
    pl_lab = factor(pl_lab, rev(pl_lab))
  ) %>% 
  ggplot(aes(x = pl_lab, y = exp_PPR_pts, color = team.abbr, fill = team.abbr, label = pl_lab)) +
  geom_bar(stat = 'identity', show.legend = F, width = 0.8, size = 0.5) +
  geom_text(aes(y = exp_PPR_pts - max(exp_PPR_pts) * 0.02), size = 2.4, family = "DIN Condensed", color = 'white', hjust = 1, show.legend = F) + 
  geom_grob(aes(x = pl_lab, y = exp_PPR_pts + max(exp_PPR_pts) * 0.04, label = grob_img_adj(ESPN_logo_url(team.abbr))), vp.height = 0.03) +
  coord_flip() +
  scale_x_discrete(expand = expansion(add = c(0.5, 0.5))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(values = NFL_pri) +
  scale_color_manual(values = NFL_sec) +
  labs(title = 'Top Expected PPR Fantasy Points Seasons at WR, 2006-2019',
       y = 'Total PPR Fantasy Points') +
  theme_SB + 
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank()
  )

brand_plot(p, asp = 9/16, save_name = 'top exp WR ppr seasons.png', data_home = 'Data: @nflfastR', fade_borders = 'tr', axis_rot = T)

library(gt)

fant_pt_avg_df %>% 
  ungroup %>% 
  arrange(-exp_PPR_pts) %>% 
  filter(teamPlayers.position=='WR') %>% 
  slice(1:25) %>% 
  mutate(
    Rank = row_number()
  ) %>% 
  select(Rank, team.abbr, teamPlayers.displayName, season, targets, catches, yards, td, PPR_pts, exp_catches, exp_yards, exp_td, exp_PPR_pts) %>% 
  gt() %>%
  tab_header(
    title = 'Top Expected PPR Fantasy Points Seasons at WR, 2006-2019'
  ) %>% 
  cols_label(
    teamPlayers.displayName = '',
    team.abbr = '',
    season = 'Yr',
    targets = 'Targ',
    catches = 'Rec',
    yards = 'Yds',
    td = 'TD',
    PPR_pts = 'FP',
    exp_catches = 'Rec',
    exp_yards = 'Yds',
    exp_td = 'TD',
    exp_PPR_pts = 'FP'
  ) %>% 
  fmt_number(
    columns = vars(exp_td, PPR_pts, exp_PPR_pts),
    decimals = 1
  ) %>% 
  fmt_number(
    columns = vars(exp_catches),
    decimals = 0
  ) %>% 
  fmt_number(
    columns = vars(yards, exp_yards),
    decimals = 0,
    sep_mark = ','
  ) %>% 
  tab_style(
    style = cell_text(font = font_SB, size = 'x-large'),
    locations = cells_title(groups = 'title')
  ) %>% 
  tab_style(
    style = cell_text(font = font_SB, size = 'medium'),
    locations = cells_title(groups = 'subtitle')
  ) %>% 
  tab_style(
    style = cell_text(font = font_SB, align = 'center', size = 'medium'),
    locations = cells_body()
  ) %>% 
  tab_style(
    style = cell_text(font = font_SB, align = 'left'),
    locations = cells_body(vars(teamPlayers.displayName))
  ) %>% 
  tab_style(
    style = cell_text(font = font_SB, align = 'center', size = 'medium'),
    locations = cells_column_labels(columns = vars(Rank, season, targets, catches, yards, td, PPR_pts, exp_catches, exp_yards, exp_td, exp_PPR_pts))
  ) %>% 
  tab_style(
    style = list(
      cell_text(font = font_SB, align = 'center', v_align = 'bottom', size = 'large'),
      cell_fill(color = 'grey95')
    ),
    locations = cells_row_groups()
  ) %>% 
  tab_spanner(label = 'Actual', columns = vars(catches, yards, td, PPR_pts)) %>% 
  tab_spanner(label = 'Expected', columns = vars(exp_catches, exp_yards, exp_td, exp_PPR_pts)) %>% 
  tab_style(
    style = cell_text(font = font_SB, align = 'center', size = 'medium'),
    locations = cells_column_spanners(spanners = c('Actual','Expected'))
  ) %>% 
  tab_source_note(source_note = '') %>% 
  data_color(
    columns = vars(PPR_pts, exp_PPR_pts),
    colors = scales::col_numeric(
      palette = c('grey97', color_SB[1]),
      domain = c(230,400)
    ),
    autocolor_text = FALSE
  ) %>%
  text_transform(
    locations = cells_body(vars(team.abbr)),
    fn = function(x) web_image(url = ESPN_logo_url(x), height = 17)
  ) %>% 
  tab_options(
    table.font.color = 'darkblue',
    data_row.padding = '2px',
    row_group.padding = '3px',
    table.width = '800px',
    column_labels.border.bottom.color = 'darkblue',
    column_labels.border.bottom.width = 1.4,
    table_body.border.top.color = 'darkblue',
    row_group.border.top.width = 1.5,
    row_group.border.top.color = '#999999',
    table_body.border.bottom.width = 0.7,
    table_body.border.bottom.color = '#999999',
    row_group.border.bottom.width = 1,
    row_group.border.bottom.color = 'darkblue',
    table.border.top.color = 'transparent',
    table.background.color = '#F2F2F2',
    table.border.bottom.color = 'transparent',
    row.striping.background_color = '#FFFFFF',
    row.striping.include_table_body	= TRUE
  ) %>% 
  gtsave('top exp ppr seasons.png')


img <- png::readPNG('top exp ppr seasons.png')
img <- img[11:(nrow(img)-10),11:(ncol(img)-10),]

p <- ggplot(iris, aes(Species, Sepal.Length))+
  annotation_custom(rasterGrob(img, 
                               width = unit(1,"npc"), 
                               height = unit(1,"npc")), 
                    -Inf, Inf, -Inf, Inf) +
  theme_void()

brand_plot(p, asp = ncol(img)/nrow(img), save_name = 'top exp ppr seasons.png', data_home = 'Data: @nflfastR')


### show an example
library(gt)

rbind(incomplete_df, fant_pt_dist_df) %>% 
  filter(game_id == '2019_14_SF_NO' & play_id == 3861) %>% 
  mutate(
    outcome =case_when(
      gain == 0 ~ 'Incomplete Pass',
      gain == 21 ~ 'Touchdown',
      TRUE ~ paste0('Gain of ',gain)
    ),
    cp = percent(cp, accuracy = 0.1),
    yac_prob = ifelse(yac_prob < 0.0005 & yac_prob > 0, '<0.1%',percent(yac_prob, accuracy = 0.1)),
    catch_run_prob = ifelse(catch_run_prob < 0.0005 & catch_run_prob > 0, '<0.1%',percent(catch_run_prob, accuracy = 0.1)),
    yac_prob = ifelse(outcome=='Incomplete Pass','',yac_prob),
    cp = ifelse(outcome=='Incomplete Pass','',cp)
  ) %>% 
  select(outcome, cp, yac_prob, catch_run_prob, PPR_points) %>% 
  gt() %>%
  tab_header(
    title = 'Combining Expected YAC and Completion Probability Models',
    subtitle = 'game_id = 2019_14_SF_NO & play_id = 3861'
  ) %>% 
  cols_label(
    outcome = 'Play Outcome',
    cp = 'Comp Prob',
    yac_prob = 'YAC Prob',
    catch_run_prob = 'Final Prob',
    PPR_points = 'Fant Pts'
  ) %>% 
  fmt_number(
    columns = vars(PPR_points),
    decimals = 1
  ) %>% 
  tab_style(
    style = cell_text(font = font_SB, size = 'x-large'),
    locations = cells_title(groups = 'title')
  ) %>% 
  tab_style(
    style = cell_text(font = font_SB, size = 'medium'),
    locations = cells_title(groups = 'subtitle')
  ) %>% 
  tab_style(
    style = cell_text(font = font_SB, align = 'center', size = 'medium'),
    locations = cells_body()
  ) %>% 
  tab_style(
    style = cell_text(font = font_SB, align = 'left'),
    locations = cells_body(vars(outcome))
  ) %>% 
  tab_style(
    style = cell_text(font = font_SB, align = 'center', size = 'medium'),
    locations = cells_column_labels(columns = vars(cp, yac_prob, catch_run_prob, PPR_points))
  ) %>% 
  tab_style(
    style = cell_text(font = font_SB, align = 'left', size = 'medium'),
    locations = cells_column_labels(columns = vars(outcome))
  ) %>% 
  tab_style(
    style = list(
      cell_text(font = font_SB, align = 'center', v_align = 'bottom', size = 'large'),
      cell_fill(color = 'grey95')
    ),
    locations = cells_row_groups()
  ) %>% 
  tab_spanner(label = '(A)', columns = vars(cp)) %>% 
  tab_spanner(label = '(B)', columns = vars(yac_prob)) %>% 
  tab_spanner(label = '(C)', columns = vars(catch_run_prob)) %>% 
  tab_spanner(label = '(D)', columns = vars(PPR_points)) %>% 
  tab_style(
    style = cell_text(font = font_SB, align = 'center', size = 'medium'),
    locations = cells_column_spanners(spanners = c('(A)','(B)','(C)','(D)'))
  ) %>% 
  tab_source_note(source_note = '') %>% 
  tab_options(
    table.font.color = 'darkblue',
    data_row.padding = '2px',
    row_group.padding = '3px',
    table.width = '500px',
    column_labels.border.bottom.color = 'darkblue',
    column_labels.border.bottom.width = 1.4,
    table_body.border.top.color = 'darkblue',
    row_group.border.top.width = 1.5,
    row_group.border.top.color = '#999999',
    table_body.border.bottom.width = 0.7,
    table_body.border.bottom.color = '#999999',
    row_group.border.bottom.width = 1,
    row_group.border.bottom.color = 'darkblue',
    table.border.top.color = 'transparent',
    table.background.color = '#F2F2F2',
    table.border.bottom.color = 'transparent',
    row.striping.background_color = '#FFFFFF',
    row.striping.include_table_body	= TRUE
  ) %>% 
  gtsave('fant_WR_ex.png')


img <- png::readPNG('fant_WR_ex.png')
img <- img[11:(nrow(img)-10),11:(ncol(img)-10),]

p <- ggplot(iris, aes(Species, Sepal.Length))+
  annotation_custom(rasterGrob(img, 
                               width = unit(1,"npc"), 
                               height = unit(1,"npc")), 
                    -Inf, Inf, -Inf, Inf) +
  theme_void()

brand_plot(p, asp = ncol(img)/nrow(img), save_name = 'fant_WR_ex.png', data_home = 'Data: @nflfastR')


