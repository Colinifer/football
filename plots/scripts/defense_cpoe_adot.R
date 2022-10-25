library(tidyverse)
library(nflfastR)

# choose seasons for which the plot shall be generated
# CPOE starts in 2006
current_season <- year

# Load pbp for the chosen season from nflfastR data repo
# can be multiple seasons
con <- fx.db_con(x.host = 'localhost')
# lapply(2006:2019, function(season){
pbp <- tbl(con, 'nflfastR_pbp') |> 
  filter(season == current_season &
           season_type == 'REG') |> 
  collect() |> 
  decode_player_ids(fast = TRUE) |> 
  mutate(defteam = ifelse(defteam == "LA", "LAR", defteam),
         posteam = ifelse(posteam == "LA", "LAR", posteam),
         posteam = ifelse(season < 2016 & posteam == 'LAR', 'STL', posteam),
         defteam = ifelse(season < 2016 & defteam == 'LAR', 'STL', defteam),
         posteam = ifelse(season < 2017 & posteam == 'LAC', 'SD', posteam),
         defteam = ifelse(season < 2017 & defteam == 'LAC', 'SD', defteam),
         posteam = ifelse(season < 2020 & posteam == 'LV', 'OAK', posteam),
         defteam = ifelse(season < 2020 & defteam == 'LV', 'OAK', defteam))
dbDisconnect(con)

# compute cpoe grouped by air_yards
cpoe <-
  pbp |>
  filter(!is.na(cpoe)) |>
  group_by(defteam, air_yards) |>
  summarise(count = n(), cpoe = mean(cpoe, na.rm = T))

# summarise cpoe using player ID (note that player ids are 'NA' for 'no_play' plays. 
# Since we would filter those plays anyways we can use the id here)
# The correct name is being joined using the roster data
# first arranged by number of plays to filter the 30 QBs with most pass attempts
# The filter is set to 30 because we want to have 6 columns and 5 rows in the facet
summary_df <-
  pbp |>
  filter(!is.na(cpoe)) |>
  group_by(defteam) |>
  summarise(plays = n(),
            total_cpoe = mean(cpoe, na.rm = T)) |>
  arrange(plays |> desc()) |>
  arrange(total_cpoe) |> 
  left_join(cpoe, by = "defteam") |>
  left_join(
    teams_colors_logos |> select(team_abbr, team_color, team_logo_espn),
    by = c("defteam" = "team_abbr")
  ) |> 
  mutate(facet_label_wrap = glue('{defteam}: {total_cpoe |> round(2)}'),
         rounded_cpoe = total_cpoe |> round(2)) |> 
  mutate_at(vars(total_cpoe), funs(factor(., levels=unique(.))))

# create data frame used to add the logos
# arranged by name because name is used for the facet
colors_raw <-
  summary_df |>
  group_by(defteam) |>
  summarise(defteam = first(defteam)) |>
  left_join(
    teams_colors_logos |> select(team_abbr, team_color),
    by = c("defteam" = "team_abbr")
  ) |>
  arrange(defteam)

summary_images_df <- 
  summary_df |> 
  select(defteam, total_cpoe, rounded_cpoe, team_logo_espn) |> 
  mutate(status = color_cw[5],
         lab_cpoe = glue('Total CPOE: {rounded_cpoe}')) |> 
  unique()

# the below used smooth algorithm uses the parameter n as the number
# of points at which to evaluate the smoother. When using color as aesthetics
# we need exactly the same number of colors (-> n times the same color per player)
n_eval <- 80
colors <-
  as.data.frame(lapply(colors_raw, rep, n_eval)) |>
  arrange(defteam)

# mean data frame for the smoothed line of the whole league
mean <-
  summary_df |>
  group_by(air_yards) |>
  summarise(league = mean(cpoe, na.rm = T), league_count = n())

panel_label <- summary_df$defteam
names(panel_label) <- summary_df$total_cpoe

# create the plot. Set asp to make sure the images appear in the correct aspect ratio
asp <- 16/16

# Desktop
p <-
  summary_df |> 
  # arrange(total_cpoe |> desc()) |> 
  ggplot(aes(x = air_yards, y = cpoe)) +
  geom_smooth(data = mean, aes(x = air_yards, 
                               y = league, 
                               weight = league_count), 
              n = n_eval, 
              color = "red", 
              alpha = 0.7, 
              se = FALSE, 
              size = 0.5, 
              linetype = "dashed"
  ) +
  geom_smooth(se = FALSE, alpha = 0.9, 
              aes(color = defteam, weight = count), 
              size = 0.65, n = n_eval
  ) +
  scale_color_manual(values =  NFL_pri_dark,
                     name = "Team") +
  geom_point(aes(color = defteam), 
             size = summary_df$count / 15, 
             alpha = 0.4) + 
  # scale_fill_manual(values =  NFL_pri,
  #                   name = "Team") +
  ggimage::geom_image(data = summary_images_df, aes(x = 27.5, y = -16.5, image = team_logo_espn),
                      size = .2, by = "width", asp = asp
  ) +
  geom_shadowtext(data = summary_images_df,
                  aes(label = lab_cpoe, 
                      x = 31, 
                      y = -24.5),
                  color = color_cw[5],
                  bg.color = color_cw[2],
                  bg.r = .4,
                  hjust = 1,
                  family = "Montserrat",
                  size = 1.4) +
  xlim(-10, 40) + # makes sure the smoothing algorithm is evaluated between -10 and 40
  coord_cartesian(xlim = c(-5, 30), ylim = c(-25, 25)) + # 'zoom in'
  labs(
    x = "Target Depth in Yards Thrown Beyond the Line of Scrimmage (DOT)",
    y = "Completion Percentage Over Expectation (CPOE)",
    title = glue::glue("Defensive Passing Efficiency {current_season}"),
    subtitle = "CPOE as a function of target depth. Dotsize equivalent to number of targets.\nSmoothed for -10 ≤ DOT ≤ 30 Yards. Red Line = League Average."
  )

p_desktop <- p + 
  facet_wrap(~total_cpoe, labeller = labeller(total_cpoe = panel_label), ncol = 8) +
  theme_cw_dark +
  theme(
    axis.title = element_text(size = 8),
    axis.text = element_text(size = 5),
    axis.ticks = element_line(color = color_cw[5], size = 0.3),
    axis.ticks.length = unit(2, 'pt'),
    axis.title.y = element_text(angle = 90),
    plot.title = element_text(size = 12),
    plot.subtitle = element_text(size = 6),
    # plot.margin = margin(1, 1, 1, 1, unit = "cm"),
    panel.background = element_rect(fill = color_cw[2]),
    panel.spacing.x = unit(1.25, "lines"),
    panel.spacing.y = unit(1, "lines"),
    legend.position = "none",
    legend.title = element_blank(),
    legend.text = element_blank(),
    strip.text = element_text(size = 4, hjust = 0.5)
  )

p_mobile <- p +
  facet_wrap(~total_cpoe, labeller = labeller(total_cpoe = panel_label), ncol = 4) +
  theme_cw_dark +
  theme(
    axis.title = element_text(size = 8),
    axis.text = element_text(size = 5),
    axis.ticks = element_line(color = color_cw[5], size = 0.3),
    axis.ticks.length = unit(2, 'pt'),
    axis.title.y = element_text(angle = 90),
    plot.title = element_text(size = 12),
    plot.subtitle = element_text(size = 6),
    plot.margin = margin(.25, 1, .25, .25, unit = "cm"),
    panel.background = element_rect(fill = color_cw[2]),
    panel.spacing.x = unit(1.25, "lines"),
    panel.spacing.y = unit(.7, "lines"),
    legend.position = "none",
    legend.title = element_blank(),
    legend.text = element_blank(),
    strip.text = element_text(size = 4, hjust = 0.5)
  )

# save the plot
brand_plot(p_desktop, asp = 16/10, save_name = glue('plots/desktop/defense/defense_cpoe_vs_dot/defense_cpoe_vs_dot_{current_season}.png'), data_home = 'Data: @nflfastR', fade_borders = '')

brand_plot( p_mobile, asp = 10/16, save_name = glue('plots/mobile/defense/defense_cpoe_vs_dot/defense_cpoe_vs_dot_{current_season}.png'), data_home = 'Data: @nflfastR', fade_borders = '')

rm(
  cpoe,
  summary_df,
  colors_raw,
  summary_images_df,
  n_eval,
  colors,
  mean,
  panel_label,
  asp,
  p,
  p_desktop,
  p_mobile
)

# })
