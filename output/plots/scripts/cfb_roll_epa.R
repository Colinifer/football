
current_season <- year

con <- fx.db_con(x.host = 'localhost')
pbp <- tbl(con, 'cfbfastR_pbp') %>% 
  filter(year == current_season & 
           !is.na(pos_team) & 
           (rush == 1 | pass == 1)) %>% 
  collect()
print(current_season)

n_week <- fx.n_week(pbp)

cfb_team_info <- readRDS('data/cfb/cfb_team_info.rds') %>% 
  rename(
    logos_light = `logos[0]`, 
    logos_dark = `logos[1]`
  )

team_colors_logos <- cfb_team_info %>% 
  select(school, abbreviation, color, logos_light, logos_dark, alt_color) %>%
  unnest(logos_light) %>%
  group_by(school) %>%
  # slice(1) %>% 
  ungroup()

off_epa <- pbp %>%
  filter(rush == 1 | pass == 1) %>% 
  group_by(offense_play, offense_conference) %>%
  summarize(off_epa = mean(EPA, na.rm = TRUE)) %>%
  arrange(desc(off_epa)) %>%
  rename(Team = offense_play) %>%
  filter(!is.na(offense_conference)) %>%
  ungroup() %>%
  mutate(Rank = row_number()) %>%
  mutate(TeamRank = paste0(Team, ' #', Rank))

off_epa %>%
  filter(off_epa > 0) %>%
  ggplot(aes(x = reorder(TeamRank, off_epa), y=off_epa)) +
  geom_point(size = 3) +
  coord_flip() +
  theme_bw() +
  ylab('Average EPA Per Play') + xlab('') +
  labs(title = 'Offensive EPA Per Play | Positive EPA Teams',
       caption = 'Chart by @cfbNate
       Data from @CFB_Data via @cfbfastr')

all_logos <- readRDS('plots/assets/cfb_logos/team_logos.rds')
signature <- 'Colin Welsh'
ma_plays <- 30

team <- 'Temple'

team_off <- pbp %>%
  filter(offense_play == team) %>%
  filter(rush == 1 | pass == 1) %>%
  filter(!is.na(EPA)) %>%
  mutate(cu_epa=cummean(EPA),  #this field is not used in this vignette but it could be substituted later to graph the cumulative EPA
         ma_epa=rollapply(EPA,ma_plays,mean,align='right',fill=NA),
         play_count = row_number(),
         week_team = paste0('WK', ifelse(week > 9, week, paste0(0,week)), ' ', defense_play))

team_off_play_start <- team_off %>%
  group_by(week_team) %>%
  slice(1) %>%
  select(defense_play, week_team, play_count) %>%
  rename(play_start = play_count,
         team = defense_play)

team_off_play_stop <- team_off %>%
  group_by(week_team) %>%
  filter(row_number() == n()) %>%
  select(week_team, play_count) %>%
  rename(play_stop = play_count)

team_off_start_stop <- team_off_play_start %>%
  left_join(team_off_play_stop, by = 'week_team') %>%
  mutate(midpoint = (play_start + play_stop)/2) %>% 
  left_join(team_colors_logos, by = c('team' = 'school')) %>% 
  mutate(color = replace_na(color,'gray')) %>%
  select(team, week_team, play_start, play_stop, midpoint, color) %>% 
  left_join(all_logos, by = c('team' = 'school'))

play_count <- max(team_off$play_count)

team_colors <- as.character(team_off_start_stop$color)
names(team_colors) <- as.character(team_off_start_stop$team)

ggplot() +
  geom_rect(
    data = team_off_start_stop,
    aes(
      xmin = play_start,
      xmax = play_stop,
      fill = team,
      ymin = -.5,
      ymax = .9
    ),
    color = 'gray90'
  ) +
  geom_rect(
    data = team_off_start_stop,
    aes(
      xmin = play_start,
      xmax = play_stop,
      ymin = .8,
      ymax = 1
    ),
    color = 'gray90',
    fill = 'white'
  ) +
  scale_fill_manual(values = team_colors) +
  geom_hline(
    yintercept = quantile(off_epa$off_epa),
    linetype = 2,
    color = 'gray20',
    alpha = .8
  ) +
  geom_hline(
    yintercept = 0,
    linetype = 1,
    color = 'gray20',
    alpha = .2
  ) +
  geom_grob(
    data = team_off_start_stop,
    aes(
    x = midpoint,
    y = .9,
    label = grob_img_adj(logo),
    vp.height = 0.08
  )) +
  # geom_image(
  #   data = team_off_start_stop,
  #   aes(x = midpoint, y = .9, image = logo),
  #   asp = 16 / 9,
  #   size = .05
  # ) +
  annotate(
    x = -2,
    y = quantile(off_epa$off_epa)[1],
    geom = 'text',
    size = 3,
    hjust = 'right',
    vjust = 0,
    label = off_epa %>% slice(n()) %>% pull(Team)
  ) +
  annotate(
    x = -2,
    y = quantile(off_epa$off_epa)[2],
    geom = 'text',
    size = 3,
    hjust = 'right',
    vjust = 0,
    label = '25%ile'
  ) +
  annotate(
    x = -2,
    y = quantile(off_epa$off_epa)[3],
    geom = 'text',
    size = 3,
    hjust = 'right',
    vjust = 0,
    label = 'Median'
  ) +
  annotate(
    x = -2,
    y = quantile(off_epa$off_epa)[4],
    geom = 'text',
    size = 3,
    hjust = 'right',
    vjust = 0,
    label = '75%ile'
  ) +
  annotate(
    x = -2,
    y = quantile(off_epa$off_epa)[5],
    geom = 'text',
    size = 3,
    hjust = 'right',
    vjust = 0,
    label = off_epa %>% slice(1) %>% pull(Team)
  ) +
  geom_line(
    data = team_off,
    aes(x = play_count, y = ma_epa),
    color = 'white',
    size = 2
  ) +
  geom_line(data = team_off, aes(x = play_count, y = ma_epa), size = 1.25) +
  theme_minimal() + theme(panel.grid = element_blank()) + theme(legend.position = 'none') +
  ylab('EPA') + xlab('Number of Plays') +
  labs(
    title = paste0(team, ' Offensive EPA | ', ma_plays, '-Play Moving Average'),
    caption = paste0(
      'Chart by ',
      signature,
      ' using code from @cfbNate
       Data from @CFB_Data via @cfbfastR'
    )
  ) +
  coord_cartesian(xlim = c(-20, play_count),  # This leaves room for the labels over the dashed lines
                  clip = 'off')               # This keeps the labels from disappearing

which.min(
  abs(
    epa_perc - 
      off_epa %>% 
      filter(Team == 'Cincinnati') %>% 
      pull(off_epa)
    )
  ) %>% 
  system.time()

which(
  abs(epa_perc - 
        off_epa %>% 
        filter(Team == 'Cincinnati') %>% 
        pull(off_epa)
      ) == min(
        abs(
          epa_perc - 
            off_epa %>%
            filter(Team == 'Cincinnati') %>% 
            pull(off_epa)
            )
        )
  ) %>% 
  system.time()

