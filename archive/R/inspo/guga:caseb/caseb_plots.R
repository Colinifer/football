source("https://raw.githubusercontent.com/leesharpe/nfldata/master/code/plays-functions.R")
library(colortools)

# load nflscrapR dataset
pbp <- readRDS("data/pbp") %>%
  filter(season == 2019) 

# collect all rushplays incl. penalties
rushplays <- pbp %>%
  filter(rush==1 & play==1)

# summary to plot correct means in the facet
rushsummary <- 
  rushplays %>% 
  group_by(posteam) %>%
  summarize(wpmean=mean(wp)) %>%
  apply_colors_and_logos()

# collect all passplays incl. penalties
passplays <- pbp %>%
  filter(pass==1 & play==1)

# summary to plot correct means in the facet
passsummary <- 
  passplays %>% 
  group_by(posteam) %>%
  summarize(wpmean=mean(wp)) %>%
  apply_colors_and_logos()

col <- splitComp("blue")

ggplot(NULL, aes(x=wp)) +
  geom_vline(data=passsummary, aes(xintercept = wpmean), color = col[1], linetype = "dashed") +
  geom_vline(data=rushsummary, aes(xintercept = wpmean), color = col[2], linetype = "dashed") +
  stat_density(data=rushplays, geom = "line", position = "identity", size = .3, aes(color = col[2])) +
  stat_density(data=passplays, geom = "line", position = "identity", size = .3, aes(color = col[1])) +
  geom_density(data=rushplays, alpha = .3, color = col[2], fill = col[2]) +
  geom_density(data=passplays, alpha = .3, color = col[1], fill = col[1]) +
  geom_image(data=passsummary, aes(x = .1, y = 1.8, image=team_logo), size = .15, by='width', asp=1) +
  xlim(0, 1) +
  scale_colour_identity(name = "Playtype",
                        breaks = c(col[2], col[1]),
                        labels = c("Rushing Plays", "Passing Plays"),
                        guide = "legend"
                        )+
  labs(x = "",
       y = "",
       caption = "Figure: @mrcaseb | Data: @nflscrapR",
       title = 'Playcalling 2019 Regular Season',
       subtitle = "Smoothed Distribution of Passing and Rushing Plays (x-Axis = Estimated win probability when calling the play)"
       ) +
  theme_bw() +  
  theme(axis.title = element_text(size = 6),
        axis.text = element_text(size = 6),
        plot.title = element_text(size = 10, hjust = 0.5, face = 'bold'),
        plot.subtitle = element_text(size = 8, hjust = 0.5),
        plot.caption = element_text(size = 6),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 6),
        strip.text = element_text(size = 6, hjust = 0.5, face = 'bold')
        ) +
  theme(legend.background = element_rect(fill="grey", size=0.5, linetype="solid"),
        legend.position = "top"
        ) +
  facet_wrap( ~ posteam, ncol=8, scales = "free_x")

