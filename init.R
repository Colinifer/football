# Packages & Init Setup ---------------------------------------------------

# devtools::install_github("mrcaseb/nflfastR")
# devtools::install_github("dynastyprocess/ffscrapr")
# devtools::install_github("jthomasmock/espnscrapeR")
proj_name <- "football"
pkgs <- c(
  "devtools",
  "tidyverse",
  "nflfastR",
  "gsisdecoder",
  "espnscrapeR",
  "DBI",
  "odbc",
  "RMariaDB",
  "distill",
  "httr",
  "readr",
  "pander",
  "furrr",
  "na.tools",
  "ggimage",
  "teamcolors",
  "glue",
  "dplyr",
  "jsonlite",
  "RJSONIO",
  "tictoc",
  "animation",
  "gt",
  "reactable",
  "png",
  "DT",
  "ggthemes",
  "ggforce",
  "ggridges",
  "ggrepel",
  "ggpmisc",
  "ggbeeswarm",
  "cowplot",
  "gridExtra",
  "grid",
  "extrafont",
  "shadowtext",
  "tidytext",
  "RCurl"
)
installed_packages <- pkgs %in%
  rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(pkgs[!installed_packages])
}
lapply(pkgs, library, character.only = TRUE)
# library("nflscrapR") # doesn't work anymore
library("nflfastR")
# library("ffscrapr")

# Detach all packages
# lapply(paste0('package:', names(sessionInfo()$otherPkgs)), detach, character.only=TRUE, unload=TRUE)

rm(pkgs, installed_packages)

source("../initR/init.R")
fx.setdir(proj_name)

# Create standard objects -------------------------------------------------

# Connect to DB
source("../initR/con.R")
dbListTables(con)
dbDisconnect(con)


# Fantasy variables -------------------------------------------------------

# Create variables --------------------------------------------------------

# ESPN Fantasy Football
swid  <-  "{2BA315B4-5941-4B1C-A315-B459416B1CC1}"
espn_s2 <- "AEBtGuDXUCKk6SpqlY71qdBDW%2BYc5KGa80m%2F0EVX9NCF%2FIFBM5b8ZMKgrMovpUeUqFTp4M%2BrPbM1I4rT1Ra2oXbM847nUp25DBY9Q%2FsAPChAykF5VNEZ05VjF6Vu3thAU0WkzQeBbjkdzNGqfbmPtMNzrBy8oV7fcAlwh4X89q4XlfPNED8ppKynNj5admyBk7WaqNzQtZJLlStpyOjz3F3d5BwUtQ8kh390OPB5HEEPfiH4%2FBftKqsLF%2BlyhTFaDiM%3D"
kona_v3_environment <- '{"leagueId":1034400,"seasonId":null}'
kona_v3_teamcontrol <- '{"leagueId":1034400,"seasonId":2020,"teamId":8}'

ff_fantasy_key <- "fantasy_football/data/fantasy_key.rds"
# league_id <- c("1034400", "62746259", "39973580")
# team_id <- c("8", "9", "10")
# league_name <- c("Colin's Minions", "Drinker's Slushy Beer", "Family League 3.0")
# team_name <- c("Rhule Tide", "Golden Rhule", "Matt Rhules")
# fantasy_key <- data.frame(league_id, league_name, team_id, team_name)
# fantasy_key %>% write_rds(ff_fantasy_key)
cookies = c(`SWID` = swid,
            `espn_s2` = espn_s2)
# 'kona_v3_environment_season_ffl' = kona_v3_environment,
# 'kona_v3_teamcontrol_ffl' = kona_v3_teamcontrol)
cookie <- paste(names(cookies), cookies, sep = "=", collapse = ";")
# fantasy_key <- ff_fantasy_key %>% read_csv()
fantasy_key <- ff_fantasy_key %>% readRDS()
base = "https://fantasy.espn.com/apis/v3/games/ffl/seasons/"
year = Sys.Date() %>% format(format = "%Y")
mid = "/segments/0/leagues/"
leagueID <- fantasy_key$league_id[3]
tail = "?view=mDraftDetail&view=mLiveScoring&view=mMatchupScore&view=mPendingTransactions&view=mPositionalRatings&view=mSettings&view=mTeam&view=modular&view=mNav&view=mMatchupScore"
player_tail = "?view=kona_player_info"
wl_tail = "?view=proTeamSchedules_wl"
user_agent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.14; rv:79.0) Gecko/20100101 Firefox/79.0"

source('plots/assets/plot_theme.R')
source("fantasy_football/ff_init.R")


pbp_df <- readRDS(url(glue('https://github.com/guga31bb/nflfastR-data/blob/master/data/play_by_play_{year}.rds?raw=true')))
pbp_df <- decode_player_ids(pbp_df, fast = T)

offense <- pbp_df %>%
  filter(!is.na(epa) & !is.na(posteam)) %>% 
  group_by(posteam, season)%>%
  summarize(
    n_pass=sum(pass),
    n_rush=sum(rush),
    epa_per_pass=sum(epa*pass)/n_pass,
    epa_per_rush=sum(epa*rush)/n_rush,
    success_per_pass=sum(pass*epa>0)/n_pass,
    success_per_rush=sum(rush*epa>0)/n_rush,
    off_epa=mean(epa),
    off_success=mean(success)
  )

defense <- pbp_df %>%
  filter(!is.na(epa) & !is.na(defteam)) %>% 
  group_by(defteam, season)%>%
  summarize(
    def_n_pass=sum(pass),
    def_n_rush=sum(rush),
    def_epa_per_pass=sum(epa*pass)/def_n_pass,
    def_epa_per_rush=sum(epa*rush)/def_n_rush,
    def_success_per_pass=sum(pass*epa>0)/def_n_pass,
    def_success_per_rush=sum(rush*epa>0)/def_n_rush,
    def_epa=mean(epa),
    def_success=mean(success)
  )

chart_all <- offense %>% 
  inner_join(defense, by=c("season", "posteam" = "defteam")) %>%
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr"))

res <- 800 #size of exported plots
slope = -1.5 #for the tiers stuff
qb_min <- 320 #min # of qb plays

chart_all %>% 
  ggplot(aes(x = off_epa, y = def_epa)) +
  geom_image(aes(image = team_logo_espn), size = 0.05, asp = 16/9) +
  geom_hline(yintercept = mean(chart_all$off_epa), color = "red", linetype = "dashed") +
  geom_vline(xintercept =  mean(chart_all$def_epa), color = "red", linetype = "dashed") +
  labs(x = "Offense EPA/play",
       y = "Defense EPA/play",
       caption = "Figure: @benbbaldwin | Data: @nflscrapR",
       title = paste(year, "NFL team tiers")) +
  geom_abline(slope=slope, intercept=.4, alpha=.2) +
  geom_abline(slope=slope, intercept=.3, alpha=.2) +
  geom_abline(slope=slope, intercept=0, alpha=.2) +
  geom_abline(slope=slope, intercept=.1, alpha=.2) +
  geom_abline(slope=slope, intercept=.2, alpha=.2) +
  geom_abline(slope=slope, intercept=-.1, alpha=.2) +
  geom_abline(slope=slope, intercept=-.2, alpha=.2) +
  geom_abline(slope=slope, intercept=-.3, alpha=.2) +
  theme_bw()+
  scale_y_reverse() +
  theme(
    legend.position = c(0.99, 0.99),
    legend.justification = c(1, 1) ,
    plot.title = element_text(size = 16, hjust = 0.5),
    #panel.grid.minor = element_blank()
  )

  them_cw() +
  theme(
    axis.line = element_line(color = 'darkblue', size = 1.5),
    panel.border = element_rect(color = 'grey95', size = 0.3),
    plot.margin = unit(c(15,25,15,15), 'points')
  ) +
  annotation_custom(make_gradient(deg = 270), ymin=0.29, ymax=Inf, xmin=-Inf, xmax=Inf) +
  annotation_custom(make_gradient(deg = 0), ymin=-Inf, ymax=Inf, xmin=0.29, xmax=Inf) +
  transition_states(week, state_length = 3, transition_length = 2, wrap = F)