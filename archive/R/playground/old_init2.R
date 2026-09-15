## devtools::install_github(repo = "maksimhorowitz/nflscrapR")

pkgs <- c("devtools", "tidyverse", "readr",
          "pander", "na.tools", "ggimage",
          "devtools", "teamcolors", "glue",
          "dplyr", "tictoc", "animation", 
          "gt", "DT", "ggthemes", 
          "bbplot", "ggtext", "ggforce", 
          "ggridges", "ggrepel", "ggbeeswarm", 
          "extrafont")
installed_packages <- pkgs %in%
  rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(pkgs[!installed_packages])
}
invisible(lapply(pkgs, library, character.only = TRUE))
library("nflscrapR")


##install.packages(c("devtools", "tidyverse", "readr", "pander", "na.tools", "ggimage", "devtools", "teamcolors", "glue", "animate", "dplyr", "tictoc", "animation"))
##devtools::install_github(repo = "maksimhorowitz/nflscrapR")

##reset
setwd("~/")
gid <- paste(getwd())
gid
device <- ""

if (gid == "/Volumes/HDD/Users/colinwelsh") {
  ##Maverick - MBP
  setwd("~/Documents/dev/football")
  device <- "Maverick (MBP)"
} else if (gid == "/Users/ColinWelsh") {
  ##Goose - iMac
  setwd("~/Documents/dev/football")
  device <- "Goose (iMac)"
  ##add Goose
} 
print(paste(device, "is ready for some football", sep = " "))
rm(gid, device)


#   Formate today for latest scrape
today <- format(Sys.Date(), "%Y%d%m")


## Create a list of Game IDs
###############
season_state <- c("pre", "reg", "post")

all_game_ids <- list.files(paste("data/games/", season_state, "_season", sep = ""),
                           pattern = "*.csv", full.names = TRUE) %>%
  lapply(read_csv) %>% 
  bind_rows()

all_game_ids <- all_game_ids$game_id
all_game_ids <- all_game_ids[all_game_ids <= today]

###############


## Use Game IDs to get the 


## Create list of all possible files
all_files <- c()


## Work-in progress function
scrape_all_files <- function(x) {
  t.game_id <- x
  
  ## Postseason Fix:
  ## IF before March, Subtract 1 from the Game ID to get the year
  get_t.year <- function(y) {
    if(as.integer(substr(t.game_id, 5, 6)) < 3) {
      invisible(print(as.integer(substr(t.game_id, 1, 4)) - 1))
      }
    else {
      invisible(print(substr(t.game_id, 1, 4)))
      }
    }
  t.year <- get_t.year(t.game_id)
  
  ## Create Game and Player filepaths
  asset_types <- c("games", "players")
  game_assets <- function(z) {
    tf.pbp_file <- paste("data/", z, "/", t.year, "/", t.game_id, ".csv", sep = "")
    tf.player_file <- paste("data/", z, "/", t.year, "/", t.game_id, z, ".csv", sep = "")
    print(paste("Scraping", z , "data for", t.game_id, sep = " "))
    
    ## Play-by-play
    if (z == asset_types[1]) {
      ## scrape_game_play_by_play() filepath
      tf.file <- tf.pbp_file
      ## scrape
      if (file.exists(tf.file) == FALSE) {
        print(paste("Scraping", t.game_id, "play-by-play data", sep = " "))
        tf.df <- nflscrapR::scrape_json_play_by_play(t.game_id)
        print("Successfully scraped pbp!")
      }
    }
    ## Player Game
    else if (z == asset_types[2]) {
      ## player_game filepath
      tf.file <- tf.player_file
      if (file.exists(tf.file) == FALSE) {
        print(paste("Scraping", t.game_id, "player game data", sep = " "))
        tf.df <- nflscrapR::player_game(t.game_id)
        print("Successfully scraped player game!")
        
        ## Grab pbp to get other stats
        tg.pbp_file <- paste("data/", asset_types[1], "/", t.year, "/", t.game_id, ".csv", sep = "")
        tg.pbp_df <- read_csv(tg.pbp_file)
        print("Grabbed pbp to calculate targets")
        
        ## Calculate targets

        
        for (z1 in tg.receivers) {
          tf.df[tf.df$playerID == z1, "targets"] <- sum(targets$receiver_player_id == z1)
        }
        
        tf.df <- tf.df[,c(1:20,ncol(tf.df),c(21:(ncol(tf.df)-1)))]
        print("Successfully dded targets")
        
        ## target_function <- function(z1) {
        ##  tf.df[tf.df$playerID == z1, "targets"] <- sum(targets$Receiver_ID == z1)
        ## }
        ## tg.receivers %>% lapply(target_function)
      }
    }
    invisible(print(tf.file))
    write.csv(tf.df, file = tf.file, row.names = FALSE)
  }
  lapply(asset_types, game_assets)
  ## scrape_data <- unlist(lapply(asset_types, game_assets))
}

lapply(all_game_ids, scrape_all_files)

get_t.year <- function(x) {
  if(as.integer(substr(x, 5, 6)) < 3) {
    invisible(as.integer(substr(x, 1, 4)) - 1)
  }
  else {
    invisible(substr(x, 1, 4))
  }
}

name_all_files <- function(x) {
  t.year <- get_t.year(x)
  f.pbp_x <- paste("data/games/", t.year, "/", x, ".csv", sep = "")
  f.players_x <- paste("data/players/", t.year, "/", x, "players.csv", sep = "")
  all_files <- append(all_files, f.pbp_x)
  all_files <- append(all_files, f.players_x)
}

all_files <- all_game_ids %>% lapply(name_all_files)
all_files <- unlist(all_files)

scrape_all_files <- function(x) {
  if (file.exists(x) == TRUE) {
    print(paste("All", x, "files exist"))
  }
  if (file.exists(x) == FALSE) {
    ## tf.pbp_file <- paste("data/games/", t.year, "/", t.game_id, ".csv", sep = "")
    ## tf.player_file <- paste("data/players/", t.year, "/", t.game_id, "players.csv", sep = "")
    
    ## Game logic
    if (substr(x, 6, 6) == "g") {
      t.game_id <- x %>% substr(17, 26) %>% as.integer()
      t.year <- get_t.year(t.game_id)
      paste("Scraping game data for", t.game_id, sep = " ") %>% print()
      tf.df <- nflscrapR::scrape_json_play_by_play(t.game_id)
      print("Successfully scraped pbp!")
      }
    ## Player logic
    if (substr(x, 6, 6) == "p") {
      t.game_id <- x %>% substr(19, 28) %>% as.integer()
      tf.pbp_file <- paste("data/games/", get_t.year(t.game_id), "/", t.game_id, ".csv", sep = "")
      paste("Scraping player data for", t.game_id, sep = " ") %>% print()
      tf.df <- player_game(t.game_id)
      print("Successfully scraped player game!")
      targets <- read_csv(tf.pbp_file) %>% filter(play_type == "pass" & receiver_player_id != "NA")
      tf.targets <- tibble(
        playerID = unique(targets$receiver_player_id),
        targets = integer(length(unique(targets$receiver_player_id)))
      )
      add_targets <- function(x){
        tf.targets[tf.targets$playerID == x, "targets"] <- sum(targets$receiver_player_id == x)
      }
      tf.targets$playerID %>% lapply(add_targets)
      tf.targets$targets <- unlist(tf.targets$playerID %>% lapply(add_targets))
      tf.df <- dplyr::left_join(tf.df, tf.targets, by = "playerID")
      print("Successfully joined targets to player game!")
      }
    write.csv(tf.df, file = x, row.names = FALSE)
  }
}

lapply(all_files, scrape_all_files)


## Get list of existing files
all_years <- seq(2009, format(Sys.Date(), "%Y"))
get_all_files <- function(x) {
  asset_types <- c("games", "players")
  game_assets <- function(y) {
    list.files(paste("data/", y, "/", x, sep = ""),
               pattern = "*.csv", full.names = TRUE)
  }
  lapply(asset_types, game_assets)
}
list_existing_files <- all_years %>% lapply(get_all_files)
## Turn into vector
list_existing_files <- unlist(list_existing_files)
1
## Compare possible files to existing files
remaining_game_ids <- all_files %in%
  list_existing_files
if (any(remaining_game_ids == FALSE)) {
  list_existing_files[!all_files]
  scrape_game_ids(x)
  install.packages(pkgs[!installed_packages])
}

installed_packages <- pkgs %in%
  rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(pkgs[!installed_packages])
}
  
all_game_ids[length(all_game_ids)] %>% grepl(list_existing_files)