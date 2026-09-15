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
all_files <- c()


## Solved postseason year filepath bug
###############

get_t.year <- function(x) {
  if(as.integer(substr(x, 5, 6)) < 3) {
    invisible(as.integer(substr(x, 1, 4)) - 1)
  }
  else {
    invisible(substr(x, 1, 4))
  }
}


## Create a list of all files
###############

name_all_files <- function(x) {
  t.year <- get_t.year(x)
  f.pbp_x <- paste("data/games/", t.year, "/", x, ".csv", sep = "")
  f.players_x <- paste("data/players/", t.year, "/", x, "players.csv", sep = "")
  all_files <- append(all_files, f.pbp_x)
  all_files <- append(all_files, f.players_x)
}

all_files <- all_game_ids %>% lapply(name_all_files)
all_files <- unlist(all_files)


## Scrape all files
###############

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