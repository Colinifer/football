# Packages & Init Setup ---------------------------------------------------

# devtools::install_github("mrcaseb/nflfastR")
# devtools::install_github("dynastyprocess/ffscrapr")
proj_name <- "football"
pkgs <- c(
  "devtools",
  "tidyverse",
  "DBI",
  "odbc",
  "RMariaDB",
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
source("../initR/con.R")
dbListTables(con)
dbDisconnect(con)

swid  <-  "{2BA315B4-5941-4B1C-A315-B459416B1CC1}"
espn_s2 <- "AEBtGuDXUCKk6SpqlY71qdBDW%2BYc5KGa80m%2F0EVX9NCF%2FIFBM5b8ZMKgrMovpUeUqFTp4M%2BrPbM1I4rT1Ra2oXbM847nUp25DBY9Q%2FsAPChAykF5VNEZ05VjF6Vu3thAU0WkzQeBbjkdzNGqfbmPtMNzrBy8oV7fcAlwh4X89q4XlfPNED8ppKynNj5admyBk7WaqNzQtZJLlStpyOjz3F3d5BwUtQ8kh390OPB5HEEPfiH4%2FBftKqsLF%2BlyhTFaDiM%3D"
leagueID <- "1034400"
