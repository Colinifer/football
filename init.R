##install.packages("devtools", "tidyverse", "readr", "pander", "na.tools", "ggimage", "devtools")
##devtools::install_github(repo = "maksimhorowitz/nflscrapR")

library(nflscrapR)
library(tidyverse)
library(readr)
library(pander)
library(dplyr)
library(na.tools)
library(ggimage)


gid <- paste(getwd())

if (gid == "/Volumes/HDD/Users/colinwelsh") {
  ##Maverick - MBP
  setwd("~/Documents/dev/football")
  print("Maverick is ready for some football")
} else if (gid == "") {
  ##Goose - iMac
  
  ##add Goose
}