##install.packages("devtools", "tidyverse", "readr")
##devtools::install_github(repo = "maksimhorowitz/nflscrapR")

library(nflscrapR)
library(tidyverse)
library(readr)

#setwd
if (paste(getwd()) == "/Volumes/HDD/Users/colinwelsh/" | paste(getwd()) == "Users/colinwelsh/"){
  ##Laptop
  if (getwd() == "/Volumes/HDD/Users/colinwelsh/Documents/dev/") {
    setwd("/Volumes/HDD/Users/colinwelsh/Documents/dev/")
    print("Initialized on Maverick")
  }
  ##iMac
  if (getwd() == "Users/colinwelsh/Documents/dev/") {
    setwd("Users/colinwelsh/Documents/dev/")
    print("Initialized on Goose")
  }
} else if (paste(getwd()) == "/Volumes/HDD/Users/colinwelsh/Documents/dev/football" | paste(getwd()) == "Users/colinwelsh/Documents/dev/football"){
  print("Already initialized")
} else (print("We dont know where you are"))
