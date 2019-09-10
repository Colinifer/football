##install.packages("devtools", "tidyverse", "readr")
##devtools::install_github(repo = "maksimhorowitz/nflscrapR")

library(nflscrapR)
library(tidyverse)
library(readr)

##Likely directories
id <- "~/"
wd <- "/Users/colinwelsh/Documents/dev/football/"

setwd(id)
setwd(wd)

getwd()
rm(wd, id)
print("Ready for some football")