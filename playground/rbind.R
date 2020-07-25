file_names <- dir("data/games_data/2019") #where you have your files

wd <- getwd()
wd2 <- "data/games_data/2019"
setwd(wd2)

pbp2019 <- do.call(rbind(deparse.level = 1),lapply(file_names,read.csv))

pbp2019 = ldply(file_names, read_csv)
pbp2019

# dat_txt = ldply(myfiles, read.table, sep = "\t", fill=TRUE, header = TRUE)

setwd(wd)