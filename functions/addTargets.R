addTargets <- function(x) {
  ## create targets dataframe
  targets <- filter(xpbp, PlayType == "Pass" & Receiver != "NA")
  xreceivers <- unique(targets$Receiver_ID)
  
  ## add targets to stats
  for (z in xreceivers) {
    xplayers[xplayers$playerID == z, "targets"] <- sum(targets$Receiver_ID == z)
  }
  
  ## bring targets column next to the receptions
  xplayers <- xplayers[,c(1:20,ncol(xplayers),22:ncol(xplayers)-1)]
  write.csv(xplayers, fplayers, row.names = FALSE)
}