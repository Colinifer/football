getPlayersFromSleeper <- function(){
  nflPlayers <- jsonlite::fromJSON("https://api.sleeper.app/v1/players/nfl", flatten = TRUE)
  #nflPlayers[[1]]
  nflPlayersRows <- names(nflPlayers)
  nflPlayerCols <- character()
  for(nR in nflPlayersRows){
    nflColNames <- names(nflPlayers[[nR]])
    nflPlayerCols <- c(nflPlayerCols, nflColNames[!(nflColNames %in% nflPlayerCols)])
  }
  pL <- unlist(nflPlayers[["4034"]])
  pL1 <- unlist(nflPlayers[["3198"]])
  pL <- rbind(pL,pL1)
  pL2 <- as.data.frame(pL, stringsAsFactors=FALSE)
  for(i in 1:ncol(pL2)){#i=1
    if(!is.na(suppressWarnings(any(as.numeric(pL2[,i]))))) pL2[,i] <- as.numeric(pL2[,i])
  }
  for(nC in nflPlayerCols){
    if(!(nC %in% colnames(pL2))){
      pL2[,nC] <- NA
    }
  }
  allPlayers <- pL2[0,]
  for(nI in 1:length(nflPlayersRows)){#nI=1
    nP <- nflPlayersRows[nI]
    pL <- unlist(nflPlayers[[nP]])
    for(i in 1:ncol(allPlayers)){#i=1
      cName <- colnames(allPlayers)[i]
      if(cName %in% names(pL)){
        if(is.na(suppressWarnings(as.numeric(pL[cName])))){
          allPlayers[nI,i] <- pL[cName]
        }else{
          allPlayers[nI,i] <- as.numeric(pL[cName])
        }
      }
    }
  }
  #sPlayers <- correctSleeperNames(allPlayers)
  sPlayers <- allPlayers#[!is.na(allPlayers$position) & !is.na(allPlayers$team),]
  sPlayers$name <- paste(sPlayers$first_name,sPlayers$last_name)#x=1
  sPlayers <- correctSleeperNames(sPlayers)
  sPlayers <- updateTeamNames(sPlayers)
  if(nrow(sPlayers) > 0){
    sPlayers$pId <- sapply(1:nrow(sPlayers), function(x){#x=1
      if(!is.na(sPlayers[x,'position']) && sPlayers[x,'position'] == "DEF"){
        paste(sPlayers[x,'last_name'], sPlayers[x,'team'], "DST", sep="|")
      }else{
        paste(sPlayers[x,'name'], sPlayers[x,'team'], sPlayers[x,'position'], sep="|")
      }
    })
  }
  return(sPlayers)
}