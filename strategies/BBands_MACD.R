maxRows <- 3100 # used to initialize a matrix to store closing prices
# set maxRows as the number of rows in data (it can be larger but should not be smaller)

getOrders <- function(store, newRowList, currentPos, info, params) {
  
  allzero  <- rep(0,length(newRowList)) # used for initializing vectors
  
  if (is.null(store)) {
    store <- initStore(newRowList,params$series)
  } else {
    store <- updateStore(store, newRowList, params$series)
  }
  
  marketOrders <- -currentPos; pos <- allzero
  
  if (store$iter > as.numeric(params$lookback[1])) {
    startIndex <-  store$iter - as.numeric(params$lookback[1])
    for (i in 1:length(params$series)) {
      cl <- newRowList[[params$series[i]]]$Close
      vol <- newRowList[[params$series[i]]]$Volume
      lastMacd <- last(MACD(store$cl[startIndex:store$iter,i],as.numeric(params$lookback[2]),
                            as.numeric(params$lookback[3]),as.numeric(params$lookback[4]),maType="EMA"))
      lastMacd <- as.numeric(lastMacd)
      lastDiff <- lastMacd[1] - lastMacd[2]
      beforeMacd <- last(MACD(store$cl[startIndex:(store$iter-1),i],as.numeric(params$lookback[2]),
                              as.numeric(params$lookback[3]),as.numeric(params$lookback[4]),maType="EMA"))
      beforeMacd <- as.numeric(beforeMacd)
      beforeDiff <- beforeMacd[1] - beforeMacd[2]
      
      if (beforeDiff > 0 && lastDiff > 0) {
        pos[params$series[i]] <- params$posSizes[params$series[i]]
      } else if (beforeDiff < 0 && lastDiff < 0) {
        pos[params$series[i]] <- -params$posSizes[params$series[i]]
      } else if (lastDiff==0) {
        if (beforeDiff < 0) {
          pos[params$series[i]] <- params$posSizes[params$series[i]]
        } else {
          pos[params$series[i]] <- -params$posSizes[params$series[i]]
        }
      }
    }
  }
  marketOrders <- marketOrders + pos
  
  #limitOrders1 <- rep(1,length(newRowList))
  #limitPrices1 <- sapply(1:length(newRowList), function(i) newRowList[[i]]$Close - spread[i]/2)
  
  #limitOrders2 <- rep(-1,length(newRowList))
  #limitPrices2 <- sapply(1:length(newRowList), function(i) newRowList[[i]]$Close + spread[i]/2)
  
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=allzero,limitPrices1=allzero,
              limitOrders2=allzero,limitPrices2=allzero))
}
initClStore  <- function(newRowList,series) {
  clStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(clStore)
}
initVolStore  <- function(newRowList,series) {
  volStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(volStore)
}
updateClStore <- function(clStore, newRowList, series, iter) {
  for (i in 1:length(series))
    clStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Close)
  return(clStore)
}
updateVolStore <- function(volStore, newRowList, series, iter) {
  for (i in 1:length(series))
    volStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Volume)
  return(volStore)
}
initStore <- function(newRowList,series) {
  return(list(iter=0,cl=initClStore(newRowList,series),
                     vol=initVolStore(newRowList,series)))
}
updateStore <- function(store, newRowList, series) {
  store$iter <- store$iter + 1
  store$cl <- updateClStore(store$cl,newRowList,series,store$iter)
  store$vol <- updateVolStore(store$vol,newRowList,series,store$iter)
  return(store)
}