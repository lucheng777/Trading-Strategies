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
  
  if (store$iter > as.numeric(params$lookback)) {
    startIndex <-  store$iter - as.numeric(params$lookback)
    for (i in 1:length(params$series)) {
      cl <- newRowList[[params$series[i]]]$Close
      high <- newRowList[[params$series[i]]]$High
      low <- newRowList[[params$series[i]]]$Low
      
      h <- max(store$hi[startIndex:store$iter,i])
      l <- min(store$lo[startIndex:store$iter,i])
      t <- (store$cl[store$iter,i]-h)/(h-l)
      
      rsi <- last(RSI(store$cl[startIndex:store$iter]),n=params$lookback)
      rsi <- as.numeric(rsi)
      
      if (t < -0.8 && rsi < 30) {
        
        pos[params$series[i]] <- params$posSizes[params$series[i]]
      } else if (t > -0.2 && rsi > 70) {
        
        pos[params$series[i]] <- -params$posSizes[params$series[i]]
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
#high INITIAL
initHiStore  <- function(newRowList,series) {
  hiStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(hiStore)
}
#low INITIAL
initLoStore  <- function(newRowList,series) {
  loStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(loStore)
}
updateClStore <- function(clStore, newRowList, series, iter) {
  for (i in 1:length(series))
    clStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Close)
  return(clStore)
}
updateHiStore <- function(hiStore, newRowList, series, iter) {
  for (i in 1:length(series))
    hiStore[iter,i] <- as.numeric(newRowList[[series[i]]]$High)
  return(hiStore)
}
updateLoStore <- function(loStore, newRowList, series, iter) {
  for (i in 1:length(series))
    loStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Low)
  return(loStore)
}
initStore <- function(newRowList,series) {
  return(list(iter=0,cl=initClStore(newRowList,series),
                     hi=initHiStore(newRowList,series),
                     lo=initLoStore(newRowList,series)))
}
updateStore <- function(store, newRowList, series) {
  store$iter <- store$iter + 1
  store$cl <- updateClStore(store$cl,newRowList,series,store$iter)
  store$hi <- updateHiStore(store$hi,newRowList,series,store$iter)
  store$lo <- updateLoStore(store$lo,newRowList,series,store$iter)
  return(store)
}