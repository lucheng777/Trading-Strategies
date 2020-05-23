###### STRATEGY USING SMA, EMA and VWAP
#     BUY IF;
# SMA18 < current price && current price < VWAP
# EMA18 > EMA50 and the close price is above the 18 and 50 day EMAs
# SMA18 is above SMA50
#     SELL IF;
# SMA18 > current price && current price > VWAP
# EMA18 < EMA50 and the close price is below the 18 and 50 day EMAs
# SMA18 is below SMA 50

# PARAMS "SMA_EMA_VWAP"=list(lookback=18,lookback1=50,lookback2=27,lookbackEMA=50,series=c(2,6,8),posSizes=rep(1,10)),

maxRows <- 3100 # used to initialize a matrix to store closing prices

getOrders <- function(store, newRowList, currentPos, info, params) {
  
  allzero  <- rep(0,length(newRowList)) # used for initializing vectors
  
  if (is.null(store)) store <- initStore(newRowList,params$series)
  store <- updateStore(store, newRowList, params$series)
  marketOrders <- allzero
  marketOrders <- -currentPos 
  pos <- allzero

  if (store$iter > params$lookback1) {
    
    startIndex <-  store$iter - params$lookback
    startIndex1 <-  store$iter - params$lookback1
    startIndex2 <-  store$iter - params$lookback2
    startIndexEMA <- store$iter - params$lookbackEMA
    size <- getPosition(store$op, store$iter)
    
    for (i in 1:length(params$series)) {
      
      cl <- store$cl[store$iter, i]
      vol <- store$vol[store$iter, i]
      
      # sma
      SMA20 <- last(SMA(store$cl[startIndex:store$iter,i], params$lookback))
      SMA50 <- last(SMA(store$cl[startIndex1:store$iter,i], params$lookback1))
      
      # ema
      EMA20 <- last(EMA(store$cl[startIndex:store$iter,i], params$lookback)) 
      EMA50 <- last(EMA(store$cl[startIndexEMA:store$iter,i],params$lookbackEMA)) 

      # vwap
      VWAP <- last(VWAP(store$cl[startIndex2:store$iter,i], store$vol[startIndex2:store$iter,i], params$lookback2))
      
      # Condition
      if (cl> SMA20 && cl < VWAP && cl > EMA20 || SMA20 > SMA50 || EMA20 > EMA50 && cl > EMA20) { #     BUY 
          pos[params$series[i]] <- size[i] * 10#params$posSizes[i]
      }
      if (cl< SMA20 && cl > VWAP && cl < EMA20 || SMA20 < SMA50 || EMA20 < EMA50 && cl < EMA20) { #      SELL  
        pos[params$series[i]] <- -size[i] * 10#-params$posSizes[i]
      }
    }
  }
  marketOrders <- marketOrders + pos
  
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=allzero,
              limitPrices1=allzero,
              limitOrders2=allzero,
              limitPrices2=allzero))
  
  return(store)
}

getTradeReturn <- function(prices, entry, exit) {
  prices <- as.numeric(prices)
  prices[exit]/price[entry] - 1
}

#Position Sizing
getPosition<- function(op,iter){
  opens<-op[1:iter,]
  openList<-split(opens, rep(1:ncol(opens), each = nrow(opens)))
  openDiffs <- lapply(openList,function(x) diff(x))
  absOpenDiffs    <- lapply(openDiffs,abs)
  avgAbsDiffs <- sapply(absOpenDiffs,mean,na.rm=TRUE)
  largestAvgAbsDiffs <- max(avgAbsDiffs)
  positionSizes <- largestAvgAbsDiffs/avgAbsDiffs
  params<- list(sizes=positionSizes)
  return(positionSizes)
}

# functions for managing the store

#Open
initOpStore  <- function(newRowList,series) {
  opStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(opStore)
}
updateOpStore <- function(opStore, newRowList, series, iter) {
  for (i in 1:length(series))
    opStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Open)
  return(opStore)
}

#close
initClStore  <- function(newRowList,series) {
  clStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(clStore)
}
updateClStore <- function(clStore, newRowList, series, iter) {
  for (i in 1:length(series))
    clStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Close)
  return(clStore)
}

#volume
initVolStore  <- function(newRowList,series) {
  volStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(volStore)
}
updateVolStore <- function(volStore, newRowList, series, iter) {
  for (i in 1:length(series))
    volStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Volume)
  return(volStore)
}

initStore <- function(newRowList,series) {
  return(list(iter=0,cl=initClStore(newRowList,series),
                     vol=initVolStore(newRowList,series),
                     op=initOpStore(newRowList,series)))
}
updateStore <- function(store, newRowList, series) {
  store$iter <- store$iter + 1
  store$op <- updateOpStore(store$op,newRowList,series,store$iter)
  store$cl <- updateClStore(store$cl,newRowList,series,store$iter)
  store$vol <- updateVolStore(store$vol,newRowList,series,store$iter)
  return(store)
}