# PARAMS <- list(lookback=20,lookback1=50,lookback2=27,lookbackEMA=50,series=c(1:10))
library(ggplot2)
library(PerformanceAnalytics)
library(forecast)
library(tseries)
library(rugarch)

maxRows <- 3100 # used to initialize a matrix to store closing prices
stoploss <- 0.05

getOrders <- function(store, newRowList, currentPos, info, params) {
  
  allzero  <- rep(0,length(newRowList)) # used for initializing vectors
  
  if (is.null(store)) store <- initStore(newRowList,params$series)
  store <- updateStore(store, newRowList, params$series)
  marketOrders <- -currentPos; pos <- allzero

  if (store$iter > params$lookback1) {
    
    startIndex <-  store$iter - params$lookback
    startIndex1 <-  store$iter - params$lookback1
    startIndex2 <-  store$iter - params$lookback2
    startIndexEMA <- store$iter - params$lookbackEMA
    
    for (i in 1:length(params$series)) {
      
      cl <- store$cl[store$iter, i]
      vol <- store$vol[store$iter, i]
      size <- getPosition(store$op, store$iter)
      vola <- AnalyzeVol(store$op,store$hi,store$lo,store$cl,i,store$iter)
      
      if (i == 1 || i == 2 || i == 3 || i == 4 || i == 8 || i == 9) {
        diff_log_return <- diff(log(cl))
        diff_log_return <- tail(diff_log_return, 50)
        #diff_log_return <- diff_log_return[-1,]
        #diff_log-return <- diff_log_return[startIndex:store$iter]
        
        final.aic <- Inf
        final.order <- c(0,0,0)
        for (p in 0:5) for (q in 0:5) {
          if ( p == 0 && q == 0) {
            next
          }
          
          arimaFit = tryCatch( arima(diff_log_return, order=c(p, 0, q)),
                               error=function( err ) FALSE,
                               warning=function( err ) FALSE )
          
          if( !is.logical( arimaFit ) ) {
            current.aic <- AIC(arimaFit)
            if (current.aic < final.aic) {
              final.aic <- current.aic
              final.order <- c(p, 0, q)
              final.arima <- arima(diff_log_return, order=final.order)
            }
          } else {
            next
          }
        }
        
        spec = ugarchspec(
          variance.model=list(garchOrder=c(1,1)),
          mean.model=list(armaOrder=c(final.order[1], final.order[3]), include.mean=T),
          distribution.model="sged"
        )
        fit = tryCatch(
          ugarchfit(
            spec, diff_log_return, solver = 'hybrid'
          ), error=function(e) e, warning=function(w) w
        )
        
        if(is(fit, "warning")) {
          #forecasts[d+1] = paste(index(spReturnsOffset[windowLength]), 1, sep=",")
          pos[params$series[i]] <- size[i]*10
        } else {
          fore = ugarchforecast(fit, n.ahead=1)
          ind = fore@forecast$seriesFor
          #forecasts[d+1] = paste(colnames(ind), ifelse(ind[1] < 0, -1, 1), sep=",")
          pos[params$series[i]] <- ifelse(ind[1] < 0, -size[i]*10, size[i]*10)
        }
      } else {
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
          pos[params$series[i]] <- size[i]*10
        }
        if (cl< SMA20 && cl > VWAP && cl < EMA20 || SMA20 < SMA50 || EMA20 < EMA50 && cl < EMA20) {  #      SELL  
          pos[params$series[i]] <- -size[i]*10
        }
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

#Analyze volatility
AnalyzeVol<- function(opStore,hiStore,loStore,clStore,column,iter){
  startIndex <- iter - 50
  open <- opStore[startIndex:iter,column]
  high <- hiStore[startIndex:iter,column]
  low <- loStore[startIndex:iter,column]
  close <- clStore[startIndex:iter,column]
  dataStore <- cbind(open,high,low,close)
  movingVol <-last(volatility(dataStore,n=50))
  return(movingVol)
}

#Position Sizing
getPosition <- function(op,iter){
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

# functions of store

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

#low
initLoStore  <- function(newRowList,series) {
  loStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(loStore)
}
updateLoStore <- function(loStore, newRowList, series, iter) {
  for (i in 1:length(series))
    loStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Low)
  return(loStore)
}
#high
initHiStore  <- function(newRowList,series) {
  hiStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(hiStore)
}
updateHiStore <- function(hiStore, newRowList, series, iter) {
  for (i in 1:length(series))
    hiStore[iter,i] <- as.numeric(newRowList[[series[i]]]$High)
  return(hiStore)
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
                     op=initOpStore(newRowList,series),
                     lo=initLoStore(newRowList,series),
                     hi=initHiStore(newRowList,series)))
}
updateStore <- function(store, newRowList, series) {
  store$iter <- store$iter + 1
  store$op <- updateOpStore(store$op,newRowList,series,store$iter)
  store$cl <- updateClStore(store$cl,newRowList,series,store$iter)
  store$lo <- updateLoStore(store$lo,newRowList,series,store$iter)
  store$hi <- updateHiStore(store$hi,newRowList,series,store$iter)
  store$vol <- updateVolStore(store$vol,newRowList,series,store$iter)
  return(store)
}