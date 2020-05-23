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
  
  if (store$iter > as.numeric(params$lookback1)) {
    size <- getPosition(store$op, store$iter)
    
    for (i in 1:length(params$series)) {
      #cl <- store$cl[(store$iter-d):store$iter, i]
      cl <- store$cl[[i]]
      #vol <- newRowList[[params$series[i]]]$Volume
      #startIndex <-  store$iter - as.numeric(params$lookback1)
      
      diff_log_return <- diff(log(cl))
      #diff_log_return[1,] <- 0
      diff_log_return <- tail(diff_log_return, 50)
      #diff_log_return <- diff_log_return[-1,]
      #diff_log_return <- diff_log_return[-1,]
      
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
        pos[params$series[i]] <- size[i]*10#params$posSizes[i]
      } else {
        fore = ugarchforecast(fit, n.ahead=1)
        ind = fore@forecast$seriesFor
        #forecasts[d+1] = paste(colnames(ind), ifelse(ind[1]u < 0, -1, 1), sep=",")
        pos[params$series[i]] <- ifelse(ind[1] < 0, -size[i]*10, size[i]*10)
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

initOpStore  <- function(newRowList,series) {
  opStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(opStore)
}
updateOpStore <- function(opStore, newRowList, series, iter) {
  for (i in 1:length(series))
    opStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Open)
  return(opStore)
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
  return(list(iter=0,op=initOpStore(newRowList,series),
              cl=initClStore(newRowList,series),
              vol=initVolStore(newRowList,series)))
}
updateStore <- function(store, newRowList, series) {
  store$iter <- store$iter + 1
  store$op <- updateOpStore(store$cl,newRowList,series,store$iter)
  store$cl <- updateClStore(store$cl,newRowList,series,store$iter)
  store$vol <- updateVolStore(store$vol,newRowList,series,store$iter)
  return(store)
}