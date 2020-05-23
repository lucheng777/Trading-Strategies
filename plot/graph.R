setwd("E:/UoL Y3/Y3S1/COMP396/backtester_v5.1/backtester_v5.1")
library(ggplot2)
source('framework/data.R'); 

dataList1 <- getData("PART1")
dataList2 <- getData("PART2")

dataListSd <- matrix(0,10,1)
for (i in 1:10) {
  a <- sd(as.numeric(dataList[[i]][1:750]$Close))
  dataListSd[i,1] <- a
}

VolatilityList <- lapply(dataList[1:10], function(x) volatility(x[1:750]$Close,n=10))
VolatilityInSingleXts <- do.call(cbind,VolatilityList)
colnames(VolatilityInSingleXts) <- c(1:10)
v <- ggplot(fortify.zoo(VolatilityInSingleXts,melt=TRUE),aes(x=Index,y=Value,group=Series))+
    geom_line()+
    facet_wrap(~ Series, scales="free")

VolatilityList <- lapply(dataList[1:10], function(x) volatility(x[1:750]$Close,n=60))
VolatilityInSingleXts <- do.call(cbind,VolatilityList)
colnames(VolatilityInSingleXts) <- c(1:10)
v1 <- ggplot(fortify.zoo(VolatilityInSingleXts,melt=TRUE),aes(x=Index,y=Value,group=Series))+
      geom_line()+
      facet_wrap(~ Series, scales="free")

dataListCorrelation <- matrix(0,10,9)
for (i in 1:9) {
  for (j in (i+1):10) {
    a <- dataList[[i]]$Close
    b <- dataList[[j]]$Close
    c <- cor(a,b)
    dataListCorrelation[j,i] <- c
  }
}



justClosesList <- lapply(dataList1[1:10], function(x) x[1:750]$Close)
DiffLogList <- lapply(dataList1[1:10], function(x) diff(log(x[1:750]$Close))[-1,])
ClosesInSingleXts <- do.call(cbind, justClosesList)
colnames(ClosesInSingleXts) <- c(1:10)
q <- ggplot(fortify.zoo(ClosesInSingleXts,melt=TRUE),
            aes(x = Index, y = Value, group = Series))
q <- q + geom_line()
q <- q + facet_wrap(~ Series, scales="free")

DiffLogList <- lapply(dataList1[1:10], function(x) diff(log(x[1:750]$Close))[-1,])
DiffLogsInSingleXts <- do.call(cbind, DiffLogList)
colnames(DiffLogsInSingleXts) <- c(1:10)
j <- ggplot(fortify.zoo(DiffLogsInSingleXts,melt=TRUE),
            aes(x = Index, y = Value, group = Series))
j <- j + geom_line()
j <- j + facet_wrap(~ Series, scales="free")

d1 <- chartSeries(dataList[[1]][1:750]$Close, TA = NULL)
addRSI(n = 14, maType = EMA, wilder = TRUE)

#limit order
#slippage incurred if we buy only one unit
OpenAndCloseList <- lapply(dataList[1:10], function(x) cbind(x[1:750]$Close, x[2:751]$Open, x[2:751]$Open-x[1:750]$Close))
OpenAndCloseList1 <- lapply(dataList[1:10], function(x) x[2:751]$Open-x[1:750]$Close)
DiffInSingleXts <- do.call(cbind, OpenAndCloseList1)
colnames(DiffInSingleXts) <- c(1:10)
g <- ggplot(fortify.zoo(DiffInSingleXts,melt=TRUE),
            aes(x = Index, y = Value, group = Series))
g <- g + geom_line()
g <- g + facet_wrap(~ Series, scales="free")

HighAndLowList <- lapply(dataList[1:10], function(x) cbind(x[1:750]$High, x[1:750]$Low, x[1:750]$High-x[1:750]$Low))

dataListIndex <- lapply(dataList[1:10], function(x) fortify.zoo(x[1:750], melt=FALSE))
dataListIndex <- lapply(dataList[1:10], function(x) {bb20 <- BBands(x[1:750]$Close, sd = 2.0)
                                                     dataPlusBB <- data.frame(x[1:750],bb20)})
#dataListIndex <- lapply(dataList[1:10], function(x) {
# plot(x = dataPlusBB$Index, y = dataPlusBB$Close, type ='l')
# lines(dataPlusBB$up, col='purple')
# lines(dataPlusBB$dn, col='brown')
# lines(dataPlusBB$mavg, col='red')
#})

dataListIndex1 <- lapply(dataList[1:10], function(x) rsi14 <- RSI(x[1:750]$Close, n = 14))
RsiInSingleXts <- do.call(cbind,dataListIndex1)
colnames(RsiInSingleXts) <- c(1:10)
h <- ggplot(fortify.zoo(RsiInSingleXts,melt=TRUE),
            aes(x = Index, y = Value, group = Series))
h <- h + geom_line()
h <- h + facet_wrap(~ Series, scales="free")

dataListFortified <- lapply(dataList[1:10], function(x) fortify.zoo(x, melt=FALSE))
RsiInSingleXtsFortified <- fortify.zoo(RsiInSingleXts, melt=FALSE)
prob <- matrix(0,10,2)
for (i in 1:10) {
  k <- cbind(dataListFortified[[i]][1:749,5],dataListFortified[[i]][2:750,5],RsiInSingleXtsFortified[1:749,i+1])
  k <- cbind(k,k[,2]-k[,1])
  k1 <- k[which(k[,3]<=25),]
  k2 <- k[which(k[,3]>=75),]
  if (class(k1) == "matrix" && class(k2) == "matrix") {
    kN <- k1[,4] > 0
    kN1 <- k2[,4] < 0
    kN <- as.numeric(kN)
    kN1 <- as.numeric(kN1)
    prob[i,1] <- sum(kN)/length(kN)
    prob[i,2] <- sum(kN1)/length(kN1)
  } else {
    kN <- k1[4] > 0
    kN1 <- k2[,4] < 0
    kN <- as.numeric(kN)
    kN1 <- as.numeric(kN1)
    prob[i,1] <- kN
    prob[i,2] <- sum(kN1)/length(kN1)
  }
}

prob1 <- matrix(0,10,1)
for (i in 1:10) {
  k1 <- cbind(dataListFortified[[i]][1:741,5],dataListFortified[[i]][2:742,5],
              dataListFortified[[i]][3:743,5],dataListFortified[[i]][4:744,5],
              dataListFortified[[i]][5:745,5],dataListFortified[[i]][6:746,5],
              dataListFortified[[i]][7:747,5],dataListFortified[[i]][8:748,5],
              dataListFortified[[i]][9:749,5],dataListFortified[[i]][10:750,5],
              RsiInSingleXtsFortified[1:741,i+1],rep(0,741))
  k1 <- k1[which(k1[,11]>=75|k1[,11]<=25),]
  for (j in 1:nrow(k1)) {
    k1[j,12] <- mean(as.numeric(k1[j,1:10]))
  }
  kN_1 <- (k1[,11] <= 25 & k1[,12] > k1[,1]) | (k1[,11] >= 75 & k1[,12] < k1[,1])
  prob1[i,1] <- sum(kN_1)/length(kN_1)
}

run <- function(prices,n,sd,stoploss) {
  bbands <- lag(BBands(prices,n=n,sd=sd))
  pos <- stopOuts <- rep(0,length=nrow(prices)) # all zeroes
  for (i in (n+1):nrow(prices)) {
    if (pos[i-1]==0) { # flat
      long <- ifelse(prices[i]<bbands$dn,1,0)
      short <- ifelse(prices[i]>bbands$up,-1,0)
      pos[i] <- long + short
      if (pos[i] != pos[i-1]) entry <- i # remember entry period
    } else {
      ret <- getTradeReturn(prices,entry,exit=i,isTRUE(pos[entry]<0))
      if (ret > -stoploss) pos[i] <- pos[i-1] # stay in trade
      else stopOuts[i] = 1 # record stopout
    }
  }
  return(pos)
}

getTradeReturn <- function(prices,entry,exit,short=FALSE) {
  prices <- as.numeric(prices)
  if (short)
    prices[entry]/prices[exit] - 1
  else
    prices[exit]/prices[entry] - 1
}