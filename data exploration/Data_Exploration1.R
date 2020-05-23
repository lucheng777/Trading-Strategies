library(ggplot2)
source('framework/data.R');

dataList <- getData("PART1")

cl <- dataList[[1]]$Close[1:100]
op <- dataList[[1]]$Open[1:100]
hi <- dataList[[1]]$High[1:100]
lo <- dataList[[1]]$Low[1:100]
dataStore <- cbind(op,hi,lo,cl)

clFortified <- fortify.zoo(cl, melt = FALSE)
cl_graph <- ggplot(clFortified, aes(x = Index, y = Close))+
                  geom_line()

rsi14 <- RSI(clFortified$Close, n=14)
clFortified_RSI <- cbind(clFortified, rsi14)

EMA20 <- EMA(clFortified$Close, n=20)
clFortified_EMA_RSI <- cbind(clFortified_RSI, EMA20)

macd <- MACD(cl$Close, nFast=12, nSlow=26, nSig=9, maType=EMA)

clFortified_EMA_RSI1 <- clFortified_EMA_RSI[1,]
clFortified_EMA_RSI2 <- clFortified_EMA_RSI[1,]

j <- 1
k <- 1
for (i in 1:100) {
  if (clFortified_EMA_RSI[i,2] == cummax(clFortified_EMA_RSI$Close)[i]) {
    clFortified_EMA_RSI1[j,] = clFortified_EMA_RSI[i,]
    j <- j+1
  } else if (clFortified_EMA_RSI[i,2] == cummin(clFortified_EMA_RSI$Close)[i]) {
    clFortified_EMA_RSI2[k,] = clFortified_EMA_RSI[i,]
    k <- k+1
  }
}

RSI1 <- clFortified_EMA_RSI1[,3]
RSI1 <- RSI1[which(!is.na(RSI1))]
RSI2 <- clFortified_EMA_RSI2[,3]
RSI2 <- RSI2[which(!is.na(RSI2))]

chartSeries(dataStore, theme = "white", TA = "addRSI();addMACD()")
for (i in 1:nrow(clFortified_EMA_RSI1)) {
  abline(v = as.Date(clFortified_EMA_RSI1[i,1]), col='red')
}

for (i in 1:nrow(clFortified_EMA_RSI2)) {
  abline(v = as.Date(clFortified_EMA_RSI2[i,1]), col='purple')
}

