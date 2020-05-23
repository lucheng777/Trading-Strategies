setwd("E:/UoL Y3/Y3S1/COMP396/backtester_v5.1/backtester_v5.1")
library(ggplot2)
source('framework/data.R');

dataList <- getData("PART1")

for (i in c(1,150,300,450,600)) {
  high<-dataList[[2]][i:(i+150),"High"]
  low<-dataList[[2]][i:(i+150),"Low"]
  close<-dataList[[2]][i:(i+150),"Close"]
  open<-dataList[[2]][i:(i+150),"Open"]
  dataStore<-cbind(high,low,close,open)

  col1 <- "red"  
  
  chartSeries(dataStore, theme="white",TA="addEMA(n=14,col=col1);addEMA(n=30);addRSI(n=7)")
}