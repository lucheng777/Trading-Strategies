setwd("E:/UoL Y3/Y3S1/COMP396/backtester_v5.1/backtester_v5.1")
library(ggplot2)
source('framework/data.R');

dataList <- getData("PART1")

OBV_prob <- matrix(0,10,1)

for (i in c(1,150,300,450,600)) {
  high<-dataList[[3]][i:(i+150),"High"] #[[1]][1:100] [[3]][50:150] [[4]][450:550]
  low<-dataList[[3]][i:(i+150),"Low"]
  close<-dataList[[3]][i:(i+150),"Close"]
  open<-dataList[[3]][i:(i+150),"Open"]
  volume <- dataList[[3]][i:(i+150),"Volume"]
  dataStore<-cbind(high,low,close,open,volume)
  
  chartSeries(dataStore,theme="white")
}