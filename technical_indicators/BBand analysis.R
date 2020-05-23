setwd("E:/UoL Y3/Y3S1/COMP396/backtester_v5.1/backtester_v5.1")
library(ggplot2)
source('framework/data.R');

dataList <- getData("PART1")

high<-dataList[[3]][50:150,"High"] #[[3]][50:150] [[5]][1:500] 
low<-dataList[[3]][50:150,"Low"]
close<-dataList[[3]][50:150,"Close"]
open<-dataList[[3]][50:150,"Open"]
dataStore<-cbind(high,low,close,open)

chartSeries(dataStore, theme="white")
addBBands(n=20,sd=1,"SMA")
#addMACD(12,26,9,type="EMA")
#addRSI(n=14,maType="EMA",wilder=TRUE)