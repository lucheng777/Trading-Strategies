setwd("E:/UoL Y3/Y3S1/COMP396/backtester_v5.1/backtester_v5.1")
library(ggplot2)
source('framework/data.R');

dataList <- getData("PART1")

high<-dataList[[6]][200:400,"High"] #[[3]][50:150] [[6]][200:500] 
low<-dataList[[6]][200:400,"Low"]
close<-dataList[[6]][200:400,"Close"]
open<-dataList[[6]][200:400,"Open"]
value<-dataList[[6]][200:400,"Volume"]
dataStore<-cbind(high,low,close,open)

chartSeries(dataStore, theme="white")
addSMI(13,25,2,9,"EMA")
