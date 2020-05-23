setwd("E:/UoL Y3/Y3S1/COMP396/backtester_v5.2/backtester_v5.2")
library(ggplot2)
library(PerformanceAnalytics)
library(forecast)
library(tseries)
library(rugarch)
source('framework/data.R')
Sys.setlocale("LC_ALL","English")

dataList1All <- getData("PART1")
dataList2All <- getData("PART2")

simple_diff <- diff(dataList1All[[1]][1:750]$Close)
simple_diff <- simple_diff[-1,]

simple_return <- ROC(dataList2All[[1]][1:750]$Close, type = 'discrete')
simple_return <- simple_return[-1,]
plot(simple_return)
hist(simple_return)

simple_return_mean <-mean(simple_return)
simple_return_std <- sd(simple_return)
cum_simple_return <- cumprod(1 + simple_return) - 1

diff_log_return <- diff(log(dataList1All[[2]][1:750,]$Close))
diff_log_return <- diff_log_return[-1,]

ts2final.aic <- Inf
ts2final.order <- c(0,0,0)

for (p in 1:4) for (q in 1:4) {
  ts2current.aic <- AIC(arima(diff_log_return, order=c(p, 0, q)))
    if (ts2current.aic < ts2final.aic) {
      ts2final.aic <- ts2current.aic
      ts2final.order <- c(p, 0, q)
      ts2final.arima <- arima(diff_log_return, order=ts2final.order)
    }
}
acf(resid(ts2final.arima), na.action=na.omit)
Box.test(resid(ts2final.arima), lag=20, type="Ljung-Box")
plot(forecast(ts2final.arima, h=25))
ft <- as.numeric(diff_log_return)
ft <- ft[!is.na(ft)]
ft.garch <- garch(ft, trace=F)
ft.res <- ft.garch$res[-1]
acf(ft.res)
acf(ft.res^2)

plot(log_return)
hist(log_return)

log_return_mean <-mean(log_return)
log_return_std <- sd(log_return)
cum_log_return <- cumsum(log_return)

chart.Drawdown(simple_return)

justClosesList <- lapply(dataList1All[1:10], function(x) x[1:750]$Close)
ReturnList <- lapply(justClosesList, function(x) ROC(x, type = 'discrete'))
ReturnList <- lapply(ReturnList, function(x) x[-1,])
ReturnInSingleXts <- do.call(cbind, ReturnList)
colnames(ReturnInSingleXts) <- c(1,10)
return_graph <- ggplot(fortify.zoo(ReturnInSingleXts,melt=TRUE),
            aes(x = Index, y = Value, group = Series))
return_graph <- return_graph + geom_line()
return_graph <- return_graph + facet_wrap(~ Series, scales="free")