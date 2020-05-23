source('framework/data.R'); 
source('framework/backtester.R')
source('framework/processResults.R'); 
source('strategies/smi_ema_vwap.R') 

numOfDays <- 750
dataList <- getData(directory="PART1")
dataList <- lapply(dataList, function(x) x[1:numOfDays])
sMult <- 0.2 # slippage multiplier

lookbackSeq <- seq(from=16,to=26,by=2)
lookback1Seq <- seq(from=46,to=56,by=2)
paramsList  <- list(lookbackSeq,lookback1Seq)
numberComb <- prod(sapply(paramsList,length))

resultsMatrix <- matrix(nrow=numberComb,ncol=3)
colnames(resultsMatrix) <- c("lookback","lookback1","PD Ratio")
pfolioPnLList <- vector(mode="list",length=numberComb)

count <- 1
for (lb in lookbackSeq) {
  for (lb1 in lookback1Seq) {
    params <- list(lookback=lb,lookback1=lb1,lookback2=27,lookbackEMA=lb1,series=1:10,posSizes=rep(1,10)) 
    results <- backtest(dataList, getOrders, params, sMult)
    pfolioPnL <- plotResults(dataList,results)
    resultsMatrix[count,] <- c(lb,lb1,pfolioPnL$fitAgg)
    pfolioPnLList[[count]]<- pfolioPnL
    cat("Just completed",count,"out of",numberComb,"\n")
    print(resultsMatrix[count,])
    count <- count + 1
  }
}
print(resultsMatrix[order(resultsMatrix[,"PD Ratio"]),])