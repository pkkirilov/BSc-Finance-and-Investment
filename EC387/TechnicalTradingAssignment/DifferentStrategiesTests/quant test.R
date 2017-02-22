
## ----echo=FALSE----------------------------------------------------------
#########################################################################
# Copyright (C) 2011-2014 Guy Yollin                                    #
# License: http://www.gnu.org/licenses/gpl.html GPL version 2 or higher #
#########################################################################



## ----results='hide'------------------------------------------------------
library(quantstrat)
startDate <- '2006-11-30'  # start of data
endDate <-  '2016-03-31'   # end of data
symbols <- c("XLB", #SPDR Materials sector
             "XLE", #SPDR Energy sector
             "XLF", #SPDR Financial sector
             "XLP", #SPDR Consumer staples sector
             "XLI", #SPDR Industrial sector
             "XLU", #SPDR Utilities sector
             "XLV", #SPDR Healthcare sector
             "XLK", #SPDR Tech sector
             "XLY", #SPDR Consumer discretionary sector
             "RWR", #SPDR Dow Jones REIT ETF
             "EWJ", #iShares Japan
             "EWG", #iShares Germany
             "EWU", #iShares UK
             "EWC", #iShares Canada
             "EWY", #iShares South Korea
             "EWA", #iShares Australia
             "EWH", #iShares Hong Kong
             "EWS", #iShares Singapore
             "IYZ", #iShares U.S. Telecom
             "EZU", #iShares MSCI EMU ETF
             "IYR", #iShares U.S. Real Estate
             "EWT", #iShares Taiwan
             "EWZ", #iShares Brazil
             "EFA", #iShares EAFE
             "IGE", #iShares North American Natural Resources
             "EPP", #iShares Pacific Ex Japan
             "LQD", #iShares Investment Grade Corporate Bonds
             "SHY", #iShares 1-3 year TBonds
             "IEF", #iShares 3-7 year TBonds
             "TLT" #iShares 20+ year Bonds
)

Sys.setenv(TZ="UTC")       # set time zone

## ----results='hide'------------------------------------------------------
getSymbols(symbols, src='yahoo', index.class=c("POSIXt","POSIXct"),
           from=startDate, to=endDate,adjust=TRUE)

## ----results='hide'------------------------------------------------------
initDate <- '2006-10-31'
initEq <- 1000000
currency("USD")
stock(symbols, currency="USD",multiplier=1)


## ------------------------------------------------------------------------
myTheme<-chart_theme()
myTheme$col$dn.col<-'lightblue'
myTheme$col$dn.border <- 'lightgray'
myTheme$col$up.border <- 'lightgray'

## ----XLX3x3,cache=FALSE--------------------------------------------------
par(mfrow=c(2,2))
for(symbol in symbols)
{
  plot(chart_Series(get(symbol),name=symbol))
}
par(mfrow=c(1,1))


## ------------------------------------------------------------------------
osFixedDollar <- function(timestamp,orderqty, portfolio, symbol, ruletype, ...)
{
  ClosePrice <- as.numeric(Cl(mktdata[timestamp,]))
  orderqty <- round(tradeSize/ClosePrice,-2)
  return(orderqty)
}


## ----results='hide'------------------------------------------------------
strategy("macd", store=TRUE)

## ----results='hide'------------------------------------------------------
add.indicator("macd", name = "MACD",
              arguments = list(x=quote(Cl(mktdata))),label='osc')

## ----results='hide'------------------------------------------------------
add.signal("macd",name="sigThreshold",
           arguments=list(column="signal.osc",relationship="gt",threshold=0,cross=TRUE),
           label="signal.gt.zero")

## ----results='hide'------------------------------------------------------
add.signal("macd",name="sigThreshold",
           arguments=list(column="signal.osc",relationship="lt",threshold=0,cross=TRUE),
           label="signal.lt.zero")


## ----results='hide'------------------------------------------------------
add.rule("macd",name='ruleSignal',
         arguments = list(sigcol="signal.gt.zero", sigval=TRUE,
                          replace=FALSE,
                          orderside='long',
                          ordertype='market',
                          orderqty=100,
                          osFUN='osFixedDollar',
                          orderset='ocolong'
         ),
         type='enter',
         label='LE'
)


## ----results='hide'------------------------------------------------------
add.rule("macd",name='ruleSignal',
         arguments = list(sigcol="signal.lt.zero", sigval=TRUE,
                          replace=TRUE,
                          orderside='long',
                          ordertype='market',
                          orderqty='all',
                          orderset='ocolong'
         ),
         type='exit',
         label='LX'
)




## ----results='hide'------------------------------------------------------
rm.strat("multi.macd.nostop") # remove portfolio, account, orderbook if re-run
initPortf(name="multi.macd.nostop", symbols, initDate=initDate)
initAcct(name="multi.macd.nostop", portfolios="multi.macd.nostop",
         initDate=initDate, initEq=initEq)
initOrders(portfolio="multi.macd.nostop", initDate=initDate)

fastMA = 12
slowMA = 26
signalMA = 9
maType="EMA"
tradeSize <- initEq/10

out<-applyStrategy("macd" , portfolios="multi.macd.nostop",
                   parameters=list(nFast=fastMA, nSlow=slowMA, nSig=signalMA,maType=maType),
                   verbose=TRUE)

updatePortf("multi.macd.nostop")
updateAcct("multi.macd.nostop")
updateEndEq("multi.macd.nostop")



## ----PERFNOSTOP,cache=FALSE----------------------------------------------
equity.curve <- getAccount("multi.macd.nostop")$summary$End.Eq
returns.ns <- Return.calculate(equity.curve,"log")
table.AnnualizedReturns(returns.ns)
charts.PerformanceSummary(returns.ns,wealth.index=TRUE,colorset="blue",
                          xlab="",main="MACD Performance (no stoploss)",minor.ticks=FALSE)

## ----TRADESTATSNOSTOP,results='hide',fig.width=9,fig.height=9------------
PerformanceAnalytics:::textplot(t(tradeStats("multi.macd.nostop")))


## ------------------------------------------------------------------------
stopLossPercent <- 0.05

## ----results='hide'------------------------------------------------------
add.rule("macd",name='ruleSignal',
         arguments = list(sigcol="signal.gt.zero", sigval=TRUE,
                          replace=FALSE,
                          orderside='long',
                          ordertype='stoplimit',
                          tmult=TRUE,
                          threshold=quote( stopLossPercent ),
                          orderqty='all',
                          orderset='ocolong'
         ),
         type='chain', parent="LE",
         label='StopLossLong',
         enabled=FALSE
)


## ----results='hide'------------------------------------------------------
rm.strat("multi.macd.stop") # remove portfolio, account, orderbook if re-run

## ----results='hide'------------------------------------------------------
initPortf(name="multi.macd.stop", symbols, initDate=initDate)
initAcct(name="multi.macd.stop", portfolios="multi.macd.stop",
         initDate=initDate, initEq=initEq)
initOrders(portfolio="multi.macd.stop", initDate=initDate)

## ----results='hide'------------------------------------------------------
enable.rule("macd",type="chain",labe="StopLoss")

## ----results='hide'------------------------------------------------------
out<-applyStrategy("macd" , portfolios="multi.macd.stop",
                   parameters=list(nFast=fastMA, nSlow=slowMA, nSig=signalMA,maType=maType),
                   verbose=TRUE)

## ----results='hide'------------------------------------------------------
updatePortf("multi.macd.stop")
updateAcct("multi.macd.stop")
updateEndEq("multi.macd.stop")

## ------------------------------------------------------------------------



## ----PERFSTOP------------------------------------------------------------
equity.curve <- getAccount("multi.macd.stop")$summary$End.Eq
returns.sl <- Return.calculate(equity.curve,"log")
table.AnnualizedReturns(returns.sl)
charts.PerformanceSummary(returns.sl,wealth.index=TRUE,colorset="blue",
                          xlab="",main="MACD Performance (with stoploss)",minor.ticks=FALSE)

## ----TRADESTATSSTOP,results='hide',fig.width=9,fig.height=9--------------
PerformanceAnalytics:::textplot(t(tradeStats("multi.macd.stop")))




## ------------------------------------------------------------------------
trailingStopPercent <- 0.07

## ----results='hide'------------------------------------------------------
add.rule("macd", name = 'ruleSignal',
         arguments=list(sigcol="signal.gt.zero" , sigval=TRUE,
                        replace=FALSE,
                        orderside='long',
                        ordertype='stoptrailing',
                        tmult=TRUE,
                        threshold=quote(trailingStopPercent),
                        orderqty='all',
                        orderset='ocolong'
         ),
         type='chain', parent="LE",
         label='StopTrailingLong',
         enabled=FALSE
)


## ----results='hide'------------------------------------------------------
rm.strat("multi.macd.trail") # remove portfolio, account, orderbook if re-run

## ----results='hide'------------------------------------------------------
initPortf(name="multi.macd.trail", symbols, initDate=initDate)
initAcct(name="multi.macd.trail", portfolios="multi.macd.trail",
         initDate=initDate, initEq=initEq)
initOrders(portfolio="multi.macd.trail", initDate=initDate)

## ----results='hide'------------------------------------------------------
enable.rule("macd",type="chain",labe="StopTrailingLong")

## ----results='hide'------------------------------------------------------
out<-applyStrategy("macd" , portfolios="multi.macd.trail",
                   parameters=list(nFast=fastMA, nSlow=slowMA, nSig=signalMA,maType=maType),
                   verbose=TRUE)

## ----results='hide'------------------------------------------------------
updatePortf("multi.macd.trail")
updateAcct("multi.macd.trail")
updateEndEq("multi.macd.trail")

## ----PERFTRAIL-----------------------------------------------------------
equity.curve <- getAccount("multi.macd.trail")$summary$End.Eq
returns.tr <- Return.calculate(equity.curve,"log")
table.AnnualizedReturns(returns.tr)
charts.PerformanceSummary(returns.tr,wealth.index=TRUE,colorset="blue",
                          xlab="",main="MACD Performance (with stoploss)",minor.ticks=FALSE)

## ----TRADESTATSTRAIL,results='hide',fig.width=9,fig.height=9-------------
PerformanceAnalytics:::textplot(t(tradeStats("multi.macd.trail")))




## ------------------------------------------------------------------------
stopLossPercentRange <- seq(0.03,0.05,by=0.01)

## ----results='hide'------------------------------------------------------
add.distribution("macd",
                 paramset.label = "STOPOPT",
                 component.type = "chain",
                 component.label = "StopLossLong",
                 variable = list( threshold = stopLossPercentRange ),
                 label = "StopLossLONG"
)


## ------------------------------------------------------------------------
trailingPercentRange <- seq(0.03,0.05,by=0.01)

## ----results='hide'------------------------------------------------------
add.distribution("macd",
                 paramset.label = "STOPOPT",
                 component.type = "chain",
                 component.label = "StopTrailingLong",
                 variable = list( threshold = trailingPercentRange ),
                 label = "TrailingLONG"
)




## ----results='hide'------------------------------------------------------
rm.strat("multi.macd.opt") # remove portfolio, account, orderbook if re-run

## ----results='hide'------------------------------------------------------
initPortf(name="multi.macd.opt", symbols, initDate=initDate)
initAcct(name="multi.macd.opt", portfolios="multi.macd.opt",
         initDate=initDate, initEq=initEq)
initOrders(portfolio="multi.macd.opt", initDate=initDate)

## ----results='hide'------------------------------------------------------
if( file.exists("resultsStopOpt.RData") )
{
  load("resultsStopOpt.RData")
} else {
  results <- apply.paramset("macd", paramset.label = "STOPOPT",
                            portfolio="multi.macd.opt", account="multi.macd.opt", nsamples=0)
  save(list="results",file="resultsStopOpt.RData")
}

## ------------------------------------------------------------------------
head(names(results),20)


## ----PROFITMDDHEAT,fig.width=7,fig.height=7,dev='png',dpi=300------------
z <- tapply(X=results$tradeStats$Profit.To.Max.Draw,
            INDEX=list(results$tradeStats$TrailingLONG,results$tradeStats$StopLossLONG),
            FUN=median)
x <- as.numeric(rownames(z))
y <- as.numeric(colnames(z))
filled.contour(x=x,y=y,z=z,color = heat.colors,
               xlab="Trailing Stop",ylab="Stop Loss")
title("Return to MaxDrawdown")


## ----results='hide'------------------------------------------------------
strategy("macd.opt", store=TRUE)

add.indicator("macd.opt", name = "MACD",
              arguments = list(x=quote(Cl(mktdata))),label='osc')

## ----results='hide'------------------------------------------------------
add.signal("macd.opt",name="sigThreshold",
           arguments=list(column="signal.osc",relationship="gt",threshold=0,cross=TRUE),
           label="signal.gt.zero")

## ----results='hide'------------------------------------------------------
add.signal("macd.opt",name="sigThreshold",
           arguments=list(column="signal.osc",relationship="lt",threshold=0,cross=TRUE),
           label="signal.lt.zero")


## ----results='hide'------------------------------------------------------
add.rule("macd.opt",name='ruleSignal',
         arguments = list(sigcol="signal.gt.zero", sigval=TRUE,
                          replace=FALSE,
                          orderside='long',
                          ordertype='market',
                          orderqty=100,
                          osFUN='osFixedDollar',
                          orderset='ocolong'
         ),
         type='enter',
         label='LE'
)

add.rule("macd.opt",name='ruleSignal',
         arguments = list(sigcol="signal.lt.zero", sigval=TRUE,
                          replace=TRUE,
                          orderside='long',
                          ordertype='market',
                          orderqty='all',
                          orderset='ocolong'
         ),
         type='exit',
         label='LX'
)


## ------------------------------------------------------------------------
stopLossPercent <- 0.03

## ----results='hide'------------------------------------------------------
add.rule("macd.opt",name='ruleSignal',
         arguments = list(sigcol="signal.gt.zero", sigval=TRUE,
                          replace=FALSE,
                          orderside='long',
                          ordertype='stoplimit',
                          tmult=TRUE,
                          threshold=quote( stopLossPercent ),
                          orderqty='all',
                          orderset='ocolong'
         ),
         type='chain', parent="LE",
         label='StopLossLong',
         enabled=TRUE
)


## ------------------------------------------------------------------------
trailingStopPercent <- 0.03

## ----results='hide'------------------------------------------------------
add.rule("macd.opt", name = 'ruleSignal',
         arguments=list(sigcol="signal.gt.zero" , sigval=TRUE,
                        replace=FALSE,
                        orderside='long',
                        ordertype='stoptrailing',
                        tmult=TRUE,
                        threshold=quote(trailingStopPercent),
                        orderqty='all',
                        orderset='ocolong'
         ),
         type='chain', parent="LE",
         label='StopTrailingLong',
         enabled=TRUE
)


## ----results='hide'------------------------------------------------------
rm.strat("multi.macd.opt") # remove portfolio, account, orderbook if re-run

## ----results='hide'------------------------------------------------------
initPortf(name="multi.macd.opt", symbols, initDate=initDate)
initAcct(name="multi.macd.opt", portfolios="multi.macd.opt",
         initDate=initDate, initEq=initEq)
initOrders(portfolio="multi.macd.opt", initDate=initDate)

## ----results='hide'------------------------------------------------------
out<-applyStrategy("macd.opt" , portfolios="multi.macd.opt",
                   parameters=list(nFast=fastMA, nSlow=slowMA, nSig=signalMA,maType=maType),
                   verbose=TRUE)

## ----results='hide'------------------------------------------------------
updatePortf("multi.macd.opt")
updateAcct("multi.macd.opt")
updateEndEq("multi.macd.opt")

## ------------------------------------------------------------------------

## ----PERFOPT-------------------------------------------------------------
equity.curve <- getAccount("multi.macd.opt")$summary$End.Eq
returns.opt <- Return.calculate(equity.curve,"log")
table.AnnualizedReturns(returns.opt)
charts.PerformanceSummary(returns.opt,wealth.index=TRUE,colorset="blue",
                          xlab="",main="MACD Performance (with stoploss)",minor.ticks=FALSE)

## ----TRADESTATSOrcs:::textplot(t(tradeStats("multi.macd.opt")))


## ----PERFOPTALL----------------------------------------------------------
rets <- cbind(returns.ns,returns.sl,returns.tr,returns.opt)
colnames(rets) <- c("NoStops","StopLoss","StopAndTrail","Optimal")
charts.PerformanceSummary(rets,main="Stop Comparision")
#risk management evolution graph
chart.RiskReturnScatter(rets,
                        main = "Risk Management Evolution", colorset = rich10equal)

tStats <- tradeStats("multi.macd.opt", use = "trades",
                     inclZeroDays = FALSE)
tStats[, 4:ncol(tStats)] <- round(tStats[, 4:ncol(tStats)], 2)
aggPF <- sum(tStats$Gross.Profits) / -sum(tStats$Gross.Losses) #profitfactr
aggCorrect <- mean(tStats$Percent.Positive)
meanAvgWLR <- mean(tStats$Avg.WinLoss.Ratio[
  tStats$Avg.WinLoss.Ratio < Inf], na.rm = TRUE)

getSymbols("SPY", from = "2006", to = to)
SPYrets <- diff(log(Cl(SPY)))[-1]
cumSPYrets <- cumprod(1 + SPYrets)
comparison <- cbind(returns.opt, cumSPYrets)
colnames(comparison) <- c("strategy", "SPY")
chart.TimeSeries(comparison, legend.loc = "topleft",
                 colors=c("green", "red"))

getSymbols('SPY', from = "2006-11-30", to = "2016-03-31")
buyholds <- 30*SPY$SPY.Adjusted
buyholdsr <- monthlyReturn(buyholds)
SharpeRatio.annualized(buyholdsr)
maxDrawdown(buyholdsr)
charts.PerformanceSummary(buyholdsr)
Return.annualized(buyholdsr)
DownsideDeviation(buyholdsr)

#buyhold for ETFS
getSymbols("ITOT", "AGG", "GLD", "VNQ", from = "2006-11-30", to = "2016-03-31")
buyhold <- ITOT$ITOT.Adjusted
buyhold$AGG <- AGG$AGG.Adjusted
buyhold$GLD <- GLD$GLD.Adjusted
buyhold$VNQ <- VNQ$VNQ.Adjusted
buyholdr <- monthlyReturn(buyhold)
SharpeRatio.annualized(buyholdr)
maxDrawdown(buyholdr)
charts.PerformanceSummary(buyholdr)
Return.annualized(buyholdr)
DownsideDeviation(buyholdr)

