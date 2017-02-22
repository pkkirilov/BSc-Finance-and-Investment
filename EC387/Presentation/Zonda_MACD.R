
library(quantstrat)
startDate <- '2006-11-30'  # start of data
endDate <-  '2016-03-31'   # end of data
symbols = c("ITOT", "AGG", "GLD", "VNQ")
Sys.setenv(TZ="UTC")       # set time zone

getSymbols(symbols, src='yahoo', index.class=c("POSIXt","POSIXct"),
  from=startDate, to=endDate,adjust=TRUE)

initDate <- '2006-10-31'
initEq <- 1000000
currency("USD")
stock(symbols, currency="USD",multiplier=1)

#set the chart theme
myTheme<-chart_theme()
myTheme$col$dn.col<-'lightblue'
myTheme$col$dn.border <- 'lightgray'
myTheme$col$up.border <- 'lightgray'

#adjust the plot window to show more than one chart simultaniously
par(mfrow=c(2,2))
for(symbol in symbols)
{
  plot(chart_Series(get(symbol),name=symbol))
}
par(mfrow=c(1,1))

#define the sizing order function <- size of trade/closing price
osFixedDollar <- function(timestamp,orderqty, portfolio, symbol, ruletype, ...)
{
  ClosePrice <- as.numeric(Cl(mktdata[timestamp,]))
  orderqty <- round(tradeSize/ClosePrice,-2)
  return(orderqty)
}
#define simple stratety
strategy("macd_strat", store=TRUE)


add.indicator("macd_strat", name = "MACD",
  arguments = list(x=quote(Cl(mktdata))),label='osc')


add.signal("macd_strat",name="sigThreshold",
  arguments=list(column="signal.osc",relationship="gt",threshold=0,cross=TRUE),
  label="signal.gt.zero")

add.signal("macd_strat",name="sigThreshold",
  arguments=list(column="signal.osc",relationship="lt",threshold=0,cross=TRUE),
  label="signal.lt.zero")



add.rule("macd_strat",name='ruleSignal',
  arguments = list(sigcol="signal.gt.zero", sigval=TRUE,
    replace=FALSE,
    orderside='long',
    ordertype='market',
    orderqty=100,
    osFUN='osFixedDollar',
    orderset='ocolong'
  ),
  type='enter',
  label='entr'
)


add.rule("macd_strat",name='ruleSignal',
  arguments = list(sigcol="signal.lt.zero", sigval=TRUE,
    replace=TRUE,
    orderside='long',
    ordertype='market',
    orderqty='all',
    orderset='ocolong'
  ),
  type='exit',
  label='ext'
)


rm.strat("basic.macd") # remove portfolio, account, orderbook if re-run
initPortf(name="basic.macd", symbols, initDate=initDate)
initAcct(name="basic.macd", portfolios="basic.macd",
  initDate=initDate, initEq=initEq)
initOrders(portfolio="basic.macd", initDate=initDate)

fastMA = 12
slowMA = 26
signalMA = 9
maType="EMA"
tradeSize <- initEq/10

out<-applyStrategy("macd_strat" , portfolios="basic.macd",
  parameters=list(nFast=fastMA, nSlow=slowMA, nSig=signalMA,maType=maType),
  verbose=TRUE)

updatePortf("basic.macd")
updateAcct("basic.macd")
updateEndEq("basic.macd")

equity.curve <- getAccount("basic.macd")$summary$End.Eq
returns.ns <- Return.calculate(equity.curve,"log")
table.AnnualizedReturns(returns.ns)
charts.PerformanceSummary(returns.ns,wealth.index=TRUE,colorset="blue",
  xlab="",main="MACD Performance (no stoploss)",minor.ticks=FALSE)

PerformanceAnalytics:::textplot(t(tradeStats("basic.macd")))


stopLossPercent <- 0.05

add.rule("macd_strat",name='ruleSignal',
  arguments = list(sigcol="signal.gt.zero", sigval=TRUE,
    replace=FALSE,
    orderside='long',
    ordertype='stoplimit',
    tmult=TRUE,
    threshold=quote( stopLossPercent ),
    orderqty='all',
    orderset='ocolong'
  ),
  type='chain', parent="entr",
  label='StopLossLong',
  enabled=FALSE
)

rm.strat("macd.stop") # remove portfolio, account, orderbook if re-run

initPortf(name="macd.stop", symbols, initDate=initDate)
initAcct(name="macd.stop", portfolios="macd.stop",
  initDate=initDate, initEq=initEq)
initOrders(portfolio="macd.stop", initDate=initDate)

enable.rule("macd_strat",type="chain",labe="StopLoss")

out<-applyStrategy("macd_strat" , portfolios="macd.stop",
  parameters=list(nFast=fastMA, nSlow=slowMA, nSig=signalMA,maType=maType),
  verbose=TRUE)


updatePortf("macd.stop")
updateAcct("macd.stop")
updateEndEq("macd.stop")


equity.curve <- getAccount("macd.stop")$summary$End.Eq
returns.sl <- Return.calculate(equity.curve,"log")
table.AnnualizedReturns(returns.sl)
charts.PerformanceSummary(returns.sl,wealth.index=TRUE,colorset="blue",
  xlab="",main="MACD Performance (with stoploss)",minor.ticks=FALSE)


PerformanceAnalytics:::textplot(t(tradeStats("macd.stop")))



trailingStopPercent <- 0.07


add.rule("macd_strat", name = 'ruleSignal',
  arguments=list(sigcol="signal.gt.zero" , sigval=TRUE,
    replace=FALSE,
    orderside='long',
    ordertype='stoptrailing',
    tmult=TRUE,
    threshold=quote(trailingStopPercent),
    orderqty='all',
    orderset='ocolong'
  ),
  type='chain', parent="entr",
  label='StopTrailingLong',
  enabled=FALSE
)


rm.strat("macd.trailstop") # remove portfolio, account, orderbook if re-run

initPortf(name="macd.trailstop", symbols, initDate=initDate)
initAcct(name="macd.trailstop", portfolios="macd.trailstop",
  initDate=initDate, initEq=initEq)
initOrders(portfolio="macd.trailstop", initDate=initDate)

enable.rule("macd_strat",type="chain",labe="StopTrailingLong")

out<-applyStrategy("macd_strat" , portfolios="macd.trailstop",
  parameters=list(nFast=fastMA, nSlow=slowMA, nSig=signalMA,maType=maType),
  verbose=TRUE)

updatePortf("macd.trailstop")
updateAcct("macd.trailstop")
updateEndEq("macd.trailstop")

equity.curve <- getAccount("macd.trailstop")$summary$End.Eq
returns.tr <- Return.calculate(equity.curve,"log")
table.AnnualizedReturns(returns.tr)
charts.PerformanceSummary(returns.tr,wealth.index=TRUE,colorset="blue",
  xlab="",main="MACD Performance (with stoploss)",minor.ticks=FALSE)

PerformanceAnalytics:::textplot(t(tradeStats("macd.trailstop")))


stopLossPercentRange <- seq(0.03,0.05,by=0.01)

add.distribution("macd_strat",
  paramset.label = "STOPOPT",
  component.type = "chain",
  component.label = "StopLossLong",
  variable = list( threshold = stopLossPercentRange ),
  label = "StopLossLONG"
)

trailingPercentRange <- seq(0.03,0.05,by=0.01)


add.distribution("macd_strat",
  paramset.label = "STOPOPT",
  component.type = "chain",
  component.label = "StopTrailingLong",
  variable = list( threshold = trailingPercentRange ),
  label = "TrailingLONG"
)


rm.strat("macd.opttrailstop") # remove portfolio, account, orderbook if re-run

initPortf(name="macd.opttrailstop", symbols, initDate=initDate)
initAcct(name="macd.opttrailstop", portfolios="macd.opttrailstop",
  initDate=initDate, initEq=initEq)
initOrders(portfolio="macd.opttrailstop", initDate=initDate)

if( file.exists("resultsStopOpt.RData") )
{
  load("resultsStopOpt.RData")
} else {
  results <- apply.paramset("macd_strat", paramset.label = "STOPOPT",
    portfolio="macd.opttrailstop", account="macd.opttrailstop", nsamples=0)
  save(list="results",file="resultsStopOpt.RData")
}

head(names(results),20)


z <- tapply(X=results$tradeStats$Profit.To.Max.Draw,
  INDEX=list(results$tradeStats$TrailingLONG,results$tradeStats$StopLossLONG),
  FUN=median)
x <- as.numeric(rownames(z))
y <- as.numeric(colnames(z))
filled.contour(x=x,y=y,z=z,color = heat.colors,
  xlab="Trailing Stop",ylab="Stop Loss")
title("Return to MaxDrawdown")



strategy("macd.optim", store=TRUE)

add.indicator("macd.optim", name = "MACD",
  arguments = list(x=quote(Cl(mktdata))),label='osc')

add.signal("macd.optim",name="sigThreshold",
  arguments=list(column="signal.osc",relationship="gt",threshold=0,cross=TRUE),
  label="signal.gt.zero")

add.signal("macd.optim",name="sigThreshold",
  arguments=list(column="signal.osc",relationship="lt",threshold=0,cross=TRUE),
  label="signal.lt.zero")


add.rule("macd.optim",name='ruleSignal',
  arguments = list(sigcol="signal.gt.zero", sigval=TRUE,
    replace=FALSE,
    orderside='long',
    ordertype='market',
    orderqty=100,
    osFUN='osFixedDollar',
    orderset='ocolong'
  ),
  type='enter',
  label='entr'
)

add.rule("macd.optim",name='ruleSignal',
  arguments = list(sigcol="signal.lt.zero", sigval=TRUE,
    replace=TRUE,
    orderside='long',
    ordertype='market',
    orderqty='all',
    orderset='ocolong'
  ),
  type='exit',
  label='ext'
)

stopLossPercent <- 0.03

add.rule("macd.optim",name='ruleSignal',
  arguments = list(sigcol="signal.gt.zero", sigval=TRUE,
    replace=FALSE,
    orderside='long',
    ordertype='stoplimit',
    tmult=TRUE,
    threshold=quote( stopLossPercent ),
    orderqty='all',
    orderset='ocolong'
  ),
  type='chain', parent="entr",
  label='StopLossLong',
  enabled=TRUE
)


trailingStopPercent <- 0.03

add.rule("macd.optim", name = 'ruleSignal',
  arguments=list(sigcol="signal.gt.zero" , sigval=TRUE,
    replace=FALSE,
    orderside='long',
    ordertype='stoptrailing',
    tmult=TRUE,
    threshold=quote(trailingStopPercent),
    orderqty='all',
    orderset='ocolong'
  ),
  type='chain', parent="entr",
  label='StopTrailingLong',
  enabled=TRUE
)


rm.strat("macd.opttrailstop") # remove portfolio, account, orderbook if re-run

initPortf(name="macd.opttrailstop", symbols, initDate=initDate)
initAcct(name="macd.opttrailstop", portfolios="macd.opttrailstop",
  initDate=initDate, initEq=initEq)
initOrders(portfolio="macd.opttrailstop", initDate=initDate)

out<-applyStrategy("macd.optim" , portfolios="macd.opttrailstop",
  parameters=list(nFast=fastMA, nSlow=slowMA, nSig=signalMA,maType=maType),
  verbose=TRUE)

updatePortf("macd.opttrailstop")
updateAcct("macd.opttrailstop")
updateEndEq("macd.opttrailstop")

equity.curve <- getAccount("macd.opttrailstop")$summary$End.Eq
returns.opt <- Return.calculate(equity.curve,"log")
table.AnnualizedReturns(returns.opt)
charts.PerformanceSummary(returns.opt,wealth.index=TRUE,colorset="blue",
  xlab="",main="MACD Performance (with stoploss)",minor.ticks=FALSE)


plot(equity.curve,main="Consolidated Fund Equity Curve" )

themelist<-chart_theme()
themelist$col$dn.col<-'lightblue'
themelist$col$dn.border <- 'lightgray'
themelist$col$dn.col<-'pink'
themelist$col$up.border <- 'lightgray'
par(mfrow=c(2,2))
chart.Posn("macd.opttrailstop", Symbol = "AGG", theme = themelist)
add_MACD()
chart.Posn("macd.opttrailstop", Symbol = "GLD", theme = themelist)
add_MACD()
chart.Posn("macd.opttrailstop", Symbol = "ITOT", theme = themelist)
add_MACD()
chart.Posn("macd.opttrailstop", Symbol = "VNQ", theme = themelist)
add_MACD()
par(mfrow=c(1,1))



rets <- cbind(returns.ns,returns.sl,returns.tr,returns.opt)
colnames(rets) <- c("NoStops","5% StopLoss","7% StopAndTrail","3% Optimal")
charts.PerformanceSummary(rets,main="StopLoss Comparision")
#risk management evolution graph
chart.RiskReturnScatter(rets,
main = "Risk Management Evolution", colorset = rich10equal)

tStats <- tradeStats("macd.opttrailstop", use = "trades",
                     inclZeroDays = FALSE)
tStats[, 4:ncol(tStats)] <- round(tStats[, 4:ncol(tStats)], 2)
aggPF <- sum(tStats$Gross.Profits) / -sum(tStats$Gross.Losses) #profitfactr
aggCorrect <- mean(tStats$Percent.Positive)
meanAvgWLR <- mean(tStats$Avg.WinLoss.Ratio[
  tStats$Avg.WinLoss.Ratio < Inf], na.rm = TRUE)
trans <- getTxns(Portfolio ="macd.opttrailstop", Symbol = "AGG")
sumtra <- trans[,1]
sum(abs(sumtra))

#cumul returns
returns.opt <- PortfReturns("macd.opttrailstop")
colnames(returns.opt) <- symbols
returns.opt <- na.omit(cbind(returns.opt,Return.calculate(equity.curve)))
names(returns.opt)[length(names(returns.opt))] <- "TOTAL"
returns.opt <- returns.opt[,c("TOTAL",symbols)]
round(tail(returns.opt,5),6)
chart.CumReturns(returns.opt, colorset = rich8equal, legend.loc = "topleft", main = "Fund Cumulative Returns" )


#buyhold SPY
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

