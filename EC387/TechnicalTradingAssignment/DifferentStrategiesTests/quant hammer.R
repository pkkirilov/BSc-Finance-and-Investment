#Hammer strategy

hammer <- function(OHLC, profMargin=1.5) {
  dailyMax <- pmax(Op(OHLC), Cl(OHLC))
  dailyMin <- pmin(Op(OHLC), Cl(OHLC))
  upShadow <- Hi(OHLC) - dailyMax
  dnShadow <- dailyMin - Lo(OHLC)
  body <- dailyMax-dailyMin
  hammerDay <- dnShadow/body > 2 & dnShadow/upShadow > 5
  hammers <- OHLC[hammerDay==1,]
  hammers$stopLoss <- 4/3*Lo(hammers)-1/3*Hi(hammers)
  hammers$takeProfit <- Hi(hammers) + (Hi(hammers)-hammers$stopLoss)*profMargin
  hammers <- cbind(hammerDay, hammers$stopLoss, hammers$takeProfit)
  hammers$stopLoss <- na.locf(hammers$stopLoss)
  hammers$takeProfit <- na.locf(hammers$takeProfit)
  colnames(hammers) <- c("hammer", "SL", "TP")
  return(hammers)
}

require(IKTrading)
require(quantstrat)
require(PerformanceAnalytics)

initDate="1990-01-01"
from="2003-01-01"
to=as.character(Sys.Date())
options(width=70)
verbose=TRUE

source("demoData.R")

#trade sizing and initial equity settings
tradeSize <- 100000
initEq <- tradeSize*length(symbols)

strategy.st <- portfolio.st <- account.st <- "Hammer_4TP"
rm.strat(portfolio.st)
rm.strat(strategy.st)
initPortf(portfolio.st, symbols=symbols, initDate=initDate, currency='USD')
initAcct(account.st, portfolios=portfolio.st, initDate=initDate, currency='USD',initEq=initEq)
initOrders(portfolio.st, initDate=initDate)
strategy(strategy.st, store=TRUE)

#parameters
nSMA1=10
nSMA2=30
nSMA3=5
profMargin=1.5

period=10
pctATR=.1


#indicators
add.indicator(strategy.st, name="lagATR", 
              arguments=list(HLC=quote(HLC(mktdata)), 
                             n=period), 
              label="atrX")

add.indicator(strategy.st, name="hammer",
              arguments=list(OHLC=quote(OHLC(mktdata)), 
                             profMargin=profMargin),
              label="hammer")

add.indicator(strategy.st, name="SMA",
              arguments=list(x=quote(Cl(mktdata)), 
                             n=nSMA1),
              label="sma1")

add.indicator(strategy.st, name="SMA",
              arguments=list(x=quote(Cl(mktdata)), 
                             n=nSMA2),
              label="sma2")

add.indicator(strategy.st, name="SMA",
              arguments=list(x=quote(Cl(mktdata)), 
                             n=nSMA3),
              label="sma3")
#signals
add.signal(strategy.st, name="sigComparison",
           arguments=list(columns=c("SMA.sma1", "SMA.sma2"), 
                          relationship="gt"),
           label="upTrend")

add.signal(strategy.st, name="sigComparison",
           arguments=list(columns=c("SMA.sma3", "SMA.sma1"), 
                          relationship="lt"),
           label="pullback")

add.signal(strategy.st, name="sigThreshold",
           arguments=list(column="hammer.hammer", threshold=.5, 
                          relationship="gt", cross=TRUE),
           label="hammerDay")

add.signal(strategy.st, name="sigAND",
           arguments=list(columns=c("upTrend", 
                                    "pullback", 
                                    "hammerDay"), 
                          cross=TRUE),
           label="longEntry")

add.signal(strategy.st, name="sigCrossover",
           arguments=list(columns=c("SMA.sma1", "SMA.sma2"), 
                          relationship="lt"),
           label="SMAexit")
#rules
add.rule(strategy.st, name="ruleSignal", 
         arguments=list(sigcol="longEntry", 
                        sigval=TRUE, 
                        ordertype="stoplimit", 
                        orderside="long", 
                        replace=FALSE, 
                        osFUN=osDollarATR,
                        tradeSize=tradeSize, 
                        prefer="High",
                        pctATR=pctATR,
                        atrMod="X",
                        orderset="orders"), 
         type="enter", path.dep=TRUE,
         label="hammerEntry")

add.rule(strategy.st, name="ruleSignal", 
         arguments=list(sigcol="longEntry", 
                        sigval=TRUE, 
                        ordertype="stoplimit", 
                        orderside="long", 
                        replace=FALSE, 
                        orderqty='all',
                        order.price=quote(mktdata$SL.hammer[timestamp]),
                        orderset="orders"), 
         type="chain", 
         parent="hammerEntry",
         label="stopLossLong",
         path.dep=TRUE)

add.rule(strategy.st, name="ruleSignal", 
         arguments=list(sigcol="longEntry", 
                        sigval=TRUE, 
                        ordertype="limit", 
                        orderside="long", 
                        replace=FALSE, 
                        orderqty='all',
                        order.price=quote(mktdata$TP.hammer[timestamp]),
                        orderset="orders"), 
         type="chain", 
         parent="hammerEntry",
         label="takeProfitLong",
         path.dep=TRUE)

add.rule(strategy.st, name="ruleSignal",
         arguments=list(sigcol="SMAexit",
                        sigval=TRUE,
                        ordertype="market",
                        orderside="long",
                        replace=TRUE,
                        orderqty='all',
                        prefer='Open',
                        orderset='orders'
         ),
         type='exit',
         label='SMAexitLong',
         path.dep=TRUE)

#apply strategy
t1 <- Sys.time()
out <- applyStrategy(strategy=strategy.st,portfolios=portfolio.st)
t2 <- Sys.time()
print(t2-t1)

#set up analytics
updatePortf(portfolio.st)
dateRange <- time(getPortfolio(portfolio.st)$summary)[-1]
updateAcct(portfolio.st,dateRange)
updateEndEq(account.st)

(aggPF <- sum(tStats$Gross.Profits)/-sum(tStats$Gross.Losses))
print(t(durStats))
