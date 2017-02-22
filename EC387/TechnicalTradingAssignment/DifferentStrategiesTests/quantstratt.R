

require(blotter)
require(quantstrat)
require(TTR)
#require(IKTrading)

symbols = c("VTI", "VEA", "VWO", "VNQ", "IEF", "TIP")
currency("USD")
initDate <- '1997-12-31'
startDate <- '1998-01-01'
endDate <- '2014-06-30'
initEq <- 1000
Sys.setenv(TZ="UTC") #timezone set
getSymbols(symbols, from = startDate, to = endDate, index.class = "POSIXct", adjust = T) 
#transform data into monthly
for(symbol in symbols)
{
  stock(symbol, currency="USD",multiplier=1)
  x<-get(symbol)
  x<-to.monthly(x,indexAt='endof',drop.time=FALSE)
  indexFormat(x)<-'%Y-%m-%d'
  colnames(x)<-gsub("x",symbol,colnames(x))
  assign(symbol,x)
}
qs.strategy <- "qsFaber"
rm.strat(qs.strategy) # remove strategy etc. if this is a re-run
initPortf(qs.strategy,'SPY', initDate=initDate)
initAcct(qs.strategy,portfolios=qs.strategy, initDate=initDate, initEq=initEq)
initOrders(portfolio=qs.strategy,initDate=initDate)
strategy(qs.strategy,store=TRUE)


add.indicator(strategy = qs.strategy, name = "SMA",
              arguments = list(x = quote(Cl(mktdata)), n=10), label="SMA10")
add.signal(qs.strategy,name="sigCrossover",
           arguments = list(columns=c("Close","SMA10"),relationship="gt"),
           label="Cl.gt.SMA")
add.signal(qs.strategy,name="sigCrossover",
           arguments = list(columns=c("Close","SMA10"),relationship="lt"),
           label="Cl.lt.SMA")
# go long when close > MA
add.rule(qs.strategy, name='ruleSignal',
         arguments = list(sigcol="Cl.gt.SMA", sigval=TRUE, orderqty=900,
                          ordertype='market', orderside='long'),
         type='enter')
# exit when close < MA
add.rule(qs.strategy, name='ruleSignal',
         arguments = list(sigcol="Cl.lt.SMA", sigval=TRUE, orderqty='all',
                          ordertype='market', orderside='long'),
         type='exit')

multi.asset <- "multiAsset"
rm.strat(multi.asset) # remove strategy etc. if this is a re-run
initPortf(multi.asset,symbols=symbols, initDate=initDate)
initAcct(multi.asset,portfolios=multi.asset, initDate=initDate,
         initEq=initEq)
initOrders(portfolio=multi.asset,initDate=initDate)

applyStrategy(strategy=qs.strategy , portfolios=multi.asset)
updatePortf(multi.asset)
updateAcct(multi.asset)
updateEndEq(multi.asset)
a <- getAccount(multi.asset)
p <- getPortfolio(multi.asset)
names(p$symbols)
par(mfrow=c(3,3))
for(symbol in symbols)
{
  chart.Posn(Portfolio=multi.asset,Symbol=symbol,theme=myTheme,
             TA="add_SMA(n=10,col='blue')")
}
par(mfrow=c(1,1))
par(mfrow=c(3,2))
for(symbol in symbols)
{
  chart.Posn(Portfolio=multi.asset,Symbol=symbol,theme=myTheme,
             TA="add_SMA(n=10,col='blue')")
}
  par(mfrow=c(1,1))

  rets.multi <- PortfReturns(multi.asset)
  colnames(rets.multi) <- symbols
  rets.multi <- na.omit(cbind(rets.multi,Return.calculate(a$summary$End.Eq)))
  names(rets.multi)[length(names(rets.multi))] <- "TOTAL"
  rets.multi <- rets.multi[,c("TOTAL",symbols)]
  round(tail(rets.multi,5),6)
  chart.CumReturns(rets.multi, colorset= rich10equal, legend.loc = "topleft",
                   main="SPDR Cumulative Returns")
  (ar.tab <- table.AnnualizedReturns(rets.multi))
  max.risk <- max(ar.tab["Annualized Std Dev",])
  max.return <- max(ar.tab["Annualized Return",])
  chart.RiskReturnScatter(rets.multi,
                          main = "SPDR Performance", colorset = rich10equal,
                          xlim=c(0,max.risk*1.1),ylim=c(0,max.return))
  #The End.Eq column from the account summary time series represents the
  #consolidated equity value across all portfolios and all of their assets.
  equity <- a$summary$End.Eq
  plot(equity,main="Consolidated SPDR Equity Curve")
  