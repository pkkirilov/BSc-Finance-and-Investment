## Intro to strategy

require(quantstrat)
require(blotter)
require(TTR)

#Phase 1 initialize currency, instruments, historical data
currency("USD")
stock("SPY", currency = "USD", multiplier = 1)
ls(envir = FinancialInstrument:::.instrument)
initDate <- '1999-12-31'
startDate <- '2000-01-01'
endDate <- '2015-12-31'
initEq <- 1e6
Sys.setenv(TZ="UTC") #timezone set
getSymbols('SPY', from = startDate, to = endDate, index.class = "POSIXct", adjust = T) #POSX time date index class
SPY = to.weekly(SPY, indexAt = 'endof', drop.time = FALSE)
SPY$SMA10m <- SMA(Cl(SPY), 10)
#Phase 2 initialize portfolio, accounts, orders, strategy
qs.strategy <- "qsFaber" 
rm.strat(qs.strategy) #if this is a re run remove strategy
initPortf(qs.strategy, 'SPY', initDate = initDate)
initAcct(qs.strategy, portfolios = qs.strategy, initDate = initDate, initEq = initEq)
initOrders(portfolio = qs.strategy, initDate = initDate) #initialize order container
strategy(qs.strategy, store = TRUE) #initialize a new strategy object
strat <- get.strategy(qs.strategy)
#add indicators, signals, rules
#Faber rules : Buy-Sell rules:
#buy when monthly price > 10-month SMA
#sell and move to cash when monthly price < 10-month SMA
#Notes:
#  all entry and exit prices are on the day of the signal at the close
#all data series are total return series including dividends, updated
#monthly
#commissions and slippage are excluded

#mktdata is the SPY historical prices
add.indicator(strategy = qs.strategy, name = "SMA", 
              arguments = list(x = quote(Cl(mktdata)), n = 10), label = "SMA10")
#Add signal for crossing above SMA
#Add signal for crossing below SMA
add.signal(qs.strategy,name="sigCrossover",
           arguments = list(columns=c("Close","SMA10"),relationship="gt"),
           label="Cl.gt.SMA")
add.signal(qs.strategy,name="sigCrossover",
           arguments = list(columns=c("Close","SMA10"),relationship="lt"),
           label="Cl.lt.SMA")
# go long when close > MA
add.rule(qs.strategy, name='ruleSignal',
         arguments = list(sigcol="Cl.gt.SMA", sigval = TRUE, orderqty=900,
                          ordertype='market', orderside='long'),
         type='enter')
# exit when close < MA
add.rule(qs.strategy, name='ruleSignal',
         arguments = list(sigcol="Cl.lt.SMA", sigval=TRUE, orderqty='all',
                          ordertype='market', orderside='long'),
         type='exit')
#Apply strategy to pf
applyStrategy(strategy = qs.strategy , portfolios = qs.strategy)
getTxns(Portfolio=qs.strategy, Symbol="SPY")
#update pf, account, equity
updatePortf(qs.strategy)
updateAcct(qs.strategy)
updateEndEq(qs.strategy)

#DATA INTEGRITY CHECK - SHOULD SAY TRUE AT THE END
checkBlotterUpdate <- function(port.st,account.st,verbose=TRUE)
{
  ok <- TRUE
  p <- getPortfolio(port.st)
  a <- getAccount(account.st)
  syms <- names(p$symbols)
  port.tot <- sum(sapply(syms,FUN = function(x) eval(parse(
    text=paste("sum(p$symbols",x,"posPL.USD$Net.Trading.PL)",sep="$")))))
  port.sum.tot <- sum(p$summary$Net.Trading.PL)
  if( !isTRUE(all.equal(port.tot,port.sum.tot)) ) {
    ok <- FALSE
    if( verbose )
      print("portfolio P&L doesn't match sum of symbols P&L")
  }
  initEq <- as.numeric(first(a$summary$End.Eq))
  endEq <- as.numeric(last(a$summary$End.Eq))
  if( !isTRUE(all.equal(port.tot,endEq-initEq)) ) {
    ok <- FALSE
    if( verbose )
      print("portfolio P&L doesn't match account P&L")
  }
  if( sum(duplicated(index(p$summary))) ) {
    ok <- FALSE
    if( verbose )
      print("duplicate timestamps in portfolio summary")
  }
  if( sum(duplicated(index(a$summary))) ) {
    ok <- FALSE
    if( verbose )
      print("duplicate timestamps in account summary")
  }
  return(ok)
}
checkBlotterUpdate(qs.strategy,qs.strategy)

#Phase 6 Reporting
myTheme<-chart_theme() #create custom theme
myTheme$col$dn.col<-'lightblue'
myTheme$col$dn.border <- 'lightgray'
myTheme$col$up.border <- 'lightgray'
chart.Posn(qs.strategy, Symbol = 'SPY', Dates = '2000::',theme=myTheme,
           TA='add_SMA(n=10,col=4, on=1, lwd=2)') #plot performance
tstats <- t(tradeStats(qs.strategy)) #trade statisctic
ob <- getOrderBook(qs.strategy) #retreve order book

perTradeStats(qs.strategy)

#quantstrat includes the capability to generate maximum adverse excursion
#(MAE) and maximum favorable excursion (MFE) charts.
chart.ME(Portfolio=qs.strategy, Symbol='SPY', type='MAE', scale='percent')
chart.ME(Portfolio=qs.strategy, Symbol='SPY', type='MFE', scale='percent')

a <- getAccount(qs.strategy) #retrieving account summary
last(a$summary,5)

library(lattice)
xyplot(a$summary,type="h",col=4) 

equity <- a$summary$End.Eq #plot equity curve and performance chart
plot(equity,main="Faber Strategy Equity Curve")
ret <- Return.calculate(equity,method="log")
charts.PerformanceSummary(ret, colorset = bluefocus,
                          main="Faber Strategy Performance")





  
