library(quantstrat)
currency("USD")
## [1] "USD"
stock("SPY",currency="USD",multiplier=1)
## [1] "SPY"
ls(envir=FinancialInstrument:::.instrument)
## [1] "SPY" "USD"
ls(all=T)
# system settings
initDate <- '1997-12-31'
startDate <- '1998-01-01'
endDate <- '2014-06-30'
initEq <- 1e6
Sys.setenv(TZ="UTC")
getSymbols('SPY', from=startDate, to=endDate, index.class="POSIXct", adjust=T)
SPY=to.monthly(SPY, indexAt='endof', drop.time=FALSE)
SPY$SMA10m <- SMA(Cl(SPY), 10)
head(SPY)
# inz portfolio, account
qs.strategy <- "qsFaber"
rm.strat(qs.strategy) # remove strategy etc. if this is a re-run
initPortf(qs.strategy,'SPY', initDate=initDate)
initAcct(qs.strategy,portfolios=qs.strategy, initDate=initDate, initEq=initEq)
# initialize orders container
args(initOrders)
## function (portfolio = NULL, symbols = NULL, initDate = "1999-12-31",
## ...)
## NULL
initOrders(portfolio=qs.strategy,initDate=initDate)
# instantiate a new strategy object
args(strategy)
## function (name, ..., assets = NULL, constraints = NULL, store = FALSE)
## NULL
strategy(qs.strategy,store=TRUE)
ls(all=T)
## [1] ".blotter" ".strategy" "endDate" "filename" "initDate"
## [6] "initEq" "qs.strategy" "SPY" "startDate"
ls(.blotter)
## [1] "account.qsFaber" "portfolio.qsFaber"
ls(.strategy)
args(getStrategy)
## function (x, envir = .strategy)
## NULL
strat <-getStrategy(qs.strategy)
class(strat)
## [1] "strategy"
summary(strat)
args(add.indicator)
add.indicator(strategy = qs.strategy, name = "SMA",
              arguments = list(x = quote(Cl(mktdata)), n=10), label="SMA10")
summary(getStrategy(qs.strategy))
add.signal(qs.strategy,name="sigCrossover",
           arguments = list(columns=c("Close","SMA10"),relationship="gt"),
           label="Cl.gt.SMA")
add.signal(qs.strategy,name="sigCrossover",
           arguments = list(columns=c("Close","SMA10"),relationship="lt"),
           label="Cl.lt.SMA")
summary(getStrategy(qs.strategy))
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
summary(getStrategy(qs.strategy))
applyStrategy(strategy=qs.strategy , portfolios=qs.strategy)
getTxns(Portfolio=qs.strategy, Symbol="SPY")
updatePortf(qs.strategy)
updateAcct(qs.strategy)
updateEndEq(qs.strategy)
#Data integrity check
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
# create custom theme
myTheme<-chart_theme()
myTheme$col$dn.col<-'lightblue'
myTheme$col$dn.border <- 'lightgray'
myTheme$col$up.border <- 'lightgray'
# plot performance
chart.Posn(qs.strategy, Symbol = 'SPY', Dates = '1998::',theme=myTheme,
           TA='add_SMA(n=10,col=4, on=1, lwd=2)')
tstats <- t(tradeStats(qs.strategy))
ob <- getOrderBook(qs.strategy)
class(ob)
## [1] "order_book"
names(ob)
## [1] "qsFaber"
names(ob$qsFaber)
## [1] "SPY"
names(ob$qsFaber$SPY)
ob$qsFaber$SPY[,1:5]
perTradeStats(qs.strategy)
chart.ME(Portfolio=qs.strategy, Symbol='SPY', type='MAE', scale='percent')
chart.ME(Portfolio=qs.strategy, Symbol='SPY', type='MFE', scale='percent')
a <- getAccount(qs.strategy)
last(a$summary,5)
library(lattice)
xyplot(a$summary,type="h",col=4)
equity <- a$summary$End.Eq
plot(equity,main="Faber Strategy Equity Curve")
ret <- Return.calculate(equity,method="log")
charts.PerformanceSummary(ret, colorset = bluefocus,
                          main="Faber Strategy Performance")
