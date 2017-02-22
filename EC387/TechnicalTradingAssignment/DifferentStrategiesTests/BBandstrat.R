#Bollinger Band strategy
#Quantstrat lecture 2
library(quantstrat)
startDate <- '2010-01-01' # start of data
endDate <- '2013-07-31' # end of data
symbols = c("XLF", "XLP", "XLE", "XLY", "XLV", "XLI", "XLB", "XLK", "XLU")
Sys.setenv(TZ="UTC") # set time zone
getSymbols(symbols, src='yahoo', index.class=c("POSIXt","POSIXct"),
           from=startDate, to=endDate, adjust=TRUE)
myTheme<-chart_theme()
myTheme$col$dn.col<-'lightblue'
myTheme$col$dn.border <- 'lightgray'
myTheme$col$up.border <- 'lightgray'
par(mfrow=c(3,3))
for(symbol in symbols)
{
  plot(chart_Series(get(symbol),name=symbol,theme=myTheme))
}
par(mfrow=c(1,1))
initDate <- '2009-12-31'
initEq <- 1e6
currency("USD")
stock(symbols, currency="USD",multiplier=1)
b <- BBands(HLC=HLC(XLF["2013"]), n=20, sd=2)
chart_Series(XLF["2013"],TA='add_BBands(lwd=2)',theme=myTheme,name="XLF")
rm.strat("multiAsset.bb1") # remove portfolio, account, orderbook if re-run
initPortf(name="multiAsset.bb1", symbols, initDate=initDate)
initAcct(name="multiAsset.bb1", portfolios="multiAsset.bb1",
         initDate=initDate, initEq=initEq)
initOrders(portfolio="multiAsset.bb1", initDate=initDate)
strategy("bbands", store=TRUE)
add.indicator("bbands", name = "BBands",
              arguments = list(HLC = quote(HLC(mktdata)), maType='SMA'), label='BBands')
add.signal("bbands", name="sigCrossover",
           arguments=list(columns=c("Close","up"),relationship="gt"),
           label="Cl.gt.UpperBand")
add.signal("bbands", name="sigCrossover",
           arguments=list(columns=c("Close","dn"),relationship="lt"),
           label="Cl.lt.LowerBand")
add.signal("bbands", name="sigCrossover",
           arguments=list(columns=c("High","Low","mavg"),relationship="op"),
           label="Cross.Mid")
add.rule("bbands", name='ruleSignal',
         arguments=list(sigcol="Cl.gt.UpperBand",sigval=TRUE, orderqty=-100,
                        ordertype='market', orderside=NULL),type='enter')
add.rule("bbands", name='ruleSignal',
         arguments=list(sigcol="Cl.lt.LowerBand",sigval=TRUE, orderqty= 100,
                        ordertype='market', orderside=NULL),type='enter')
add.rule("bbands", name='ruleSignal',
         arguments=list(sigcol="Cross.Mid",sigval=TRUE, orderqty= 'all',
                        ordertype='market', orderside=NULL),type='exit')
SD = 2
N = 20
out <- applyStrategy("bbands",
                     portfolios="multiAsset.bb1",parameters=list(sd=SD,n=N))
updatePortf("multiAsset.bb1")
updateAcct("multiAsset.bb1")
updateEndEq("multiAsset.bb1")
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
checkBlotterUpdate("multiAsset.bb1","multiAsset.bb1")
chart.Posn("multiAsset.bb1","XLB",TA="add_BBands(n=20,sd=2)",theme=myTheme)
chart.Posn("multiAsset.bb1","XLB",TA="add_BBands(n=20,sd=2)",
           Dates="2010",theme=myTheme)
rm.strat("multiAsset.bb2")
initPortf(name="multiAsset.bb2", symbols, initDate=initDate)
initAcct(name="multiAsset.bb2", portfolios="multiAsset.bb2",
         initDate=initDate, initEq=initEq)
initOrders(portfolio="multiAsset.bb2", initDate=initDate)
SD=3
out <- applyStrategy("bbands",
                     portfolios="multiAsset.bb2",parameters=list(sd=SD,n=N))
updatePortf("multiAsset.bb2")
updateAcct("multiAsset.bb2")
updateEndEq("multiAsset.bb2")
checkBlotterUpdate("multiAsset.bb2","multiAsset.bb2")
eq1 <- getAccount("multiAsset.bb1")$summary$End.Eq
rt1 <- Return.calculate(eq1,"log")
eq2 <- getAccount("multiAsset.bb2")$summary$End.Eq
rt2 <- Return.calculate(eq2,"log")
returns <- cbind(rt1,rt2)
colnames(returns) <- c("SD=2","SD=3")
chart.CumReturns(returns,colorset=c(2,4),legend.loc="topleft",
                 main="BBand SD Parameter Comparison",ylab="cum return",xlab="",
                 minor.ticks=FALSE)