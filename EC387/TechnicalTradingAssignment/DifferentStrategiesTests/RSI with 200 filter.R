
#seems like the two cannot be made to work together
#load the needed packages
#RSI with 200 filter 
require(blotter)
require(quantstrat)
require(lattice)
require(TTR)
require(IKTrading)


#Initialise currency, instruments, historical data
currency("USD")
stock("SPY", currency = "USD", multiplier = 1)
initDate <- '1993-01-29'
startDate <- '1993-02-22'
endDate <- '2015-12-31'
initEq <- 100000
filma <- 200
Sys.setenv(TZ="UTC") #timezone set
getSymbols('SPY', from = startDate, to = endDate, index.class = "POSIXct", adjust = T) #POSX time date index class
#SPY = to.monthly(SPY, indexAt = 'endof', drop.time = FALSE)
#SPY$SMA10m <- SMA(Cl(SPY), 10)

#initialise portfolio, accounts, orders, strategy
momentumstrat <- "RSI" 
rm.strat(momentumstrat) #if this is a re run remove strategy
initPortf(momentumstrat, 'SPY', initDate = initDate)
initAcct(momentumstrat, portfolios = momentumstrat, initDate = initDate, initEq = initEq)
initOrders(portfolio = momentumstrat, initDate = initDate) #initialize order container
strategy(momentumstrat, store = TRUE) #initialize a new strategy object
pkstrat <- get.strategy(momentumstrat)

#MACD 
#macd <- MACD( Cl(SPY), 12, 26, 9, maType="EMA" )


#Calculating the histogram
#MACD_SIG = 100 * (EMA(x = Cl(SPY), n = 12) / EMA(x = Cl(SPY), n = 26))
#MACD_SIGL <- EMA(x <- MACD_SIG, n = 9)
#MACD_hist <- MACD_SIG - MACD_SIGL
#SPY <- cbind(SPY, MACD_hist)

#RSI indicator
add.indicator(momentumstrat, name = "RSI", 
              arguments = list(price = quote(Cl(mktdata))),
              label = "RSI")

#slow 200 day simple moving average
add.indicator(momentumstrat, name = "SMA", 
              arguments = list(x = quote(Cl(mktdata)), n = filma), 
              label = "filMA")
#MACD Indicator
#add.indicator(momentumstrat, name = "MACD",
#             arguments = list(x= quote(Cl(mktdata))), 
#             label='osc')

#add.signal(momentumstrat ,name = "sigThreshold",
#           arguments = list(column = "signal.osc", relationship = "gt", threshold = 0, cross = TRUE),
#           label = "signal.gt.zero")

#add.signal(momentumstrat, name = "sigThreshold",
#           arguments = list(column = "signal.osc", relationship = "lt", threshold = 0, cross = TRUE),
#           label = "signal.lt.zero")

# The first is when RSI is greater than 90
add.signal(strategy = momentumstrat, name="sigThreshold",
           arguments = list(threshold=90, column="RSI",relationship="gt", cross=TRUE),
           label="RSI.gt.90")
# The second is when RSI is less than 10
add.signal(strategy = momentumstrat, name="sigThreshold",
           arguments = list(threshold=10, column="RSI",relationship="lt",cross=TRUE),
           label="RSI.lt.10")

#signal Fast MA
add.signal(momentumstrat, name = "sigComparison",
           arguments = list(columns = c("Close", "filMA"), relationship = "gt"),
           label = "upswing")

add.signal(momentumstrat, name = "sigComparison",
           arguments = list(columns = c("Close", "filMA"), relationship = "lt"),
           label = "downswing")
add.signal(momentumstrat, name = "sigAND",
           arguments = list(columns = c("RSI.gt.90", "upswing"), cross = TRUE),
           label = "entryabove")

add.signal(momentumstrat, name = "sigAND",
           arguments = list(columns = c("RSI.lt.10", "downswing"), cross = TRUE),
           label = "entrybelow")

#Implement when both signals are active
#add.signal(momentumstrat, name = "sigAND",
#           arguments = list(columns = c("RSI.gt.90", "signal.gt.zero"), cross = TRUE),
#           label = "longentry")
#add.signal(momentumstrat, name = "sigAND",
#           arguments = list(columns = c("RSI.lt.10", "signal.lt.zero"), cross = TRUE),
#           label = "shortentry")

#add.rule(momentumstrat, name='ruleSignal',
#         arguments = list(sigcol="longentry", sigval = TRUE, ordertype='market', 
#                          orderside='long', TxnFees = "pennyPerShare", replace = FALSE, prefer = "Open"),
#         type='enter', label = "enterlong")
#add.rule(momentumstrat, name='ruleSignal',
#         arguments = list(sigcol="shortentry", sigval = TRUE, ordertype='market', 
#                          orderside='long', TxnFees = "pennyPerShare", replace = FALSE, prefer = "Open"),
#         type='enter', label = "entryshort")

#add.rule(strategy.pk, name='ruleSignal',
#         arguments = list(sigcol="entryabove", sigval = TRUE, ordertype='market', 
#                          orderside='long', replace = FALSE, prefer = "Open"),
#         type='enter', label = "longentry2")

#add.rule(strategy.pk, name='ruleSignal',
#        arguments = list(sigcol="entrybelow", sigval = TRUE, ordertype='market', 
 #                         orderside='short', replace = FALSE, prefer = "Open"),
 #        type='enter', label = "longentry2")
# we use osMaxPos to put trade on in layers, or to a maximum position.
# The first is to sell when the RSI crosses above the threshold
add.rule(strategy = momentumstrat, name='ruleSignal', 
         arguments = list(sigcol="entryabove", sigval=TRUE, orderqty=1000, 
                          ordertype='market', orderside='long', pricemethod='market', 
                          TxnFees = "pennyPerShare", replace=FALSE, osFUN=osMaxPos), type='enter', path.dep=TRUE)
add.rule(strategy = momentumstrat, name='ruleSignal', 
         arguments = list(sigcol="RSI.lt.10", sigval=TRUE, orderqty='all', 
                          ordertype='market', orderside='long', pricemethod='market', 
                          TxnFees = "pennyPerShare", replace=FALSE), type='exit', path.dep=TRUE)
# The second is to buy when the RSI crosses below the threshold
add.rule(strategy = momentumstrat, name='ruleSignal', 
         arguments = list(sigcol="entrybelow", sigval=TRUE, orderqty= -1000, 
                          ordertype='market', orderside='short', pricemethod='market', 
                          TxnFees = "pennyPerShare", replace=FALSE, osFUN=osMaxPos), type='enter', path.dep=TRUE)
add.rule(strategy = momentumstrat, name='ruleSignal', 
         arguments = list(sigcol="RSI.gt.90", sigval=TRUE, orderqty='all', 
                          ordertype='market', orderside='short', pricemethod='market', 
                          TxnFees = "pennyPerShare", replace=FALSE), type='exit', path.dep=TRUE)


#add.rule(momentumstrat, name = 'ruleSignal',
#         arguments = list(sigcol = "signal.gt.zero", 
#                          sigval = TRUE, 
#                          orderqty = 1000,
#                          ordertype = 'market', 
#                          orderside='long'),
#         type = 'enter', 
#         label = 'enter', 
#         storefun = FALSE)#

#add.rule(momentumstrat, name = 'ruleSignal',
#         arguments = list(sigcol = "signal.lt.zero", 
#                          sigval = TRUE, 
#                         orderqty = 'all',
#                          ordertype = 'market', 
#                          orderside = 'long'),
#         type = 'exit', 
#         label = 'exit')
#stoploss fixed at 5%
#add.rule(momentumstrat, name = 'ruleSignal', 
#         arguments = list(sigcol = "signal.gt.zero",
#                          sigval=TRUE, 
#                          orderqty='all', 
#                          ordertype='stoplimit', 
#                          orderside='long', 
#                          threshold=-.05,
#                          tmult=TRUE, 
#                          orderset='stoplossexit'),
#         type='chain', 
#         parent='enter', 
#         label='risk',
#         storefun=FALSE)
#setup max pos
addPosLimit(momentumstrat, "SPY", initDate, 300, 3 )

fastMA = 12
slowMA = 26
signalMA = 9
maType="EMA"
applyStrategy(momentumstrat , portfolios = momentumstrat,
              parameters=list(nFast=fastMA, nSlow=slowMA, nSig=signalMA,maType=maType),
              verbose=TRUE)

applyStrategy(strategy=momentumstrat , portfolios=momentumstrat, parameters=list(n=2 ) )
txn <- getTxns(Portfolio=momentumstrat, Symbol="SPY")

#update pf, account, equity
updatePortf(momentumstrat)
updateAcct(momentumstrat) 
updateEndEq(momentumstrat)

tstats <- t(tradeStats(momentumstrat))
perTradeStats(momentumstrat)

themelist<-chart_theme()
themelist$col$up.col<-'lightgreen'
themelist$col$dn.col<-'pink'
chart.Posn(Portfolio=momentumstrat,Symbol="SPY", theme=themelist)
plot(add_RSI(n=2, RSIup = 90, RSIdn = 10 ) )
add_MACD()
add_SMA(n = 200)
add_BBands()
ret1 <- PortfReturns(momentumstrat)
ret1$total <- rowSums(ret1)
charts.PerformanceSummary(ret1$total,geometric=FALSE,wealth.index=TRUE)

a <- getAccount(momentumstrat) #retrieving account summary
last(a$summary,5)

library(lattice)
xyplot(a$summary,type="h",col=4) 

equity <- a$summary$End.Eq #plot equity curve and performance chart
plot(equity,main="Faber Strategy Equity Curve")
ret <- Return.calculate(equity,method="log")
charts.PerformanceSummary(ret, colorset = bluefocus,
                          main="Faber Strategy Performance")

ob <- get.orderbook(momentumstrat)
#COMPARE TWO STRATEGIES - make one with MACD and one with RSI and compare
#check for chart.Correlation(if not put for pf)
#maybe compare 3/7 RSI with MACD 
