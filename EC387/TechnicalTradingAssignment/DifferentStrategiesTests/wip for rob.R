
#seems like the two cannot be made to work together
#load the needed packages
require(blotter)
require(quantstrat)
require(lattice)
require(TTR)
require(IKTrading)


#Initialise currency, instruments, historical data
currency("USD")
stock("MRW.L", currency = "USD", multiplier = 1)
initDate <- '1993-01-29'
startDate <- '1993-02-01'
endDate <- '2003-12-31'
initEq <- 100000
Sys.setenv(TZ="UTC") #timezone set
getSymbols('MRW.L', from = startDate, to = endDate, index.class = "POSIXct", adjust = T) #POSX time date index class
MRW.L = to.weekly(MRW.L, indexAt = 'endof', drop.time = FALSE)
#MRW.L$SMA10m <- SMA(Cl(MRW.L), 10)

#initialise portfolio, accounts, orders, strategy
strategy.alh <- "MACD" 
rm.strat(strategy.alh) #if this is a re run remove strategy
initPortf(strategy.alh, 'MRW.L', initDate = initDate)
initAcct(strategy.alh, portfolios = strategy.alh, initDate = initDate, initEq = initEq)
initOrders(portfolio = strategy.alh, initDate = initDate) #initialize order container
strategy(strategy.alh, store = TRUE) #initialize a new strategy object
strategya <- get.strategy(strategy.alh)

#MACD 
#macd <- MACD( Cl(MRW.L), 12, 26, 9, maType="EMA" )


#Calculating the histogram
#MACD_SIG = 100 * (EMA(x = Cl(MRW.L), n = 12) / EMA(x = Cl(MRW.L), n = 26))
#MACD_SIGL <- EMA(x <- MACD_SIG, n = 9)
#MACD_hist <- MACD_SIG - MACD_SIGL
#MRW.L <- cbind(MRW.L, MACD_hist)

#RSI indicator
#add.indicator(strategy.alh, name = "RSI", 
#              arguments = list(price = quote(Cl(mktdata))),
#              label = "RSI")
#MACD Indicator
add.indicator(strategy.alh, name = "MACD",
             arguments = list(x= quote(Cl(mktdata))), 
             label='osc')

add.signal(strategy.alh ,name = "sigThreshold",
           arguments = list(column = "signal.osc", relationship = "gt", threshold = 0, cross = TRUE),
           label = "signal.gt.zero")

add.signal(strategy.alh, name = "sigThreshold",
           arguments = list(column = "signal.osc", relationship = "lt", threshold = 0, cross = TRUE),
           label = "signal.lt.zero")

# The first is when RSI is greater than 90
#add.signal(strategy = strategy.alh, name="sigThreshold",
#           arguments = list(threshold=90, column="RSI",relationship="gt", cross=TRUE),
#           label="RSI.gt.90")
# The second is when RSI is less than 10
#add.signal(strategy = strategy.alh, name="sigThreshold",
#           arguments = list(threshold=10, column="RSI",relationship="lt",cross=TRUE),
#           label="RSI.lt.10")
#Implement when both signals are active
#add.signal(strategy.alh, name = "sigAND",
#           arguments = list(columns = c("RSI.gt.90", "signal.gt.zero"), cross = TRUE),
#           label = "longentry")
#add.signal(strategy.alh, name = "sigAND",
#           arguments = list(columns = c("RSI.lt.10", "signal.lt.zero"), cross = TRUE),
#           label = "shortentry")

#add.rule(strategy.alh, name='ruleSignal',
#         arguments = list(sigcol="longentry", sigval = TRUE, ordertype='market', 
 #                         orderside='long', TxnFees = "pennyPerShare", replace = FALSE, prefer = "Open"),
#         type='enter', label = "enterlong")
#add.rule(strategy.alh, name='ruleSignal',
#         arguments = list(sigcol="shortentry", sigval = TRUE, ordertype='market', 
#                          orderside='long', TxnFees = "pennyPerShare", replace = FALSE, prefer = "Open"),
#         type='enter', label = "entryshort")
# we use osMaxPos to put trade on in layers, or to a maximum position.
# The first is to sell when the RSI crosses above the threshold
#add.rule(strategy = strategy.alh, name='ruleSignal', 
#         arguments = list(sigcol="RSI.gt.90", sigval=TRUE, orderqty=1000, 
#                          ordertype='market', orderside='long', pricemethod='market', 
#                          TxnFees = "pennyPerShare", replace=FALSE, osFUN=osMaxPos), type='enter', path.dep=TRUE, label = "longentry")
#add.rule(strategy = strategy.alh, name='ruleSignal', 
##         arguments = list(sigcol="RSI.lt.10", sigval=TRUE, orderqty='all', 
#                          ordertype='market', orderside='long', pricemethod='market', 
#                          TxnFees = "pennyPerShare", replace=FALSE), type='exit', path.dep=TRUE)
# The second is to buy when the RSI crosses below the threshold
#add.rule(strategy = strategy.alh, name='ruleSignal', 
#         arguments = list(sigcol="RSI.lt.10", sigval=TRUE, orderqty= -1000, 
#                          ordertype='market', orderside='short', pricemethod='market', 
#                          TxnFees = "pennyPerShare", replace=FALSE, osFUN=osMaxPos), type='enter', path.dep=TRUE, label = "shortentry")
#add.rule(strategy = strategy.alh, name='ruleSignal', 
#         arguments = list(sigcol="RSI.gt.90", sigval=TRUE, orderqty='all', 
#                          ordertype='market', orderside='short', pricemethod='market', 
#                          TxnFees = "pennyPerShare", replace=FALSE), type='exit', path.dep=TRUE)


add.rule(strategy.alh, name = 'ruleSignal',
         arguments = list(sigcol = "signal.gt.zero", 
                          sigval = TRUE, 
                          orderqty = 1000,
                          ordertype = 'market', 
                          orderside='long'),
         type = 'enter', 
         label = 'enter', 
         storefun = FALSE)

add.rule(strategy.alh, name = 'ruleSignal',
         arguments = list(sigcol = "signal.lt.zero", 
                          sigval = TRUE, 
                         orderqty = 'all',
                          ordertype = 'market', 
                          orderside = 'long'),
         type = 'exit', 
         label = 'exit')
#stoploss fixed at 5%
#add.rule(strategy.alh, name = 'ruleSignal', 
#         arguments = list(sigcol = "RSI.gt.90",
#                          sigval=TRUE, 
#                          orderqty='all', 
#                          ordertype='stoplimit', 
#                          orderside='long', 
#                          threshold=-.05,
#                          tmult=TRUE, 
#                          orderset='stoplossexit'),
#         type='chain', 
#         parent='longentry', 
#         label='risk',
#         storefun=FALSE)
#add.rule(strategy.alh, name = 'ruleSignal', 
#         arguments = list(sigcol = "RSI.lt.10",
#                          sigval=TRUE, 
#                          orderqty='all', 
#                          ordertype='stoplimit', 
#                          orderside='short', 
#                          threshold=-.05,
#                          tmult=TRUE, 
#                          orderset='sstoplossexit'),
#         type='chain', 
#         parent='shortentry', 
#         label='risk',
#         storefun=FALSE)


fastMA = 12
slowMA = 26
signalMA = 9
maType="EMA"
applyStrategy(strategy.alh , portfolios = strategy.alh,
              parameters=list(nFast=fastMA, nSlow=slowMA, nSig=signalMA,maType=maType),
              verbose=TRUE)

#applyStrategy(strategy=strategy.alh , portfolios=strategy.alh, parameters=list(n=2) )
txn <- getTxns(Portfolio=strategy.alh, Symbol="MRW.L")

#update pf, account, equity
updatePortf(strategy.alh)
updateAcct(strategy.alh) 
updateEndEq(strategy.alh)

themelist<-chart_theme()
themelist$col$up.col<-'lightgreen'
themelist$col$dn.col<-'pink'
chart.Posn(Portfolio=strategy.alh,Symbol="MRW.L", theme=themelist)
plot(add_RSI(n=2, RSIup = 90, RSIdn = 10 ) )
add_MACD()












#Buy and hold
getSymbols('MRW.L', from = startDate, to = endDate)
#COMPARE CUM RETURN AND DRAWDOWN sharpe etc

tstats <- t(tradeStats(strategy.alh))
perTradeStats(strategy.alh)


ret1 <- PortfReturns(strategy.alh)
ret1$MRW.Lal <- rowSums(ret1)
charts.PerformanceSummary(ret1$MRW.Lal,geometric=FALSE,wealth.index=TRUE)

a <- getAccount(strategy.alh) #retrieving account summary
last(a$summary,5)

library(lattice)
xyplot(a$summary,type="h",col=4) 

equity <- a$summary$End.Eq #plot equity curve and performance chart
plot(equity,main="Faber Strategy Equity Curve")
ret <- Return.calculate(equity,method="log")
charts.PerformanceSummary(ret, colorset = bluefocus,
                          main="Faber Strategy Performance")

ob <- get.orderbook(strategy.alh)
#COMPARE TWO STRATEGIES - make one with MACD and one with RSI and compare
#check for chart.Correlation(if not put for pf)
#maybe compare 3/7 RSI with MACD 








require(quantstrat)

currency("USD")
stock("MRW.L", currency = "USD", multiplier = 1)
initDate <- '1993-01-29'
startDate <- '1993-02-01'
endDate <- '2003-12-31'
initEq <- 100000
Sys.setenv(TZ="UTC") #timezone set
getSymbols('MRW.L', from = startDate, to = endDate, index.class = "POSIXct", adjust = T) #POSX time date index class
MRW.L = to.monthly(MRW.L, indexAt = 'endof', drop.time = FALSE)

#MA parameters for MACD
fastMA = 12 
slowMA = 26 
signalMA = 9
maType="EMA"

portfolio.st='macd'
account.st='macd'

initPortf(portfolio.st,symbols=stock.str)
initAcct(account.st,portfolios=portfolio.st)
initOrders(portfolio=portfolio.st)

strat.st<-portfolio.st
# define the strategy
strategy(strat.st, store=TRUE)

#one indicator
add.indicator(strat.st, name = "MACD", 
              arguments = list(x=quote(Cl(mktdata)),
                               nFast=fastMA, 
                               nSlow=slowMA),
              label='_' 
)

#two signals
add.signal(strat.st,name="sigThreshold",
           arguments = list(column="signal._",
                            relationship="gt",
                            threshold=0,
                            cross=TRUE),
           label="signal.gt.zero"
)

add.signal(strat.st,name="sigThreshold",
           arguments = list(column="signal._",
                            relationship="lt",
                            threshold=0,
                            cross=TRUE),
           label="signal.lt.zero"
)

####
# add rules

# entry
add.rule(strat.st,name='ruleSignal', 
         arguments = list(sigcol="signal.gt.zero",
                          sigval=TRUE, 
                          orderqty=100, 
                          ordertype='market', 
                          orderside='long', 
                          threshold=NULL),
         type='enter',
         label='enter',
         storefun=FALSE
)

#alternatives for risk stops:
# simple stoplimit order, with threshold multiplier
#add.rule(strat.st,name='ruleSignal', arguments = list(sigcol="signal.gt.zero",sigval=TRUE, orderqty='all', ordertype='stoplimit', orderside='long', threshold=-.05,tmult=TRUE, orderset='exit2'),type='chain', parent='enter', label='risk',storefun=FALSE)
# alternately, use a trailing order, also with a threshold multiplier
#add.rule(strat.st,name='ruleSignal', arguments = list(sigcol="signal.gt.zero",sigval=TRUE, orderqty='all', ordertype='stoptrailing', orderside='long', threshold=-1,tmult=FALSE, orderset='exit2'),	type='chain', parent='enter', label='trailingexit')

# exit
add.rule(strat.st,name='ruleSignal', 
         arguments = list(sigcol="signal.lt.zero",
                          sigval=TRUE, 
                          orderqty='all', 
                          ordertype='market', 
                          orderside='long', 
                          threshold=NULL,
                          orderset='exit2'),
         type='exit',
         label='exit'
)

#end rules
####

#getSymbols("MRW.L",from=startDate, to='2014-06-01')
out<-applyStrategy(strat.st , portfolios=portfolio.st,parameters=list(nFast=fastMA, nSlow=slowMA, nSig=signalMA,maType=maType),verbose=TRUE)



chart.Posn(Portfolio=portfolio.st,Symbol="MRW.L")
plot(add_MACD(fast=fastMA, slow=slowMA, signal=signalMA,maType="EMA"))

#look at the order book
obook<-getOrderBook('macd')

# set tz as it was before the demo
Sys.setenv(TZ=oldtz)
