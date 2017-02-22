
#load the needed packages
require(blotter)
require(quantstrat)
require(lattice)
require(TTR)
require(IKTrading)


#Initialise currency, instruments, historical data
currency("USD")
stock("QQQ", currency = "USD", multiplier = 1)
initDate <- '1993-01-29'
startDate <- '1993-02-01'
endDate <- '2016-03-15'
initEq <- 100000
Sys.setenv(TZ="UTC") #timezone set
getSymbols('QQQ', from = startDate, to = endDate, index.class = "POSIXct", adjust = T) 
QQQ = to.monthly(QQQ, indexAt = 'endof', drop.time = FALSE)


#initialise portfolio, accounts, orders, strategy
momentumstrat <- "RSI" 
rm.strat(momentumstrat) #if this is a re run remove strategy
initPortf(momentumstrat, 'QQQ', initDate = initDate)
initAcct(momentumstrat, portfolios = momentumstrat, initDate = initDate, initEq = initEq)
initOrders(portfolio = momentumstrat, initDate = initDate) #initialize order container
strategy(momentumstrat, store = TRUE) #initialize a new strategy object
pkstrat <- get.strategy(momentumstrat)


#RSI indicator
add.indicator(momentumstrat, name = "RSI", 
              arguments = list(price = quote(Cl(mktdata))),
              label = "RSI")

# The first is when RSI is greater than 90
add.signal(strategy = momentumstrat, name="sigThreshold",
           arguments = list(threshold=90, column="RSI",relationship="gt", cross=TRUE),
           label="RSI.gt.90")
# The second is when RSI is less than 10
add.signal(strategy = momentumstrat, name="sigThreshold",
           arguments = list(threshold=10, column="RSI",relationship="lt",cross=TRUE),
           label="RSI.lt.10")

# we use osMaxPos to put trade on in layers, or to a maximum position.
# The first is to sell when the RSI crosses above the threshold
add.rule(strategy = momentumstrat, name='ruleSignal', 
         arguments = list(sigcol="RSI.gt.90", sigval=TRUE, orderqty=1000, 
                          ordertype='market', orderside='long', pricemethod='market', 
                          TxnFees = "pennyPerShare", replace=FALSE, osFUN=osMaxPos), type='enter', path.dep=TRUE, label = "longentry")
add.rule(strategy = momentumstrat, name='ruleSignal', 
         arguments = list(sigcol="RSI.lt.10", sigval=TRUE, orderqty='all', 
                          ordertype='market', orderside='long', pricemethod='market', 
                          TxnFees = "pennyPerShare", replace=FALSE), type='exit', path.dep=TRUE)
# The second is to buy when the RSI crosses below the threshold
add.rule(strategy = momentumstrat, name='ruleSignal', 
         arguments = list(sigcol="RSI.lt.10", sigval=TRUE, orderqty= -1000, 
                          ordertype='market', orderside='short', pricemethod='market', 
                          TxnFees = "pennyPerShare", replace=FALSE, osFUN=osMaxPos), type='enter', path.dep=TRUE, label = "shortentry")
add.rule(strategy = momentumstrat, name='ruleSignal', 
         arguments = list(sigcol="RSI.gt.90", sigval=TRUE, orderqty='all', 
                          ordertype='market', orderside='short', pricemethod='market', 
                          TxnFees = "pennyPerShare", replace=FALSE), type='exit', path.dep=TRUE)



#stoploss fixed at 5%
add.rule(momentumstrat, name = 'ruleSignal', 
         arguments = list(sigcol = "RSI.gt.90",
                          sigval=TRUE, 
                          orderqty='all', 
                          ordertype='stoplimit', 
                          orderside='long', 
                          threshold=-.05,
                          tmult=TRUE, 
                          orderset='stoplossexit'),
         type='chain', 
         parent='longentry', 
         label='risk',
         storefun=FALSE)
add.rule(momentumstrat, name = 'ruleSignal', 
         arguments = list(sigcol = "RSI.lt.10",
                          sigval=TRUE, 
                          orderqty='all', 
                          ordertype='stoplimit', 
                          orderside='short', 
                          threshold=-.05,
                          tmult=TRUE, 
                          orderset='sstoplossexit'),
         type='chain', 
         parent='shortentry', 
         label='risk',
         storefun=FALSE)

#setup max pos
addPosLimit(momentumstrat, "QQQ", initDate, 300, 3 )



applyStrategy(strategy=momentumstrat , portfolios=momentumstrat, parameters=list(n=2) )
txn <- getTxns(Portfolio=momentumstrat, Symbol="QQQ")

#update pf, account, equity
updatePortf(momentumstrat)
updateAcct(momentumstrat) 
updateEndEq(momentumstrat)

themelist<-chart_theme()
themelist$col$up.col<-'lightgreen'
themelist$col$dn.col<-'pink'
chart.Posn(Portfolio=momentumstrat,Symbol="QQQ", theme=themelist)
plot(add_RSI(n=2, RSIup = 90, RSIdn = 10 ) )


#Out of Sample---------------------------------------------------------------------------------------------------------
#Initialise currency, instruments, historical data
currency("USD")
stock("SPY", currency = "USD", multiplier = 1)
initDate <- '2003-12-30'
startDate <- '2003-12-31'
endDate <- '2016-03-15'
initEq <- 100000
Sys.setenv(TZ="UTC") #timezone set
getSymbols('SPY', from = startDate, to = endDate, index.class = "POSIXct", adjust = T) 
SPY = to.monthly(SPY, indexAt = 'endof', drop.time = FALSE)


#initialise portfolio, accounts, orders, strategy
momentumstrat <- "RSI" 
rm.strat(momentumstrat) #if this is a re run remove strategy
initPortf(momentumstrat, 'SPY', initDate = initDate)
initAcct(momentumstrat, portfolios = momentumstrat, initDate = initDate, initEq = initEq)
initOrders(portfolio = momentumstrat, initDate = initDate) #initialize order container
strategy(momentumstrat, store = TRUE) #initialize a new strategy object
pkstrat <- get.strategy(momentumstrat)


#RSI indicator
add.indicator(momentumstrat, name = "RSI", 
              arguments = list(price = quote(Cl(mktdata))),
              label = "RSI")

# The first is when RSI is greater than 90
add.signal(strategy = momentumstrat, name="sigThreshold",
           arguments = list(threshold=90, column="RSI",relationship="gt", cross=TRUE),
           label="RSI.gt.90")
# The second is when RSI is less than 10
add.signal(strategy = momentumstrat, name="sigThreshold",
           arguments = list(threshold=10, column="RSI",relationship="lt",cross=TRUE),
           label="RSI.lt.10")

# we use osMaxPos to put trade on in layers, or to a maximum position.
# The first is to sell when the RSI crosses above the threshold
add.rule(strategy = momentumstrat, name='ruleSignal', 
         arguments = list(sigcol="RSI.gt.90", sigval=TRUE, orderqty=1000, 
                          ordertype='market', orderside='long', pricemethod='market', 
                          TxnFees = "pennyPerShare", replace=FALSE, osFUN=osMaxPos), type='enter', path.dep=TRUE, label = "longentry")
add.rule(strategy = momentumstrat, name='ruleSignal', 
         arguments = list(sigcol="RSI.lt.10", sigval=TRUE, orderqty='all', 
                          ordertype='market', orderside='long', pricemethod='market', 
                          TxnFees = "pennyPerShare", replace=FALSE), type='exit', path.dep=TRUE)
# The second is to buy when the RSI crosses below the threshold
add.rule(strategy = momentumstrat, name='ruleSignal', 
         arguments = list(sigcol="RSI.lt.10", sigval=TRUE, orderqty= -1000, 
                          ordertype='market', orderside='short', pricemethod='market', 
                          TxnFees = "pennyPerShare", replace=FALSE, osFUN=osMaxPos), type='enter', path.dep=TRUE, label = "shortentry")
add.rule(strategy = momentumstrat, name='ruleSignal', 
         arguments = list(sigcol="RSI.gt.90", sigval=TRUE, orderqty='all', 
                          ordertype='market', orderside='short', pricemethod='market', 
                          TxnFees = "pennyPerShare", replace=FALSE), type='exit', path.dep=TRUE)



#stoploss fixed at 5%
add.rule(momentumstrat, name = 'ruleSignal', 
         arguments = list(sigcol = "RSI.gt.90",
                          sigval=TRUE, 
                          orderqty='all', 
                          ordertype='stoplimit', 
                          orderside='long', 
                          threshold=-.05,
                          tmult=TRUE, 
                          orderset='stoplossexit'),
         type='chain', 
         parent='longentry', 
         label='risk',
         storefun=FALSE)
add.rule(momentumstrat, name = 'ruleSignal', 
         arguments = list(sigcol = "RSI.lt.10",
                          sigval=TRUE, 
                          orderqty='all', 
                          ordertype='stoplimit', 
                          orderside='short', 
                          threshold=-.05,
                          tmult=TRUE, 
                          orderset='sstoplossexit'),
         type='chain', 
         parent='shortentry', 
         label='risk',
         storefun=FALSE)

#setup max pos
addPosLimit(momentumstrat, "SPY", initDate, 300, 3 )



applyStrategy(strategy=momentumstrat , portfolios=momentumstrat, parameters=list(n=2) )
txn <- getTxns(Portfolio=momentumstrat, Symbol="SPY")

#update pf, account, equity
updatePortf(momentumstrat)
updateAcct(momentumstrat) 
updateEndEq(momentumstrat)

themelist<-chart_theme()
themelist$col$up.col<-'lightgreen'
themelist$col$dn.col<-'pink'
chart.Posn(Portfolio=momentumstrat,Symbol="SPY", theme=themelist)
plot(add_RSI(n=2, RSIup = 90, RSIdn = 10 ) )
add_MACD()     



#Buy and hold
getSymbols('SPY', from = startDate, to = endDate)
buyhold <- 1000*SPY$SPY.Adjusted
buyholdr <- monthlyReturn(buyhold) 
table.AnnualizedReturns(perf)
perf <- cbind(buyholdr, ret1$total)
charts.PerformanceSummary(perf)
#COMPARE CUM RETURN AND DRAWDOWN sharpe etc

tstats <- t(tradeStats(momentumstrat))
perTradeStats(momentumstrat)


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
