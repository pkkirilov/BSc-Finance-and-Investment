
#Install the needed packages
#install.packages("quantstrat", repos="http://R-Forge.R-project.org")
#install.packages("devtools")
#require(devtools)
#install_github("IlyaKipnis/IKTrading")
#install.packages("TTR")

#load the needed packages
require(blotter)
require(quantstrat)
require(TTR)
require(IKTrading)

#In-Sample tesing period***************************************************************************
#Initialise currency, instruments, historical data
currency("USD")
stock("SPY", currency = "USD", multiplier = 1)
initDate <- '1993-01-29'
startDate <- '1993-02-01'
endDate <- '2003-12-31'
initEq <- 1000
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
                       arguments = list(threshold=90, 
                                        column="RSI",
                                        relationship="gt", 
                                        cross=TRUE),
                       label="RSI.gt.90")
# The second is when RSI is less than 10
add.signal(strategy = momentumstrat, name="sigThreshold",
                       arguments = list(threshold=10, 
                                        column="RSI",
                                        relationship="lt",
                                        cross=TRUE),
                       label="RSI.lt.10")


#Buy when the RSI crosses above the threshold
add.rule(strategy = momentumstrat, name='ruleSignal', 
                     arguments = list(sigcol="RSI.gt.90", 
                                      sigval=TRUE, 
                                      orderqty=10, 
                                      ordertype='market', 
                                      orderside='long', 
                                      pricemethod='market', 
                                      TxnFees = "pennyPerShare", 
                                      replace=FALSE, 
                                      osFUN=osMaxPos), 
         type='enter', path.dep=TRUE, label = "longentry")
add.rule(strategy = momentumstrat, name='ruleSignal', 
                     arguments = list(sigcol="RSI.lt.10", 
                                      sigval=TRUE, 
                                      orderqty='all', 
                                      ordertype='market', 
                                      orderside='long', 
                                      pricemethod='market', 
                                      TxnFees = "pennyPerShare", 
                                      replace=FALSE), 
         type='exit', path.dep=TRUE)
#Sell when the RSI crosses below the threshold
add.rule(strategy = momentumstrat, name='ruleSignal', 
                     arguments = list(sigcol="RSI.lt.10", 
                                      sigval=TRUE, orderqty= -10, 
                                      ordertype='market', 
                                      orderside='short', 
                                      pricemethod='market', 
                                      TxnFees = "pennyPerShare", 
                                      replace=FALSE, 
                                      osFUN=osMaxPos), 
         type='enter', path.dep=TRUE, label = "shortentry")
add.rule(strategy = momentumstrat, name='ruleSignal', 
                     arguments = list(sigcol="RSI.gt.90", 
                                      sigval=TRUE, 
                                      orderqty='all', 
                                      ordertype='market', 
                                      orderside='short', 
                                      pricemethod='market', 
                                      TxnFees = "pennyPerShare", 
                                      replace=FALSE), 
         type='exit', path.dep=TRUE)


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

# we use osMaxPos to put trade on in layers, or to a maximum position.
#setup max pos
addPosLimit(momentumstrat, "SPY", initDate, 30, 3 )
applyStrategy(strategy=momentumstrat , portfolios=momentumstrat, parameters=list(n=2) )
txn <- getTxns(Portfolio=momentumstrat, Symbol="SPY")

#update pf, account, equity
updatePortf(momentumstrat)
updateAcct(momentumstrat) 
updateEndEq(momentumstrat)

#create custom chart with positions and MACD
themelist<-chart_theme()
themelist$col$dn.col<-'lightblue'
themelist$col$dn.border <- 'lightgray'
themelist$col$dn.col<-'pink'
themelist$col$up.border <- 'lightgray'
chart.Posn(Portfolio=momentumstrat,Symbol="SPY", theme=themelist)
plot(add_RSI(n=2, RSIup = 90, RSIdn = 10 ) )
add_MACD()   

#strategy Sharpe and statistics
trdstat <- tradeStats(Portfolios = momentumstrat, use = "trades", inclZeroDays = FALSE)
print(trdstat)
portret <- PortfReturns(momentumstrat)
SharpeRatio.annualized(portret)
maxDrawdown(portret)
ob <- get.orderbook(momentumstrat) #get the order book

     
#Out-of-Sample---------------------------------------------------------------------------------------------------------
     #Initialise currency, instruments, historical data
     currency("USD")
     stock("SPY", currency = "USD", multiplier = 1)
     initDate <- '2003-12-30'
     startDate <- '2003-12-31'
     endDate <- '2016-03-15'
     initEq <- 1000
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
                arguments = list(threshold=90, 
                                 column="RSI",
                                 relationship="gt", 
                                 cross=TRUE),
                label="RSI.gt.90")
     # The second is when RSI is less than 10
     add.signal(strategy = momentumstrat, name="sigThreshold",
                arguments = list(threshold=10, 
                                 column="RSI",
                                 relationship="lt",
                                 cross=TRUE),
                label="RSI.lt.10")
     
     # Buy when the RSI crosses above the threshold
     add.rule(strategy = momentumstrat, name='ruleSignal', 
              arguments = list(sigcol="RSI.gt.90", 
                               sigval=TRUE, 
                               orderqty=10, 
                               ordertype='market', 
                               orderside='long', 
                               pricemethod='market', 
                               TxnFees = "pennyPerShare", 
                               replace=FALSE, 
                               osFUN=osMaxPos), 
              type='enter', 
              path.dep=TRUE, 
              label = "longentry")
     add.rule(strategy = momentumstrat, name='ruleSignal', 
              arguments = list(sigcol="RSI.lt.10", 
                               sigval=TRUE, 
                               orderqty='all', 
                               ordertype='market', 
                               orderside='long', 
                               pricemethod='market', 
                               TxnFees = "pennyPerShare", 
                               replace=FALSE), 
              type='exit', path.dep=TRUE)
     # Sell when the RSI crosses below the threshold
     add.rule(strategy = momentumstrat, name='ruleSignal', 
              arguments = list(sigcol="RSI.lt.10", 
                               sigval=TRUE, 
                               orderqty= -10, 
                               ordertype='market', 
                               orderside='short', 
                               pricemethod='market', 
                               TxnFees = "pennyPerShare", 
                               replace=FALSE, 
                               osFUN=osMaxPos), 
              type='enter', path.dep=TRUE, label = "shortentry")
     add.rule(strategy = momentumstrat, name='ruleSignal', 
              arguments = list(sigcol="RSI.gt.90", 
                               sigval=TRUE, 
                               orderqty='all', 
                               ordertype='market', 
                               orderside='short', 
                               pricemethod='market', 
                               TxnFees = "pennyPerShare",
                               replace=FALSE), 
              type='exit', path.dep=TRUE)
     
     
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
     
     #setup maximum position
     addPosLimit(momentumstrat, "SPY", initDate, 30, 3 )
     
     
     
     applyStrategy(strategy=momentumstrat , portfolios=momentumstrat, parameters=list(n=2) )
     txn <- getTxns(Portfolio=momentumstrat, Symbol="SPY")
     
     #update pf, account, equity
     updatePortf(momentumstrat)
     updateAcct(momentumstrat) 
     updateEndEq(momentumstrat)
     
     #create chart for strategy and MACD
     
     themelist<-chart_theme()
     themelist$col$dn.col<-'lightblue'
     themelist$col$dn.border <- 'lightgray'
     themelist$col$dn.col<-'pink'
     themelist$col$up.border <- 'lightgray'
     chart.Posn(Portfolio=momentumstrat,Symbol="SPY", theme=themelist)
     plot(add_RSI(n=2, RSIup = 90, RSIdn = 10 ) )
     add_MACD()  
     #strategy Sharpe and statistics
     trdstat <- tradeStats(Portfolios = momentumstrat, use = "trades", inclZeroDays = FALSE)    
     print(trdstat)
     portret <- PortfReturns(momentumstrat)
     SharpeRatio.annualized(portret)
     maxDrawdown(portret)


#Buy and hold In-sample***********************************************************************************
getSymbols('SPY', from = "1993-02-25", to = "2003-12-31")
buyhold <- 30*SPY$SPY.Adjusted
buyholdr <- monthlyReturn(buyhold)
SharpeRatio.annualized(buyholdr)
maxDrawdown(buyholdr)

#Buy and hold Out-of-sample********************************************************************************
getSymbols('SPY', from = "2003-12-31", to = "2016-03-15")
buyhold <- 30*SPY$SPY.Adjusted
buyholdr <- monthlyReturn(buyhold)
SharpeRatio.annualized(buyholdr)
maxDrawdown(buyholdr)


#AAPL strategy test**************************************************************************************************

#Initialise currency, instruments, historical data
currency("USD")
stock("AAPL", currency = "USD", multiplier = 1)
initDate <- '2005-01-30'
startDate <- '2005-01-31'
endDate <- '2016-03-15'
initEq <- 1000
Sys.setenv(TZ="UTC") #timezone set
getSymbols('AAPL', from = startDate, to = endDate, index.class = "POSIXct", adjust = T) 
AAPL = to.monthly(AAPL, indexAt = 'endof', drop.time = FALSE)


#initialise portfolio, accounts, orders, strategy
momentumstrat <- "RSI" 
rm.strat(momentumstrat) #if this is a re run remove strategy
initPortf(momentumstrat, 'AAPL', initDate = initDate)
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
           arguments = list(threshold=90, 
                            column="RSI",
                            relationship="gt", 
                            cross=TRUE),
           label="RSI.gt.90")
# The second is when RSI is less than 10
add.signal(strategy = momentumstrat, name="sigThreshold",
           arguments = list(threshold=10, 
                            column="RSI",
                            relationship="lt",
                            cross=TRUE),
           label="RSI.lt.10")


#Buy when the RSI crosses above the threshold
add.rule(strategy = momentumstrat, name='ruleSignal', 
         arguments = list(sigcol="RSI.gt.90", 
                          sigval=TRUE, 
                          orderqty=10, 
                          ordertype='market', 
                          orderside='long', 
                          pricemethod='market', 
                          TxnFees = "pennyPerShare", 
                          replace=FALSE, 
                          osFUN=osMaxPos), 
         type='enter', path.dep=TRUE, label = "longentry")
add.rule(strategy = momentumstrat, name='ruleSignal', 
         arguments = list(sigcol="RSI.lt.10", 
                          sigval=TRUE, 
                          orderqty='all', 
                          ordertype='market', 
                          orderside='long', 
                          pricemethod='market', 
                          TxnFees = "pennyPerShare", 
                          replace=FALSE), 
         type='exit', path.dep=TRUE)
#Sell when the RSI crosses below the threshold
add.rule(strategy = momentumstrat, name='ruleSignal', 
         arguments = list(sigcol="RSI.lt.10", 
                          sigval=TRUE, 
                          orderqty= -10, 
                          ordertype='market', 
                          orderside='short', 
                          pricemethod='market', 
                          TxnFees = "pennyPerShare", 
                          replace=FALSE, 
                          osFUN=osMaxPos), 
         type='enter', path.dep=TRUE, label = "shortentry")
add.rule(strategy = momentumstrat, name='ruleSignal', 
         arguments = list(sigcol="RSI.gt.90", 
                          sigval=TRUE, 
                          orderqty='all', 
                          ordertype='market', 
                          orderside='short', 
                          pricemethod='market', 
                          TxnFees = "pennyPerShare", 
                          replace=FALSE), 
         type='exit', path.dep=TRUE)


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

#setup maximum position
addPosLimit(momentumstrat, "AAPL", initDate, 30, 3 )
applyStrategy(strategy=momentumstrat , portfolios=momentumstrat, parameters=list(n=2) )
txn <- getTxns(Portfolio=momentumstrat, Symbol="AAPL")

#update pf, account, equity
updatePortf(momentumstrat)
updateAcct(momentumstrat) 
updateEndEq(momentumstrat)

#create chart for strategy and MACD
themelist<-chart_theme()
themelist$col$dn.col<-'lightblue'
themelist$col$dn.border <- 'lightgray'
themelist$col$dn.col<-'pink'
themelist$col$up.border <- 'lightgray'
chart.Posn(Portfolio=momentumstrat,Symbol="AAPL", theme=themelist)
plot(add_RSI(n=2, RSIup = 90, RSIdn = 10 ) )
add_MACD() 
#strategy Sharpe and statistics
trdstat <- tradeStats(Portfolios = momentumstrat, use = "trades", inclZeroDays = FALSE)  
print(trdstat)
portret <- PortfReturns(momentumstrat)
SharpeRatio.annualized(portret)
maxDrawdown(portret)

#Buy and hold AAPL***********************************************************************************************
getSymbols('AAPL', from = "2005-01-31", to = "2016-03-15")
buyhold <- 30*AAPL$AAPL.Adjusted
buyholdr <- monthlyReturn(buyhold)
SharpeRatio.annualized(buyholdr)
maxDrawdown(buyholdr)

#Testing with QQQ**************************************************************************************************
#Initialise currency, instruments, historical data
currency("USD")
stock("QQQ", currency = "USD", multiplier = 1)
initDate <- '2000-01-29'
startDate <- '2000-02-01'
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
#plot positions
themelist<-chart_theme()
themelist$col$dn.col<-'lightblue'
themelist$col$dn.border <- 'lightgray'
themelist$col$dn.col<-'pink'
themelist$col$up.border <- 'lightgray'
chart.Posn(Portfolio=momentumstrat,Symbol="QQQ", theme=themelist)
plot(add_RSI(n=2, RSIup = 90, RSIdn = 10 ) )
add_MACD()  
#strategy Sharpe and statistics
trdstat <- tradeStats(Portfolios = momentumstrat, use = "trades", inclZeroDays = FALSE)
print(trdstat)
portret <- PortfReturns(momentumstrat)
SharpeRatio.annualized(portret)
maxDrawdown(portret)

#Buy and hold AAPL***********************************************************************************************
getSymbols('QQQ', from = "2000-02-29", to = "2016-03-15")
buyhold <- 30*QQQ$QQQ.Adjusted
buyholdr <- monthlyReturn(buyhold)
SharpeRatio.annualized(buyholdr)
maxDrawdown(buyholdr)