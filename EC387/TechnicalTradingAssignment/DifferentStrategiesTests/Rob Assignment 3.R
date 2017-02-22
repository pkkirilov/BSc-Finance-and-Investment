#load the needed packages
require(blotter)
require(quantstrat)
require(lattice)
require(TTR)
require(IKTrading)


#Initialise currency, instruments, historical data
currency("USD")
stock("SPY", currency = "USD", multiplier = 1)
initDate <- '1993-01-29'
startDate <- '1993-02-01'
endDate <- '2015-12-31'
initEq <- 100000
Sys.setenv(TZ="UTC") #timezone set
getSymbols('SPY', from = startDate, to = endDate, index.class = "POSIXct", adjust = T) #POSX time date index class
SPY = to.monthly(SPY, indexAt = 'endof', drop.time = FALSE)
SPY$SMA10m <- SMA(Cl(SPY), 10)

#parameters
fma = 10
filma = 200
rsindex = 2
thre1 = 70
thre2 = 30


#Load the required data from Yahoo manually
#SPDR <- read.csv("D:/University of Brighton/Year 4/EC387 - Applied Finance Techniques 2/Assignment 3/SPYDailydata.csv", header = T)
#SPDR$Date <- as.POSIXct(SPDR$Date)
#SPDR <- xts(SPDR[,-1], order.by = SPDR[,1]) #RABOTI
#names <- c("SPY.Open", "SPY.High", "SPY.Low", "SPY.Close", "SPY.Volume", "SPY.Adjusted")
#colnames(SPDR) <- names
#Sys.setenv(TZ="UTC") #timezone set
#colnames(SPDR) <- c("SPY.Open", "SPY.High", "SPY.Low", "SPY.Close", "SPY.Volume", "SPY.Adjusted")
#SPDR = to.monthly(SPDR, indexAt = 'endof', drop.time = FALSE)
#SPDR$SMA10m <- SMA(Cl(SPDR), 10)
#supermarket <- read.csv("supermarket.csv")
#supermarket$Date <- as.POSIXct(supermarket$Date)
#supermarket <- xts(supermarket[,-1], order.by=supermarket[,1])
#is.xts(supermarket)


#initialise portfolio, accounts, orders, strategy
strategy.pk <- "fin" 
rm.strat(strategy.pk) #if this is a re run remove strategy
initPortf(strategy.pk, 'SPY', initDate = initDate)
initAcct(strategy.pk, portfolios = strategy.pk, initDate = initDate, initEq = initEq)
initOrders(portfolio = strategy.pk, initDate = initDate) #initialize order container
strategy(strategy.pk, store = TRUE) #initialize a new strategy object
pkstrat <- get.strategy(strategy.pk)


#initialise indicators, rules, signals
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
#fast 10 day simple  average
add.indicator(strategy.pk, name = "SMA", 
              arguments = list(x = quote(Cl(mktdata)), n = fma), 
              label = "fastMA")

#slow 200 day simple moving average
add.indicator(strategy.pk, name = "SMA", 
              arguments = list(x = quote(Cl(mktdata)), n = filma), 
              label = "filMA")

#RSI indicator
add.indicator(strategy.pk, name = "RSI", 
              arguments = list(price = quote(Cl(mktdata)), n = rsindex),
              label = "rsi")


#signal Fast MA
add.signal(strategy.pk, name = "sigComparison",
           arguments = list(columns = c("Close", "filMA"), relationship = "gt"),
           label = "upswing")
#signal RSI
add.signal(strategy.pk, name = "sigThreshold",
           arguments = list(column = "rsi", threshold = thre1, relationship = "lt", 
                            cross = FALSE),
           label = "rsiT1")
add.signal(strategy.pk, name = "sigThreshold",
           arguments = list(column = "rsi", threshold = thre2, relationship = "lt", 
                            cross = FALSE),
           label = "rsiT2")
add.signal(strategy.pk, name = "sigAND",
           arguments = list(columns = c("rsiT1", "upswing"), cross = TRUE),
           label = "entrylong1")
add.signal(strategy.pk, name = "sigAND",
           arguments = list(columns = c("rsiT2", "upswing"), cross = TRUE),
           label = "entrylong2")
add.signal(strategy.pk, name = "sigCrossover",
           arguments = list(columns = c("Close", "fastMA"), relationship = "gt"),
           label = "exitlongfast")
add.signal(strategy.pk, name = "sigCrossover",
           arguments = list(columns = c("Close", "filMA"), relationship = "lt"),
           label = "exitlongfil")
#Add signal for crossing above SMA
#Add signal for crossing below SMA
#add.signal(strategy.pk,name="sigCrossover",
 #          arguments = list(columns=c("Close","lowSMA"),relationship="gt"),
  #         label="Cl.gt.SMA")
#add.signal(strategy.pk,name="sigCrossover",
#           arguments = list(columns=c("Close","lowSMA"),relationship="lt"),
#           label="Cl.lt.SMA")
add.rule(strategy.pk, name='ruleSignal',
         arguments = list(sigcol="entrylong1", sigval = TRUE, ordertype='market', 
                          orderside='long', replace = FALSE, prefer = "Open"),
         type='enter', label = "longentry1")
add.rule(strategy.pk, name='ruleSignal',
         arguments = list(sigcol="entrylong2", sigval = TRUE, ordertype='market', 
                          orderside='long', replace = FALSE, prefer = "Open"),
         type='enter', label = "longentry2")
add.rule(strategy.pk, name='ruleSignal',
         arguments = list(sigcol="exitlongfast", sigval = TRUE, orderqty = "all",
                          ordertype='market', orderside='long', replace = FALSE, 
                          prefer = "Open"),
         type='exit', label = "exitlong1")
add.rule(strategy.pk, name='ruleSignal',
         arguments = list(sigcol="exitlongfil", sigval = TRUE, orderqty = "all",
                          ordertype='market', orderside='long', replace = FALSE, 
                          prefer = "Open"),
         type='exit', label = "exitlong2")

# go long when close > MA
#add.rule(strategy.pk, name='ruleSignal',
#         arguments = list(sigcol="Cl.gt.SMA", sigval = TRUE, orderqty=900,
#                          ordertype='market', orderside='long'),
#         type='enter')
## exit when close < MA
#add.rule(strategy.pk, name='ruleSignal',
#         arguments = list(sigcol="Cl.lt.SMA", sigval=TRUE, orderqty='all',
#                          ordertype='market', orderside='long'),
#         type='exit')



#Apply strategy to pf
applyStrategy(strategy = strategy.pk , portfolios = strategy.pk)
getTxns(Portfolio=strategy.pk, Symbol="SPY")


#update pf, account, equity
updatePortf(strategy.pk)
updateAcct(strategy.pk) 
updateEndEq(strategy.pk)

tstats <- t(tradeStats(strategy.pk))
perTradeStats(strategy.pk)
