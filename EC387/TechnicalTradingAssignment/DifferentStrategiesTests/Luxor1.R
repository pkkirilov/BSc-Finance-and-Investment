#!/usr/bin/Rscript --vanilla
#
# Jan Humme (@opentrades) - August 2012, revised April 2013
#
# Tested and found to work correctly using blotter r1457
#
# After Jaekle & Tamasini: A new approach to system development and portfolio optimisation (ISBN 978-1-905641-79-6)
#
# Paragraph 3.2: luxor with slippage and transaction costs

require(quantstrat)

##### PLACE DEMO AND TEST DATES HERE #################
#
#if(isTRUE(options('in_test')$in_test))
#  # use test dates
#  {initDate="2011-01-01" 
#  endDate="2012-12-31"   
#  } else
#  # use demo defaults
#  {initDate="1999-12-31"
#  endDate=Sys.Date()}

Sys.setenv(TZ="UTC")

###

initDate = '2002-10-21'

.from=initDate

#.to='2008-07-04'
.to='2002-10-31'

###

strategy.st = 'luxor'
portfolio.st = 'forex'
account.st = 'IB'

###

.orderqty = 100000
.threshold = 0.0005
.txnfees = -6		# round-trip fee

### Distributions for paramset analysis

.nsamples=80

.FastSMA = (1:20)
.SlowSMA = (30:80)

.StopLoss = seq(0.05, 2.4, length.out=48)/100
.StopTrailing = seq(0.05, 2.4, length.out=48)/100
.TakeProfit = seq(0.1, 4.8, length.out=48)/100

.FastWFA = c(1, 3, 5, 7, 9)
.SlowWFA = c(42, 44, 46)

# generate 24x24h ISO8601 timespan vector

.timespans.start<-paste(sprintf("T%02d",0:23),':00',sep='')
.timespans.stop<-paste(sprintf("T%02d",0:23),':59',sep='')

.timespans<-outer(.timespans.start, .timespans.stop, FUN=paste, sep='/')

# in order to run the full 24x24 hour scan above, comment out the following line:
.timespans<-c('T06:00/T10:00', 'T07:00/T11:00', 'T08:00/T12:00', 'T09:00/T13:00', 'T10:00/T14:00', 'T11:00/T15:00', 'T12:00/T16:00')

### Actual arameters

.fast = 6
.slow = 44

#.timespan = 'T09:00/T13:00'
#.timespan = 'T00:00/T23:59'
.timespan = NULL

.stoploss <- 0.40/100
.stoptrailing <- 0.8/100
.takeprofit <- 2.0/100

suppressWarnings(rm(list = c(paste("account", account.st, sep='.'), paste("portfolio", portfolio.st, sep='.')), pos=.blotter))
suppressWarnings(rm(list = c(strategy.st, paste("order_book", portfolio.st, sep='.')), pos=.strategy))
.fast = 10
.slow = 30

currency(c('GBP', 'USD'))

exchange_rate('GBPUSD', tick_size=0.0001)

### quantmod

getSymbols.FI(Symbols='GBPUSD',
              #	      dir=system.file('extdata',package='quantstrat'),
              dir='~/R/OHLC',
              from=.from, to=.to
)

# ALTERNATIVE WAY TO FETCH SYMBOL DATA
#setSymbolLookup.FI(system.file('extdata',package='quantstrat'), 'GBPUSD')
#getSymbols('GBPUSD', from=.from, to=.to, verbose=FALSE)

### xts

GBPUSD = to.minutes30(GBPUSD)
GBPUSD = align.time(to.minutes30(GBPUSD), 1800)

### blotter

initPortf(portfolio.st, symbols='GBPUSD', initDate=initDate, currency='USD')
initAcct(account.st, portfolios=portfolio.st, initDate=initDate, currency='USD')

### quantstrat

initOrders(portfolio.st, initDate=initDate)

### define strategy

strategy(strategy.st, store=TRUE)

### indicators

add.indicator(strategy.st, name = "SMA",
              arguments = list(
                x = quote(Cl(mktdata)[,1]),
                n = .fast
              ),
              label="nFast"
)

add.indicator(strategy.st, name="SMA",
              arguments = list(
                x = quote(Cl(mktdata)[,1]),
                n = .slow
              ),
              label="nSlow"
)

### signals

add.signal(strategy.st, name='sigCrossover',
           arguments = list(
             columns=c("nFast","nSlow"),
             relationship="gte"
           ),
           label='long'
)

add.signal(strategy.st, name='sigCrossover',
           arguments = list(
             columns=c("nFast","nSlow"),
             relationship="lt"
           ),
           label='short'
)

### rules

add.rule(strategy.st, name='ruleSignal',
         arguments=list(sigcol='long' , sigval=TRUE,
                        orderside='short',
                        ordertype='market',
                        orderqty='all',
                        TxnFees=.txnfees,
                        replace=TRUE
         ),
         type='exit',
         label='Exit2LONG'
)

add.rule(strategy.st, name='ruleSignal',
         arguments=list(sigcol='short', sigval=TRUE,
                        orderside='long' ,
                        ordertype='market',
                        orderqty='all',
                        TxnFees=.txnfees,
                        replace=TRUE
         ),
         type='exit',
         label='Exit2SHORT'
)

add.rule(strategy.st, name='ruleSignal',
         arguments=list(sigcol='long' , sigval=TRUE,
                        orderside='long' ,
                        ordertype='stoplimit', prefer='High', threshold=.threshold,
                        orderqty=+.orderqty,
                        replace=FALSE
         ),
         type='enter',
         label='EnterLONG'
)

add.rule(strategy.st, name='ruleSignal',
         arguments=list(sigcol='short', sigval=TRUE,
                        orderside='short',
                        ordertype='stoplimit', prefer='Low', threshold=-.threshold,
                        orderqty=-.orderqty,
                        replace=FALSE
         ),
         type='enter',
         label='EnterSHORT'
)

###############################################################################

applyStrategy(strategy.st, portfolio.st)

View(getOrderBook(portfolio.st)[[portfolio.st]]$GBPUSD)

###############################################################################

updatePortf(portfolio.st, Symbols='GBPUSD', Dates=paste('::',as.Date(Sys.time()),sep=''))

chart.Posn(portfolio.st, "GBPUSD")

###############################################################################

View(t(tradeStats(portfolio.st, 'GBPUSD')))

###############################################################################

# save the strategy in an .RData object for later retrieval

save.strategy(strategy.st)

##### PLACE THIS BLOCK AT END OF DEMO SCRIPT ################### 
# book  = getOrderBook(port)
# stats = tradeStats(port)
# rets  = PortfReturns(acct)
################################################################