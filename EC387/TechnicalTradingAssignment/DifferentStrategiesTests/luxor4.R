# After Jaekle & Tamasini: A new approach to system development and portfolio optimisation (ISBN 978-1-905641-79-6)
#
# Paragraph 3.4: luxor timespan paramset optimization

require(quantstrat)

source(paste0(path.package("quantstrat"),"/demo/luxor.include.R"))
source(paste0(path.package("quantstrat"),"/demo/luxor.getSymbols.R"))

### blotter

initPortf(portfolio.st, symbols='GBPUSD', initDate=initDate, currency='USD')
initAcct(account.st, portfolios=portfolio.st, initDate=initDate, currency='USD')

### quantstrat

initOrders(portfolio.st, initDate=initDate)

load.strategy(strategy.st)

### doMC
if (!"doMC" %in% installed.packages()[,1]) {
  install.packages("doMC")
}
require(doMC)
registerDoMC(cores=8)

#require(doParallel)
#registerDoParallel(cores=2)

#require(doRedis)
#registerDoRedis('jobs')

results <- apply.paramset(strategy.st, paramset.label='Timespan', portfolio.st=portfolio.st, account.st=account.st, nsamples=.nsamples, verbose=TRUE)

###

stats <- results$tradeStats

print(stats)

save(stats, file='luxor.4.paramset.timespan.RData')

