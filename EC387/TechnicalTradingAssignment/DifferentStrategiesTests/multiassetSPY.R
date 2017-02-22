#Multi asset portfolio - 9 asset SPDR portfolio dividing SP500 to 9 sectors
#phase 1
symbols = c("XLF", "XLP", "XLE", "XLY", "XLV", "XLI", "XLB", "XLK", "XLU")
getSymbols(symbols, src='yahoo', index.class=c("POSIXt","POSIXct"),
           from=startDate, to=endDate, adjust=T)
for(symbol in symbols)
{
  stock(symbol, currency="USD",multiplier=1)
  x<-get(symbol)
  x<-to.monthly(x,indexAt='endof',drop.time=FALSE)
  indexFormat(x)<-'%Y-%m-%d'
  colnames(x)<-gsub("x",symbol,colnames(x))
  assign(symbol,x)
}
#phase 2
multi.asset <- "multiAsset"
rm.strat(multi.asset) # remove strategy etc. if this is a re-run
initPortf(multi.asset,symbols=symbols, initDate=initDate)
initAcct(multi.asset,portfolios=multi.asset, initDate=initDate,
         initEq=initEq)
initOrders(portfolio=multi.asset,initDate=initDate)
#phase 3
applyStrategy(strategy=qs.strategy , portfolios=multi.asset)
updatePortf(multi.asset)
updateAcct(multi.asset)
updateEndEq(multi.asset)
checkBlotterUpdate(multi.asset,multi.asset)
## [1] TRUE
a <- getAccount(multi.asset)
p <- getPortfolio(multi.asset)
names(p$symbols)
par(mfrow=c(3,3))
for(symbol in symbols)
{
  chart.Posn(Portfolio=multi.asset,Symbol=symbol,theme=myTheme,
             TA="add_SMA(n=10,col='blue')")
}
par(mfrow=c(1,1))
#individual asset returns
rets.multi <- PortfReturns(multi.asset)
colnames(rets.multi) <- symbols
rets.multi <- na.omit(cbind(rets.multi,Return.calculate(a$summary$End.Eq)))
names(rets.multi)[length(names(rets.multi))] <- "TOTAL"
rets.multi <- rets.multi[,c("TOTAL",symbols)]
round(tail(rets.multi,5),6)
#cum returns by asset
chart.CumReturns(rets.multi, colorset= rich10equal, legend.loc = "topleft",
                 main="SPDR Cumulative Returns")
#return distribution analysis
chart.Boxplot(rets.multi, main = "SPDR Returns", colorset= rich10equal)
#annualised risk and return
(ar.tab <- table.AnnualizedReturns(rets.multi))
max.risk <- max(ar.tab["Annualized Std Dev",])
max.return <- max(ar.tab["Annualized Return",])
chart.RiskReturnScatter(rets.multi,
                        main = "SPDR Performance", colorset = rich10equal,
                        xlim=c(0,max.risk*1.1),ylim=c(0,max.return))
#consolidated equity curve
equity <- a$summary$End.Eq
plot(equity,main="Consolidated SPDR Equity Curve")



