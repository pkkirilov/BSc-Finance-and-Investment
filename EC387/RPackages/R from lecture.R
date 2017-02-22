#Rpackages lecture 11/03/16

library(PerformanceAnalytics)
data(managers)
head(managers)
head(managers[,1])
charts.PerformanceSummary(managers[, c(1:6, 10)])
t(table.CalendarReturns(managers[, c(1:6, 10)]))
table.Stats(managers[, c(1:6, 10)])  
chart.Boxplot(managers[, c(1:6, 10)])

# Try to look at some other variables.  Compare one strategy vs risk-free or
# index. 

par(mfrow = c(2, 2)) #creating more than one chart on the plots. In the case we are creating a 2x2 matrix accommodating 4 charts alltogether
chart.Histogram(managers[,1,drop=F], main = "Plain", methods = NULL)
chart.Histogram(managers[,1,drop=F], main = "Density", breaks=40,
                methods = c("add.density", "add.normal"))
chart.Histogram(managers[,1,drop=F], main = "Skew and Kurt", methods = c
                ("add.centered", "add.rug"))
chart.Histogram(managers[,1,drop=F], main = "Risk Measures", methods = c
                ("add.risk"))

# Try some of the other methods.  Change the parameters of the layout. 

par(mfrow = c(1,1))
chart.RiskReturnScatter(managers[, c(1:6, 10)], Rf=.03/12, main =
                          "Trailing 36-Month Performance") #Hedge fund 5 has a lot of risk, but the same return as the Treasury

# add boxplots. 

chart.RollingPerformance(managers[, c(1:2, 10)]) 
chart.RelativePerformance(managers[, c(1:2)], Rb = managers[,8])

table.DownsideRisk(managers[, c(1, 2)])
#-----------------------------------------------
library(urca)
summary(ur.df(managers[,1]))
summary(ur.pp(managers[,1]))
#------------------------------------------------
library(vars) #vectorautoregressions package, very useful
data(Canada)
plot(Canada, ac = 2, xlab = "")
adf1 <- summary(ur.df(Canada[, "prod"], type = "trend", lags = 2))
adf1
model1 <- VAR(Canada[, c("prod", "e", "U", "rw")])
summary(model1)
plot(model1)
#----------------------------------------------------
require(quantmod)
require(TTR)
getSymbols("SPY", from = "2000-01-01", to = "2015-12-31")
head(SPY)
chart_Series(SPY)
zoom_Chart("2008")
zoom_Chart("2008-01-01::2008-01-31")
sma <- SMA(x = Cl(SPY), n = 200)
tail(sma)
chart_Series(SPY)
add_TA(sma, on=1, lwd = 1.5, col = 'blue')
chart_Series(SPY, subset = "2008-01-01::2009-12-31")
add_TA(sma, on = 1, col = "blue") #adds a technical indicator
testRSI <- RSI(Cl(SPY), n = 2)
chart_Series(SPY, subset = "2008-01-01::2009-12-31")
add_TA(testRSI, col = 'green')
# Create a series with bolinger bands, 
bb <- BBands(HLC = SPY[, c(2, 3, 4)], n = 20, sd = 2) #HLC - high low close; 20 moving average
tail(bb)
chart_Series(SPY, subset = "2008-01-01::2009-12-31")
add_TA(bb[,c(1:3)], on = 1, col = "blue")
zoom_Chart("2015")
#-----------------------------------------------------
#work in progresss
head(SPY)
da <- cbind(SPY[,4], bb[,c(1:3)])  
head(da, n = 100)
tail(da, n = 100)
da$action <- rep(NA, dim(da)[1])
i <- 21
for (i in nrow(da)) {
if (da[i,1] <= da[i,2]) {
  da[i,5] <- 1
} else if (da[i,1] >= da[i,4]) {
  da[i,5] <- -1
} else {
  da[i,5] <- 0
} }
sum(da[,4] == 0, na.rm = TRUE)
