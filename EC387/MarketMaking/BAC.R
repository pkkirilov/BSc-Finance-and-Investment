BAC <- read.csv("BAC.csv")
head(BAC)
BAC$Date <- as.Date(BAC$Date, format = "%d/%m/%Y")
BAC <- BAC[,c(1,7)]
colnames(BAC) <- c("Date", "Close")
BAC <- BAC[rev(rownames(BAC)),]
head(BAC)
BACR <- diff(BAC$Close) / BAC$Close[-length(BAC$Close)] 
BACr <- diff(log(BAC$Close))
BAC <- cbind(BAC[-1,], BACR, BACr)
tail(BAC)
library(quantmod)
getSymbols("BAC", from = "2000-01-01", to = "2013-12-31")
BACR <- monthlyReturn(BAC, type = "arithmetic")
BACr <- monthlyReturn(BAC, type = "log")
plot(BACR, BAC$Date, type = 'l')
require(moments)
statNames <- c("mean", "std dev", "skewness", "kurtosis", "max", "min")
BAC.stats <- c(mean(BACr), sd(BACr), skewness(BACr), kurtosis(BACr), 
               max(BACr), min(BACr))
names(BAC.stats) <- statNames
round(BAC.stats, 4)
SEmeanBAC <- BAC.stats[1]/length(BACr)^0.5
lowerconfBAC <- BAC.stats[1] - 2 * SEmeanBAC
upperConfBAC <- BAC.stats[1] + 2 * SEmeanBAC
acf(BACr)
names(acf(BACr, plot = FALSE))
acf(BACr, plot = FALSE)$acf
n <- 1000 # sample size
MeanSample <- numeric(n)
for(i in 1:n){
  temp <- BACr[sample(length(BACr), length(BACr), replace = TRUE)]
  MeanSample[i] <- mean(temp, na.rm = TRUE)
}
hist(MeanSample)
quantile(MeanSample, 0.025)
quantile(MeanSample, 0.975)
