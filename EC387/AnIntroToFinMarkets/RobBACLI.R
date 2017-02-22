require(zoo)
#use the zoo package so that I can use the rollmean function below
BAC <- read.csv("BAC.LI.csv")
# the part in quotation is whereever your data is stored
BAC$Date <- as.Date(BAC$Date, format = "%d/%m/%Y")
BAC <- BAC[rev(rownames(BAC)),]
BACR <- diff(BAC$Close) / BAC$Close[-length(BAC$Close)] 
BAC <- cbind(BAC[-1,], BACR)
#calculate the liquidity index
LI <- abs(BAC$BACR)/BAC$Volume*10000000
LIS <- rollmean(BACR, k = 10)
plot(BAC$Date[1:708], LIS, type ='l', main = "BAC Liquidity Index", xlab = "Date")
#just use the last 708 rows because the rolling mean does not cover whole set
BAC <- cbind(BAC[-c(1:9), ], LIS)
plot(BAC$Date[1:708], LIS, type ='l', main = "BAC Liquidity Index", xlab = "Date")
earnings.Dates <- c("14-10-2015", "15-07-2015", "15-04-2015", "15-01-2015", "15-10-2014", "16-07-2014", "16-04-2014", "15-01-2014",
                    "16-10-2013", "17-07-2013", "17-04-2013", "17-01-2013")
earnings.Dates <- as.Date(earnings.Dates, format = "%d-%m-%Y")
abline(v = earnings.Dates, col = "red")
# abline create a verticle (v) line at earnings dates
#create the dummy variables
BAC$D1 <- NA
BAC$D1 <- as.numeric(BAC$Date %in% earnings.Dates)
# One day before
BAC$D2 <- NA
BAC$D2 <- as.numeric((BAC$Date + 1) %in% earnings.Dates)
# One day after
BAC$D3 <- NA
BAC$D3 <- as.numeric((BAC$Date - 1) %in% earnings.Dates)
eq1 <- lm(BAC$LIS ~ BAC$D1 + BAC$D2 + BAC$D3)
# lm is the function for a linear model 
summary(eq1)