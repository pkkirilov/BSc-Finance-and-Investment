#Basic Trading Strategy from DataCamp
tradedata <- read.csv(file = "http://assets.datacamp.com/course/quantinsti/data_ch2.csv", header = TRUE, stringsAsFactors = FALSE)
parameter <- 2
tradedata$signal <- ""
tradedata$tqty <- 0
tradedata$tprice <- 0
tradedata$tprofit <- 0
tradedata$mtm <- 0
nrows = nrow(tradedata)
currpos = 0

for (i in 2:nrows) {
  if(abs(tradedata$Price[i]-tradedata$Price[i-1]) >= parameter)  {
    if(currpos < 1) {
      tradedata$signal[i] <- "buy"
      tradedata$tprice[i] <- tradedata$Ask[i]
      tradedata$tqty[i] <- if (currpos==0) { 1 } else { 2 }
      currpos <- currpos+tradedata$tqty[i]
    } else {
      tradedata$signal[i] <- "sell"
      tradedata$tprice[i] <- tradedata$Bid[i]
      tradedata$tqty[i] <- if (currpos==0) { -1 } else { -2 }
      currpos <- -1
    } 
  } else {
    tradedata$signal[i] <- "no change"
    tradedata$tprice[i] <- 0
    tradedata$tqty[i] <- 0
  }
  # tprofit
  tradedata$tprofit[i] <- - tradedata$tqty[i] * tradedata$tprice[i]
  # mtm
  tradedata$mtm[i] <- sum(tradedata$tprofit[1:i]) + currpos * tradedata$Price[i]  
} 

# Calculate 'num_traded_lots'
num_traded_lots <- sum(abs(tradedata$tqty)) + 1

# Print 'num_traded_lots'
num_traded_lots

# Calculate 'mtm_final'
mtm_final <- tradedata$mtm[nrows]

# Print 'mtm_final'
mtm_final
