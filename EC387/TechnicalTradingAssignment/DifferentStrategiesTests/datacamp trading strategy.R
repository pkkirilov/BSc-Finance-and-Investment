#As in the previous chapter, you have now carefully implemented a more advanced version of a for-loop that goes through all the rows of tradedata, decides on the buy signal that has to be generated and updates all output columns accordingly. 
# On top, the decision threshold that decides on the signal is changed adaptively using the bench variable.
# add your function definition here
run_adaptive_strategy <- function(tradedata, bsiz_box, brev_box) {
  ticksize <- 0.05
  bsiz <- bsiz_box * ticksize
  brev <- brev_box * bsiz
  bench <- tradedata$Price[1]
  tradedata$signal <- ""
  tradedata$tqty <- 0
  tradedata$tprice <- 0
  tradedata$tprofit <- 0
  tradedata$mtm <- 0
  tradedata$benchmark <- 0
  nrows <- nrow(tradedata)
  currpos <- 0
  for (i in 2:nrows) {
    tradedata$benchmark[i] <- bench
    
    if(tradedata$Price[i] - bench >= brev && currpos < 1) {
      tradedata$signal[i] <- "buy"
      tradedata$tprice[i] <- tradedata$Ask[i]
      tradedata$tqty[i] <- if (currpos==0) { 1 } else { 2 }
      currpos=currpos+tradedata$tqty[i]
    } else if(bench-tradedata$Price[i] >= brev && currpos > -1) {
      tradedata$signal[i] <- "sell"
      tradedata$tprice[i] <- tradedata$Bid[i]
      tradedata$tqty[i] <- if (currpos==0) { -1 } else { -2 }
      currpos=currpos+tradedata$tqty[i]
    } else {
      tradedata$signal[i] <- "no change"
      tradedata$tprice[i] <- 0
      tradedata$tqty[i] <- 0
    }   
    
    if(currpos > 0 && tradedata$Price[i] - bench >= bsiz) {
      bench <- bench + (round(100 * (tradedata$Price[i] - bench),0) %/% (100 * bsiz)) * bsiz
    } else if(currpos < 0 && bench - tradedata$Price[i] >= bsiz) {
      bench <- bench - (round(100 * (bench - tradedata$Price[i]),0) %/% (100 * bsiz)) * bsiz
    }
    
    tradedata$tprofit[i] <- - tradedata$tqty[i] * tradedata$tprice[i]
    tradedata$mtm[i] <- sum(tradedata$tprofit[1:i]) + currpos * tradedata$Price[i]
  }
  return(tradedata)
}

#load tradedata
tradedata_csv = read.csv(file = "http://assets.datacamp.com/course/quantinsti/data_ch3.csv", header = TRUE, stringsAsFactors =FALSE)

# Calculate 'tradedata_result' using your own function
tradedata_result <- run_adaptive_strategy(tradedata_csv, 4, 3)

# Print 'tradedata_result'
tradedata_result