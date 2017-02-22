require("eventstudies")
set.seed(5)
# create 1000 random returns
rets <- rnorm(1000, 0.01, 0.05)
# create a sequence of dates
date <- seq(from = as.Date("2000-01-01"), by = 1, length.out = 1000)
retsx <- as.xts(rets, order.by = date)
#create an xts object
colnames(retsx) <- "BAC"
eventDates <- sample(date, 10)
BAC <- rep("BAC", 10)
#create the required dataframe
eventdf <- data.frame(unit = BAC, when = eventDates)
es <- phys2eventtime(retsx, events = eventdf, width = 5)
es.w <- window(es$z, start = -5, end = 5)
es.cs <-remap.cumsum(es.w, is.pc = FALSE, base = 0)
result <- inference.Ecar(es.cs)
#result <- inference.Ecar(es.cs, to.plot = TRUE)
rownames(result) <- seq(-5, 5, 1)
matplot(rownames(result), result, type = 'l', lty = c(2, 1, 2), col = 'darkgreen', main = "Event study with CI",
        xlab = 'Event times', ylab = 'Return')
