
> # This is a very simple trend following strategy for testing the results of:
  > # Faber, Mebane T., "A Quantitative Approach to Tactical Asset Allocation." 
  > # Journal of Risk Management (Spring 2007).
  > # The article proposes a very simple quantitative market-timing model.  They 
  > # test the model in sample on the US stock market since 1900 before testing
  > # it out-of-sample in twenty other markets.
  > 
  > # The article discusses a 200-day simple moving average, which is proposed
  > # in Jeremy Seigel's book "Stocks for the Long Run" for timing the DJIA.  He 
  > # concludes that a simple market timing strategy improves the absolute and
  > # risk adjusted returns over a buy-and-hold strategy.  After all transaction
  > # costs are included, the timing strategy falls short on the absolute return,
  > # but still provides a better risk-adjusted return.  Siegel also tests timing on  
  > # the Nasdaq composite since 1972 and finds better absolute and risk adjusted
  > # returns.
  > 
  > # The article implements a simpler version of the 200-day SMA, opting for a
  > # 10-month SMA.  Monthly data is more easily available for long periods of time,
  > # and the lower granularity should translate to lower transaction costs.  
  > 
  > # The rules of the system are relatively simple:
  > # - Buy when monthly price > 10-month SMA
  > # - Sell and move to cash when monthly price < 10-month SMA
  > 
  > # 1. All entry and exit prices are on the day of the signal at the close.
  > # 2. All data series are total return series including dividends, updated monthly. 
  > #    For the purposes of this demo, we only use price returns.
  > # 3. Cash returns are estimated with 90-day commercial paper.  Margin rates for
  > #    leveraged models are estimated with the broker call rate.  Again, for the
  > #    purposes of this demo, we ignore interest and leverage.
  > # 4. Taxes, commissions, and slippage are excluded.
  > 
  > # This simple strategy is different from well-known trend-following systems in
  > # three respects.  First, there's no shorting.  Positions are converted to cash on
  > # a 'sell' signal, rather than taking a short position. Second, the entire position
  > # is put on at trade inception.  No assumptions are made about increasing position
  > # size as the trend progresses.  Third, there are no stops.  If the trend reverts
  > # quickly, this system will wait for a sell signal before selling the position.
  > 
  > # Data
  > # Instead of using total returns data, this demo uses monthly data for the SP500
  > # downloaded from Yahoo Finance.  We'll use about 10 years of data, starting at 
  > # the beginning of 1998.
  > 
  > # Load required libraries
  > require(quantstrat)

> #correct for TZ issues if they crop up
  > oldtz <- Sys.getenv('TZ')
  
  > if(oldtz=='') {
    +   Sys.setenv(TZ="UTC")
    + }
  
  > # Try to clean up in case the demo was run previously
    > suppressWarnings(rm("account.faber","portfolio.faber",pos=.blotter))
  
  > suppressWarnings(rm("ltaccount", "ltportfolio", "ClosePrice", "CurrentDate", "equity", 
                        +             "GSPC", "stratFaber", "startDate", "initEq", "Posn", "UnitSize", "verbose"))
  
  > suppressWarnings(rm("order_book.faber",pos=.strategy))
  
  > # Set initial values
    > startDate='1997-12-31'
    
    > initEq=100000
    
    > # Set up instruments with FinancialInstruments package
      > currency("USD")
    [1] "USD"
    
    > symbols = c("XLF", "XLP", "XLE", "XLY", "XLV", "XLI", "XLB", "XLK", "XLU")
    
    > for(symbol in symbols){ # establish tradable instruments
      +     stock(symbol, currency="USD",multiplier=1)
      + }
    
    > # Load data with quantmod
      > #getSymbols(symbols, src='yahoo', index.class=c("POSIXt","POSIXct"), from='1998-01-01')
      > ### Download monthly data instead?
      > ### GSPC=to.monthly(GSPC, indexAt='endof')
      > getSymbols(symbols, src='yahoo', index.class=c("POSIXt","POSIXct"), from='1999-01-01')
    As of 0.4-0, 'getSymbols' uses env=parent.frame() and
    auto.assign=TRUE by default.
    
    This  behavior  will be  phased out in 0.5-0  when the call  will
    default to use auto.assign=FALSE. getOption("getSymbols.env") and 
    getOptions("getSymbols.auto.assign") are now checked for alternate defaults
    
    This message is shown once per session and may be disabled by setting 
    options("getSymbols.warning4.0"=FALSE). See ?getSymbols for more details.
    pausing 1 second between requests for more than 5 symbols
    pausing 1 second between requests for more than 5 symbols
    pausing 1 second between requests for more than 5 symbols
    pausing 1 second between requests for more than 5 symbols
    pausing 1 second between requests for more than 5 symbols
    [1] "XLF" "XLP" "XLE" "XLY" "XLV" "XLI" "XLB" "XLK" "XLU"
    
    > for(symbol in symbols) {
      +     x<-get(symbol)
      +     x<-to.monthly(x,indexAt='lastof',drop.time=TRUE)
      +     indexFormat(x)<-'%Y-%m-%d'
      +     colnames(x)<-gsub("x",symbol,colnames(x))
      +     assign(symbol,x)
      + }
    
    > # Initialize portfolio and account
      > initPortf('faber', symbols=symbols)
    [1] "faber"
    
    > initAcct('faber', portfolios='faber', initEq=100000)
    [1] "faber"
    
    > initOrders(portfolio='faber')
    
    > # set intial position limits
      > posval<-initEq/length(symbols)
    
    > for(symbol in symbols){
      +     pos<-round((posval/first(getPrice(get(symbol)))[,1]),-2)
      +     addPosLimit('faber', symbol, startDate, maxpos=pos, minpos=-pos)
      + }
    
    > print("setup completed")
    [1] "setup completed"
    
    > # Initialize a strategy object
      > strategy("faber", store=TRUE)
    
    > # Add an indicator
      > add.indicator('faber', name = "SMA", arguments = list(x = quote(Cl(mktdata)), n=10), label="SMA10")
    [1] "faber"
    
    > # There are two signals:
      > # The first is when monthly price crosses over the 10-month SMA
      > add.signal('faber',name="sigCrossover",arguments = list(columns=c("Close","SMA10"),relationship="gte"),label="Cl.gt.SMA")
    [1] "faber"
    
    > # The second is when the monthly price crosses under the 10-month SMA
      > add.signal('faber',name="sigCrossover",arguments = list(columns=c("Close","SMA10"),relationship="lt"),label="Cl.lt.SMA")
    [1] "faber"
    
    > # There are two rules:
      > # The first is to buy when the price crosses above the SMA
      > add.rule('faber', name='ruleSignal', arguments = list(sigcol="Cl.gt.SMA", sigval=TRUE, orderqty=100000, osFUN='osMaxPos', ordertype='market', orderside='long', pricemethod='market',TxnFees=-5), type='enter', path.dep=TRUE)
    [1] "faber"
    
    > # The second is to sell when the price crosses below the SMA
      > add.rule('faber', name='ruleSignal', arguments = list(sigcol="Cl.lt.SMA", sigval=TRUE, orderqty='all', ordertype='market', orderside='long', pricemethod='market',TxnFees=-5), type='exit', path.dep=TRUE)
    [1] "faber"
    
    > # add quaterly rebalancing
      > add.rule('faber', 'rulePctEquity',
                 +         arguments=list(rebalance_on='quarters',
                                          +                 trade.percent=1/length(symbols),
                                          +                 refprice=quote(last(getPrice(mktdata)[paste('::',as.character(curIndex),sep='')][,1])),
                                          +                 digits=0
                                          +         ),
                 +         type='rebalance',
                 +         label='rebalance'
                 + )
    [1] "faber"
    
    > # Process the strategy and generate trades
      > start_t<-Sys.time()
    
    > out<-applyStrategy.rebalancing(strategy='faber' , portfolios='faber')
    [1] "1999-12-31 00:00:00 XLB 418 @ 26.578119"
    [1] "2000-01-31 00:00:00 XLB -418 @ 23.328119"
    [1] "1999-12-31 00:00:00 XLI 375 @ 29.609381"
    [1] "2000-01-31 00:00:00 XLI -375 @ 27.1875"
    [1] "2000-03-31 00:00:00 XLE 374 @ 29.3125"
    [1] "2000-03-31 00:00:00 XLF 452 @ 24.265619"
    [1] "2000-03-31 00:00:00 XLI 372 @ 29.203119"
    [1] "2000-05-31 00:00:00 XLP 512 @ 23.9375"
    [1] "2000-04-30 00:00:00 XLU 406 @ 28.484381"
    [1] "2000-03-31 00:00:00 XLV 354 @ 30.671881"
    [1] "2000-03-31 00:00:00 XLY 371 @ 29.265619"
    [1] "2000-05-31 00:00:00 XLY -371 @ 27.109381"
    [1] "2000-06-30 00:00:00 XLI -372 @ 28.28125"
    [1] "2000-07-31 00:00:00 XLI 383 @ 28.71875"
    [1] "2000-06-30 00:00:00 XLK 200 @ 54.1875"
    [1] "2000-07-31 00:00:00 XLK -200 @ 51.3125"
    [1] "2000-08-31 00:00:00 XLK 200 @ 56.53125"
    [1] "2000-06-30 00:00:00 XLU -406 @ 27.25"
    [1] "2000-08-31 00:00:00 XLU 398 @ 28.75"
    [1] "2000-07-31 00:00:00 XLV -354 @ 29.125"
    [1] "2000-08-31 00:00:00 XLV 366 @ 30.15625"
    [1] "2000-11-30 00:00:00 XLE -374 @ 30.203119"
    [1] "2000-09-30 00:00:00 XLK -200 @ 46.375"
    [1] "2000-09-30 00:00:00 XLV -366 @ 29.0625"
    [1] "2000-12-31 00:00:00 XLB 507 @ 21.421881"
    [1] "2000-12-31 00:00:00 XLE 347 @ 33.1875"
    [1] "2000-12-31 00:00:00 XLE -24 @ 33.1875"
    [1] "2001-02-28 00:00:00 XLF -452 @ 27.450001"
    [1] "2001-02-28 00:00:00 XLI -383 @ 29.49"
    [1] "2001-01-31 00:00:00 XLV 409 @ 29.219999"
    [1] "2001-02-28 00:00:00 XLV -409 @ 28.66"
    [1] "2001-01-31 00:00:00 XLY 435 @ 28.57"
    [1] "2001-03-31 00:00:00 XLB -507 @ 19.969999"
    [1] "2001-04-30 00:00:00 XLB 521 @ 22.1"
    [1] "2001-03-31 00:00:00 XLE -323 @ 30.700001"
    [1] "2001-04-30 00:00:00 XLE 336 @ 33.869999"
    [1] "2001-05-31 00:00:00 XLF 390 @ 28.709999"
    [1] "2001-05-31 00:00:00 XLI 394 @ 30.870001"
    [1] "2001-03-31 00:00:00 XLP -512 @ 24.719999"
    [1] "2001-04-30 00:00:00 XLV 390 @ 28.799999"
    [1] "2001-06-30 00:00:00 XLE -336 @ 30.35"
    [1] "2001-07-31 00:00:00 XLF -390 @ 28.129999"
    [1] "2001-06-30 00:00:00 XLI -394 @ 28.9"
    [1] "2001-06-30 00:00:00 XLU -398 @ 31.15"
    [1] "2001-08-31 00:00:00 XLV -390 @ 27.49"
    [1] "2001-08-31 00:00:00 XLY -435 @ 26.370001"
    [1] "2001-09-30 00:00:00 XLB -521 @ 19.200001"
    [1] "2001-11-30 00:00:00 XLB 464 @ 22.219999"
    [1] "2001-11-30 00:00:00 XLP 377 @ 26"
    [1] "2001-11-30 00:00:00 XLY 430 @ 27.4"
    [1] "2001-12-31 00:00:00 XLI 355 @ 27.700001"
    [1] "2002-01-31 00:00:00 XLI -355 @ 25.950001"
    [1] "2002-02-28 00:00:00 XLV 364 @ 27.98"
    [1] "2002-03-31 00:00:00 XLE 349 @ 28.889999"
    [1] "2002-03-31 00:00:00 XLF 371 @ 27.15"
    [1] "2002-03-31 00:00:00 XLI 367 @ 27.24"
    [1] "2002-04-30 00:00:00 XLI -367 @ 25.360001"
    [1] "2002-04-30 00:00:00 XLP -377 @ 25.129999"
    [1] "2002-07-31 00:00:00 XLB -464 @ 20.559999"
    [1] "2002-06-30 00:00:00 XLE -349 @ 26.200001"
    [1] "2002-06-30 00:00:00 XLF -371 @ 25.139999"
    [1] "2002-07-31 00:00:00 XLV -364 @ 26.879999"
    [1] "2002-06-30 00:00:00 XLY -430 @ 27.469999"
    [1] "2002-11-30 00:00:00 XLK 767 @ 17.1"
    [1] "2002-12-31 00:00:00 XLK -767 @ 14.8"
    [1] "2003-04-30 00:00:00 XLB 496 @ 20.040001"
    [1] "2003-05-31 00:00:00 XLE 409 @ 24.48"
    [1] "2003-04-30 00:00:00 XLF 439 @ 23.370001"
    [1] "2003-04-30 00:00:00 XLI 472 @ 21.4"
    [1] "2003-04-30 00:00:00 XLK 641 @ 15.61"
    [1] "2003-05-31 00:00:00 XLP 493 @ 20.059999"
    [1] "2003-04-30 00:00:00 XLU 498 @ 19.99"
    [1] "2003-04-30 00:00:00 XLV 343 @ 27.6"
    [1] "2003-04-30 00:00:00 XLY 403 @ 25.389999"
    [1] "2004-04-30 00:00:00 XLK -641 @ 19.389999"
    [1] "2004-05-31 00:00:00 XLK 564 @ 20.15"
    [1] "2004-07-31 00:00:00 XLF -439 @ 27.969999"
    [1] "2004-08-31 00:00:00 XLF 406 @ 28.879999"
    [1] "2004-07-31 00:00:00 XLK -564 @ 19.440001"
    [1] "2004-07-31 00:00:00 XLP -493 @ 21.950001"
    [1] "2004-07-31 00:00:00 XLV -343 @ 28.92"
    [1] "2004-07-31 00:00:00 XLY -403 @ 30.43"
    [1] "2004-09-30 00:00:00 XLF -406 @ 28.459999"
    [1] "2004-11-30 00:00:00 XLF 404 @ 29.5"
    [1] "2004-10-31 00:00:00 XLK 602 @ 19.959999"
    [1] "2004-10-31 00:00:00 XLY 369 @ 32.459999"
    [1] "2005-01-31 00:00:00 XLK -602 @ 19.92"
    [1] "2004-12-31 00:00:00 XLP 524 @ 23.08"
    [1] "2004-12-31 00:00:00 XLV 401 @ 30.190001"
    [1] "2005-01-31 00:00:00 XLV -401 @ 29.25"
    [1] "2005-02-28 00:00:00 XLV 401 @ 30.129999"
    [1] "2005-04-30 00:00:00 XLB -496 @ 28.01"
    [1] "2005-03-31 00:00:00 XLF -404 @ 28.389999"
    [1] "2005-05-31 00:00:00 XLF 433 @ 29.280001"
    [1] "2005-04-30 00:00:00 XLI -472 @ 29.32"
    [1] "2005-05-31 00:00:00 XLI 404 @ 30.25"
    [1] "2005-05-31 00:00:00 XLK 629 @ 20.18"
    [1] "2005-04-30 00:00:00 XLY -369 @ 31.049999"
    [1] "2005-05-31 00:00:00 XLY 369 @ 32.970001"
    [1] "2005-08-31 00:00:00 XLF -433 @ 29.440001"
    [1] "2005-06-30 00:00:00 XLI -404 @ 29.379999"
    [1] "2005-07-31 00:00:00 XLI 418 @ 30.5"
    [1] "2005-08-31 00:00:00 XLI -418 @ 29.780001"
    [1] "2005-06-30 00:00:00 XLK -629 @ 19.92"
    [1] "2005-07-31 00:00:00 XLK 615 @ 21.049999"
    [1] "2005-06-30 00:00:00 XLY -369 @ 32.779999"
    [1] "2005-07-31 00:00:00 XLY 371 @ 34.639999"
    [1] "2005-11-30 00:00:00 XLB 467 @ 29.67"
    [1] "2005-09-30 00:00:00 XLF 418 @ 29.52"
    [1] "2005-09-30 00:00:00 XLF 17 @ 29.52"
    [1] "2005-11-30 00:00:00 XLI 423 @ 31.459999"
    [1] "2005-10-31 00:00:00 XLV -401 @ 30.389999"
    [1] "2005-09-30 00:00:00 XLY -371 @ 32.490002"
    [1] "2005-11-30 00:00:00 XLY 388 @ 33.099998"
    [1] "2005-12-31 00:00:00 XLV 392 @ 31.719999"
    [1] "2005-12-31 00:00:00 XLY -388 @ 32.650002"
    [1] "2006-01-31 00:00:00 XLY 380 @ 33.23"
    [1] "2006-05-31 00:00:00 XLK -615 @ 20.43"
    [1] "2006-03-31 00:00:00 XLU -498 @ 30.83"
    [1] "2006-04-30 00:00:00 XLV -392 @ 30.99"
    [1] "2006-07-31 00:00:00 XLB -467 @ 30.9"
    [1] "2006-08-31 00:00:00 XLB 403 @ 31.83"
    [1] "2006-07-31 00:00:00 XLI -423 @ 31.889999"
    [1] "2006-08-31 00:00:00 XLK 637 @ 21.190001"
    [1] "2006-06-30 00:00:00 XLU 398 @ 32.290001"
    [1] "2006-07-31 00:00:00 XLV 426 @ 31.889999"
    [1] "2006-07-31 00:00:00 XLY -380 @ 32.279999"
    [1] "2006-09-30 00:00:00 XLB -403 @ 31.639999"
    [1] "2006-10-31 00:00:00 XLB 408 @ 33.540001"
    [1] "2006-09-30 00:00:00 XLE -409 @ 53.450001"
    [1] "2006-11-30 00:00:00 XLE 241 @ 60.509998"
    [1] "2006-09-30 00:00:00 XLI 385 @ 33.34"
    [1] "2006-09-30 00:00:00 XLY 369 @ 34.950001"
    [1] "2007-02-28 00:00:00 XLE -241 @ 56.950001"
    [1] "2007-03-31 00:00:00 XLE 227 @ 60.279999"
    [1] "2007-03-31 00:00:00 XLE 1 @ 60.279999"
    [1] "2007-06-30 00:00:00 XLF -435 @ 36.18"
    [1] "2007-07-31 00:00:00 XLP -524 @ 26.280001"
    [1] "2007-08-31 00:00:00 XLP 538 @ 26.9"
    [1] "2007-07-31 00:00:00 XLU -398 @ 38"
    [1] "2007-07-31 00:00:00 XLV -426 @ 33.630001"
    [1] "2007-07-31 00:00:00 XLY -369 @ 37"
    [1] "2007-09-30 00:00:00 XLU 372 @ 39.799999"
    [1] "2007-09-30 00:00:00 XLV 416 @ 35.349998"
    [1] "2008-01-31 00:00:00 XLB -408 @ 39.950001"
    [1] "2008-01-31 00:00:00 XLE -228 @ 69.599998"
    [1] "2008-02-29 00:00:00 XLE 186 @ 76.050003"
    [1] "2008-01-31 00:00:00 XLI -385 @ 37"
    [1] "2008-01-31 00:00:00 XLK -637 @ 23.25"
    [1] "2008-01-31 00:00:00 XLP -538 @ 27.200001"
    [1] "2008-01-31 00:00:00 XLU -372 @ 39.220001"
    [1] "2008-01-31 00:00:00 XLV -416 @ 33.700001"
    [1] "2008-04-30 00:00:00 XLB 345 @ 42.310001"
    [1] "2008-05-31 00:00:00 XLI 365 @ 38.84"
    [1] "2008-05-31 00:00:00 XLK 613 @ 25.35"
    [1] "2008-03-31 00:00:00 XLP 491 @ 27.860001"
    [1] "2008-04-30 00:00:00 XLP -491 @ 27.65"
    [1] "2008-05-31 00:00:00 XLP 491 @ 28.49"
    [1] "2008-04-30 00:00:00 XLU 362 @ 39.880001"
    [1] "2008-06-30 00:00:00 XLB -345 @ 41.73"
    [1] "2008-07-31 00:00:00 XLE -186 @ 74.400002"
    [1] "2008-06-30 00:00:00 XLI -365 @ 34.009998"
    [1] "2008-06-30 00:00:00 XLK -613 @ 22.91"
    [1] "2008-06-30 00:00:00 XLP -491 @ 26.75"
    [1] "2008-08-31 00:00:00 XLP 511 @ 28.139999"
    [1] "2008-07-31 00:00:00 XLU -362 @ 38.080002"
    [1] "2008-09-30 00:00:00 XLP -511 @ 27.6"
    [1] "2009-05-31 00:00:00 XLB 597 @ 27.17"
    [1] "2009-05-31 00:00:00 XLE 312 @ 51.68"
    [1] "2009-05-31 00:00:00 XLK 849 @ 17.65"
    [1] "2009-04-30 00:00:00 XLY 675 @ 23.290001"
    [1] "2009-06-30 00:00:00 XLE -312 @ 48.049999"
    [1] "2009-07-31 00:00:00 XLE 271 @ 50.619999"
    [1] "2009-07-31 00:00:00 XLF 1091 @ 13.01"
    [1] "2009-07-31 00:00:00 XLI 594 @ 23.879999"
    [1] "2009-07-31 00:00:00 XLP 570 @ 24.52"
    [1] "2009-07-31 00:00:00 XLU 470 @ 28.940001"
    [1] "2009-06-30 00:00:00 XLV 498 @ 26.309999"
    [1] "2010-05-31 00:00:00 XLB -597 @ 30.75"
    [1] "2010-05-31 00:00:00 XLE -271 @ 53.049999"
    [1] "2010-05-31 00:00:00 XLF -1091 @ 14.68"
    [1] "2010-05-31 00:00:00 XLK -849 @ 21.639999"
    [1] "2010-05-31 00:00:00 XLP -570 @ 26.35"
    [1] "2010-05-31 00:00:00 XLU -470 @ 28.76"
    [1] "2010-05-31 00:00:00 XLV -498 @ 28.870001"
    [1] "2010-07-31 00:00:00 XLB 511 @ 32.009998"
    [1] "2010-08-31 00:00:00 XLB -511 @ 31.040001"
    [1] "2010-06-30 00:00:00 XLI -594 @ 27.43"
    [1] "2010-07-31 00:00:00 XLI 536 @ 30.290001"
    [1] "2010-08-31 00:00:00 XLI -536 @ 28.190001"
    [1] "2010-07-31 00:00:00 XLK 726 @ 21.92"
    [1] "2010-08-31 00:00:00 XLK -726 @ 20.690001"
    [1] "2010-07-31 00:00:00 XLP 583 @ 26.98"
    [1] "2010-08-31 00:00:00 XLP -583 @ 26.48"
    [1] "2010-07-31 00:00:00 XLU 527 @ 30.370001"
    [1] "2010-06-30 00:00:00 XLY -675 @ 29.129999"
    [1] "2010-07-31 00:00:00 XLY 513 @ 31.440001"
    [1] "2010-08-31 00:00:00 XLY -513 @ 30.190001"
    [1] "2010-09-30 00:00:00 XLB 511 @ 32.779999"
    [1] "2010-09-30 00:00:00 XLB -40 @ 32.779999"
    [1] "2010-09-30 00:00:00 XLE 276 @ 56.060001"
    [1] "2010-09-30 00:00:00 XLI 536 @ 31.280001"
    [1] "2010-09-30 00:00:00 XLI -54 @ 31.280001"
    [1] "2010-09-30 00:00:00 XLK 726 @ 23.02"
    [1] "2010-09-30 00:00:00 XLK -76 @ 23.02"
    [1] "2010-09-30 00:00:00 XLP 583 @ 27.879999"
    [1] "2010-09-30 00:00:00 XLP -47 @ 27.879999"
    [1] "2010-09-30 00:00:00 XLV 492 @ 30.49"
    [1] "2010-09-30 00:00:00 XLY 513 @ 33.41"
    [1] "2010-09-30 00:00:00 XLY -76 @ 33.41"
    [1] "2010-12-31 00:00:00 XLF 957 @ 15.95"
    [1] "2011-07-31 00:00:00 XLB -471 @ 38.009998"
    [1] "2011-08-31 00:00:00 XLE -276 @ 68.639999"
    [1] "2011-06-30 00:00:00 XLF -957 @ 15.35"
    [1] "2011-07-31 00:00:00 XLI -482 @ 34.68"
    [1] "2011-08-31 00:00:00 XLK -650 @ 24.440001"
    [1] "2011-08-31 00:00:00 XLY -437 @ 37.57"
    [1] "2011-10-31 00:00:00 XLK 672 @ 26.01"
    [1] "2011-11-30 00:00:00 XLK -672 @ 25.620001"
    [1] "2011-09-30 00:00:00 XLP -536 @ 29.66"
    [1] "2011-10-31 00:00:00 XLP 534 @ 31.040001"
    [1] "2011-09-30 00:00:00 XLV -492 @ 31.73"
    [1] "2011-10-31 00:00:00 XLY 458 @ 39.02"
    [1] "2011-11-30 00:00:00 XLY -458 @ 38.740002"
    [1] "2012-01-31 00:00:00 XLB 491 @ 37.18"
    [1] "2012-02-29 00:00:00 XLE 238 @ 74.879997"
    [1] "2012-02-29 00:00:00 XLF 1265 @ 14.76"
    [1] "2012-01-31 00:00:00 XLI 487 @ 36.220001"
    [1] "2012-01-31 00:00:00 XLK 645 @ 27.030001"
    [1] "2011-12-31 00:00:00 XLV 466 @ 34.689999"
    [1] "2011-12-31 00:00:00 XLY 458 @ 39.02"
    [1] "2011-12-31 00:00:00 XLY -44 @ 39.02"
    [1] "2012-05-31 00:00:00 XLB -491 @ 33.82"
    [1] "2012-05-31 00:00:00 XLE -238 @ 63.630001"
    [1] "2012-06-30 00:00:00 XLB 438 @ 35.290001"
    [1] "2012-06-30 00:00:00 XLB 27 @ 35.290001"
    [1] "2012-07-31 00:00:00 XLB -465 @ 34.84"
    [1] "2012-08-31 00:00:00 XLB 465 @ 35.650002"
    [1] "2012-08-31 00:00:00 XLE 246 @ 71.529999"
    [1] "2012-10-31 00:00:00 XLK -645 @ 28.870001"
    [1] "2012-11-30 00:00:00 XLU -527 @ 35.32"
    [1] "2013-01-31 00:00:00 XLK 587 @ 29.4"
    [1] "2012-12-31 00:00:00 XLP -534 @ 34.900002"
    [1] "2013-01-31 00:00:00 XLP 485 @ 36.869999"
    [1] "2013-01-31 00:00:00 XLU 485 @ 36.580002"
    [1] "2013-08-31 00:00:00 XLU -485 @ 37.299999"
    [1] "2013-10-31 00:00:00 XLU 535 @ 38.779999"
    [1] "2013-11-30 00:00:00 XLU -535 @ 38.029999"
    [1] "2014-01-31 00:00:00 XLP -485 @ 40.759998"
    [1] "2014-02-28 00:00:00 XLP 509 @ 42.349998"
    [1] "2014-01-31 00:00:00 XLU 575 @ 39.099998"
    [1] "2014-07-31 00:00:00 XLI -487 @ 51.830002"
    [1] "2014-08-31 00:00:00 XLI 432 @ 54.02"
    [1] "2014-07-31 00:00:00 XLP -509 @ 43.139999"
    [1] "2014-08-31 00:00:00 XLP 523 @ 45.130001"
    [1] "2014-09-30 00:00:00 XLE -246 @ 90.620003"
    [1] "2014-12-31 00:00:00 XLB -465 @ 48.580002"
    [1] "2015-02-28 00:00:00 XLB 497 @ 51.490002"
    [1] "2015-01-31 00:00:00 XLF -1265 @ 23.01"
    [1] "2015-02-28 00:00:00 XLF 988 @ 24.35"
    [1] "2015-03-31 00:00:00 XLB -497 @ 48.779999"
    [1] "2015-04-30 00:00:00 XLB 500 @ 50.419998"
    [1] "2015-03-31 00:00:00 XLU -575 @ 44.43"
    [1] "2015-06-30 00:00:00 XLB -500 @ 48.389999"
    [1] "2015-08-31 00:00:00 XLF -988 @ 23.43"
    [1] "2015-06-30 00:00:00 XLI -432 @ 54.060001"
    [1] "2015-06-30 00:00:00 XLK -587 @ 41.400002"
    [1] "2015-07-31 00:00:00 XLK 578 @ 42.580002"
    [1] "2015-08-31 00:00:00 XLK -578 @ 40.23"
    [1] "2015-06-30 00:00:00 XLP -523 @ 47.599998"
    [1] "2015-07-31 00:00:00 XLP 502 @ 50.310001"
    [1] "2015-08-31 00:00:00 XLP -502 @ 47.310001"
    [1] "2015-08-31 00:00:00 XLV -466 @ 70.489998"
    [1] "2015-10-31 00:00:00 XLF 1020 @ 24.08"
    [1] "2015-11-30 00:00:00 XLI 467 @ 54.740002"
    [1] "2015-10-31 00:00:00 XLK 590 @ 43.650002"
    [1] "2015-10-31 00:00:00 XLP 490 @ 49.880001"
    [1] "2015-09-30 00:00:00 XLY -414 @ 74.260002"
    [1] "2015-10-31 00:00:00 XLY 315 @ 80.970001"
    [1] "2015-12-31 00:00:00 XLF -1020 @ 23.83"
    [1] "2015-12-31 00:00:00 XLI -467 @ 53.009998"
    [1] "2016-01-31 00:00:00 XLK -590 @ 41.240002"
    [1] "2016-01-31 00:00:00 XLU 541 @ 45.419998"
    [1] "2016-01-31 00:00:00 XLY -315 @ 74.110001"
    [1] "2016-03-31 00:00:00 XLB 522 @ 44.810001"
    [1] "2016-03-31 00:00:00 XLI 422 @ 55.470001"
    [1] "2016-03-31 00:00:00 XLK 523 @ 44.360001"
    [1] "2016-03-31 00:00:00 XLY 294 @ 79.099998"
    
    > end_t<-Sys.time()
    
    > print("Strategy Loop:")
    [1] "Strategy Loop:"
    
    > print(end_t-start_t)
    Time difference of 3.293799 mins
    
    > Sys.setenv(TZ=oldtz)
    
    > # look at the order book
      > #print(getOrderBook('faber'))
      > 
      > start_t<-Sys.time()
    
    > updatePortf(Portfolio='faber',Dates=paste('::',as.Date(Sys.time()),sep=''))
    [1] "faber"
    
    > updateAcct('faber')
    [1] "faber"
    
    > end_t<-Sys.time()
    
    > print("trade blotter portfolio update:")
    [1] "trade blotter portfolio update:"
    
    > print(end_t-start_t)
    Time difference of 0.2270131 secs
    
    > # hack for new quantmod graphics, remove later
      > themelist<-chart_theme()
    
    > themelist$col$up.col<-'lightgreen'
    
    > themelist$col$dn.col<-'pink'
    
    > dev.new()
    NULL
    
    > layout(mat=matrix(1:(length(symbols)+1),ncol=2))
    
    > for(symbol in symbols){
      +     chart.Posn(Portfolio='faber',Symbol=symbol,theme=themelist,TA="add_SMA(n=10,col='darkgreen')")
      + }
    > ret1 <- PortfReturns('faber')
    
    > ret1$total <- rowSums(ret1)
    
    > print(ret1)
    if("package:PerformanceAnalytics" %in% search() || require("PerformanceAnalytics",quietly=TRUE)){
      +   getSymbols("SPY", src='yahoo', index.class=c("POSIXt","POSIXct"), from='1999-01-01')
      +   SPY<-to.monthly(SPY, indexAt='lastof')
      +   SPY.ret <- Return.calculate(SPY$SPY.Close)
      +   dev.new()
      +   charts.PerformanceSummary(cbind(ret1$total,SPY.ret), geometric=FALSE, wealth.index=TRUE)
      + }
    faber.stats<-tradeStats('faber')[,c('Net.Trading.PL','Max.Drawdown','Num.Trades','Profit.Factor','Std.Dev.Trade.PL','Largest.Winner','Largest.Loser','Max.Equity','Min.Equity')]
    
    > faber.stats
    