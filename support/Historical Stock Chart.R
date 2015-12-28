###############################################################################
# Function to create 4 separate historical charts for stock kprices
# Woks for historical data only
# Function works as such:
#   1. Pulls data using the GetYahooData function
#   2. Pulls data for a stock as well as a benchmark
#   3. Uses Plotly to create charts
#   4. Separate charts created are:
#     a. Stock price and volume
#     b. Stock price and benchmark price
#     c. Rolling correlations with benchmark
#     d. Volume weighted trend lines (moving averages)
# 
# Function arguments:
#   1. stock = Ticker for the stock
#   2. bench = Ticker for the benchmark
#   3. start.date = start date for data pull (end date is default system date)
#   4. freq = frequency of data pull   
###############################################################################

HistoricalChart <- function(stock = "AXISBANK.NS",
                            bench = "^NSEBANK",
                            start.date = "2010-01-01",
                            freq = "d"){
  
  # Load libraries --------------------------------------------------------------
  #install.packages("plotly")
  #library(plotly)
  library(zoo)
  library(RColorBrewer)
  library(TTR)
  
  # Choose an appropriate width for rolling corerlations ------------------------
  width = switch(freq,
                 d = c(15, 30, 90, 180),
                 w = c(2, 4, 12, 24),
                 m = c(3, 6, 9, 12))
  
  # Get stock and benchmark data ------------------------------------------------
  mat = GetYahooData(stock = stock, freq = freq, intraday = F, start.date = start.date)
  bench.mat = GetYahooData(stock = bench, freq = freq, intraday = F, start.date = start.date)
  
  # Match dates between stock and benchmark 
  # Uses character matching nt merge
  mtch = match(x = mat$Date, table = bench.mat$Date)
  mat$Bench = bench.mat$Adj.Close[mtch]
  
  #Remove any NAs. For indices at daily frequency dates don't match up
  mat = na.omit(mat)
  
  # Create overlay function for second y axis -----------------------------------
  # See https://plot.ly/r/multiple-axes/
  new_y <- list(
    overlaying = "y",
    side = "right"
  )
  
  # Historical prices and volume ------------------------------------------------
  title1 = switch(freq,
                  d = paste("Historical Chart for", stock, "Freqency: Daily"),
                  w = paste("Historical Chart for", stock, "Freqency: Weekly"),
                  m = paste("Historical Chart for", stock, "Freqency: Monthly"))
  
  plt1 = mat %>%
    plot_ly(x = Date, y = Adj.Close, name = "Hist. Prices", line = list(width = 2)) %>%
    add_trace(x = Date, y = Volume, type = "bar", name = "Volume", yaxis = "y2",
              opacity = 0.2,  marker = list(color = "#000000")) %>%
    layout(title = title1, yaxis2 = new_y)
  
  # Historical prices and Benchmark ---------------------------------------------
  title2 = paste0(stock, " vs benchmark (", bench, ")")
  plt2 = mat %>%
    plot_ly(x = Date, y = Adj.Close, name = "Hist. Prices", line = list(width = 2)) %>%
    add_trace(x = Date, y = Bench, name = "Benchmark", yaxis = "y2",
              line = list(color = "#808080", width = 2)) %>%
    layout(title = title2, yaxis2 = new_y, plot_bgcolor = "#ffffff")
  
  # Rolling correlations --------------------------------------------------------
  idx = nrow(mat):1  # Note that the original data frame has data in reverse chronological order
  
  # Run rolling correlations for each value of width
  rollCor = lapply(width, FUN = function(width){
    rollapply(data = idx, width = width, align = "right", FUN = function(x){
      cor(mat$Adj.Close[x], mat$Bench[x])
    })})
  
  # Define a function to add NAs to the top as needed
  # # x should be the longer vector
  # Function will return the shorter vector with NAs added
  addNA = function(x, y){
    
    N = length(x) - length(y)
    y = c(rep(NA, N),y)
    
    return(y)
  }
  
  # Create a dataframe to plot with the right dates
  # Dates = dates from mat shifted with the first width
  # Dates have to be reversed
  rollCor.plot = data.frame(Date = rev(mat$Date),
                            roll1 = addNA(mat$Adj.Close, rollCor[[1]]),
                            roll2 = addNA(mat$Adj.Close, rollCor[[2]]),
                            roll3 = addNA(mat$Adj.Close, rollCor[[3]]),
                            roll4 = addNA(mat$Adj.Close, rollCor[[4]]))
  
  # Plot using sequential colors from RColorBrewer
  cols = brewer.pal(9, name = "Blues")[c(3, 5, 7, 9)]
  plt3 = rollCor.plot %>% 
    plot_ly(x = Date, y = roll1, name = paste(width[1], freq, "Rolling Window"), 
            line = list(color = cols[1], shape = "spline", smoothing = 1)) %>% 
    
    add_trace(y = roll2, name = paste(width[2], freq, "Rolling Window"),
              line = list(color = cols[2], dash = "2px")) %>% 
    
    add_trace(x = Date, y = roll3, name = paste(width[3], freq, "Rolling Window"), 
              line = list(color = cols[3], dash = "6px")) %>%
    
    add_trace(x = Date, y = roll4, name = paste(width[4], freq, "Rolling Window"),
              line = list(color = cols[4], dash = "10px")) %>%
    
    layout(title = paste("Rolling Correlations for", stock),
           yaxis = list(title = "Rolling Correlations"))
  
  # Moving Average lines  -----------------------------------------------------
  # Uses the VWMA function for TTR
  MA.mat = data.frame(Date = rev(mat$Date),
                      ma1 = VWMA(rev(mat$Adj.Close), rev(mat$Volume), n = width[1]),
                      ma2 = VWMA(rev(mat$Adj.Close), rev(mat$Volume), n = width[2]),
                      ma3 = VWMA(rev(mat$Adj.Close), rev(mat$Volume), n = width[3]),
                      ma4 = VWMA(rev(mat$Adj.Close), rev(mat$Volume), n = width[4]),
                      Adj.Close = rev(mat$Adj.Close),
                      High = rev(mat$High),
                      Low = rev(mat$Low))
  
  # Colors
  cols = brewer.pal(8, name = "Accent")[c(3, 5, 7, 9)]
  # Line setttings
  l <- list(
    color = "#bfbfbf"
  )
  
  # Plot
  plt4 = MA.mat %>% 
    plot_ly(x = Date, y = ma1, name = paste(width[1], freq, "Moving Window"), 
            line = list(color = cols[1], width = 2)) %>% 
    
    add_trace(y = ma2, name = paste(width[2], freq, "Moving Window"),
              line = list(color = cols[2], dash = "2px")) %>% 
    
    add_trace(x = Date, y = ma3, name = paste(width[3], freq, "Moving Window"), 
              line = list(color = cols[3], dash = "6px")) %>%
    
    add_trace(x = Date, y = ma4, name = paste(width[4], freq, "Moving Window"),
              line = list(color = cols[4], dash = "10px")) %>%
    
    add_trace(x = Date, y = Adj.Close, name = "Hist. Prices", line = l, fill = "tonexty", yaxis = "y2") %>%
    
    layout(title = paste("Vol. Weighted Moving Avg.", stock), yaxis = list(title = "VWMA"), yaxis2 = new_y)
  
  # Returns the plots ---------------------------------------------------------
  ret = list(plot1 = plt1,
             plot2 = plt2,
             plot3 = plt3,
             plot4 = plt4)
  
  return(ret)
  
}