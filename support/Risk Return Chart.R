###############################################################################
# Function to draw returns vs std dev with overlays
# Function works as such:
#   1. Reads in benchmark data
#   2. Pulls data for stock input
#   3. Charts returns across specified window lengths vs Std Devs
#   
# Function takes the following arguments:
#   1. stock = ticker
#   2. freq = frequency of data pull
#   3. start.date = start date for data pull
###############################################################################

RiskReturnChart <- function(stock = "SBIN.NS",
                            freq = "d",
                            start.date = "2010-01-01"){
  # Load Library ----------------------------------------------------------------
  library(plotly)
  library(PerformanceAnalytics)
  library(reshape2)
  library(zoo)
  
  # Read in benchmark data and merge with current stock data --------------------
  filename = paste0("./support/Benchmarkdata", freq, ".csv")
  stock.data = read.csv(filename, stringsAsFactors = F)
  stock.data = zoo(stock.data[,-1], order.by = as.Date(stock.data$Date))
  
  # Get data for particular stock
  # Get only if not already in universe
  if(! stock %in% colnames(stock.data)){
  mat = GetYahooData(stock = stock, freq = freq, start.date = start.date, intraday = F)
  mat = zoo(mat$Adj.Close, order.by = as.Date(mat$Date))
  
  # Merge with benchmark data
  stock.data = merge.zoo(stock.data, mat)
  stock.data = na.omit(stock.data)
  
  # Update column names
  colnames(stock.data)[ncol(stock.data)] = stock
  }
  
  # Get appropriate window for return calculation -------------------------------
  width = switch(freq,
                 d = c(90, 180, 365),
                 w = c(12, 24, 52),
                 m = c(3, 9, 12))
  
  # Calculate log returns -------------------------------------------------------
  ret.mat = CalculateReturns(prices = stock.data, method = "log")
  
  # Calculate returns for each window and respective standard deviations --------
  n = nrow(ret.mat)
  ret.plot = NULL
  sd.plot = NULL
  
  for (i in 1:ncol(ret.mat)) {
    
    x = sapply(width, FUN = function(x){
      sum(ret.mat[,i][n:(n - x + 1)])  # Log returns hence added
    })
    
    y = sapply(width, FUN = function(x){
      sd(ret.mat[,i][n:(n - x + 1)])  # Std dev for returns in window
    })
    
    ret.plot = rbind(ret.plot, x)
    sd.plot = rbind(sd.plot, y)
  }
  
  colnames(ret.plot) = paste0(width, freq)
  colnames(sd.plot) = paste0(width, freq)
  row.names(ret.plot) = row.names(sd.plot) = colnames(stock.data)
  
  # Convert from wide to long form
  ret.plot = melt(ret.plot)
  sd.plot = melt(sd.plot)
  
  # Create data frame needed for plot -------------------------------------------
  plot.mat = data.frame(Ticker = ret.plot$Var1, 
                        Type = ret.plot$Var2, 
                        Returns = round(ret.plot$value,3),
                        SD = round(sd.plot$value,3),
                        Size = round(1/sd.plot$value,3))
  
  # Plot Settings ---------------------------------------------------------------
  xlim.low = 0.05
  xlim.high = max(plot.mat$Returns) + 0.05
  xlim.high.neg = min(plot.mat$Returns) - 0.05
  ylim.low = 0.08
  ylim.high = max(plot.mat$SD)
  opac = 0.10
  
  # Separate the stock data from benchmark data ---------------------------------
  plot.mat.sub = subset(plot.mat, subset = Ticker == stock)
  
  
  plt = plot.mat %>% 
    plot_ly(x = Returns, y = SD, size = Size, group = Type, mode = "markers",
            marker = list(opacity = pnorm(Returns))
    ) %>% 
    
    layout(title = "Return vs Std (Market vs Stock)", plot_bgcolor = "#e6e6e6",
           shapes = list(
             list(type = "rect", 
                  fillcolor = "green", line = list(color = "green"), opacity = opac, 
                  x0 = xlim.low, x1 = xlim.high, xref = "x", 
                  y0 = 0, y1 = ylim.high, yref = "y"),
             
             list(type = "rect", 
                  fillcolor = "blue", line = list(color = "blue"), opacity = opac, 
                  x0 = xlim.low, x1 = xlim.high, xref = "x", 
                  y0 = ylim.low, y1 = ylim.high, yref = "y"),
             
             list(type = "rect", 
                  fillcolor = "red", line = list(color = "red"), opacity = opac - 0.05, 
                  x0 = xlim.low, x1 = xlim.high.neg, xref = "x", 
                  y0 = 0, y1 = ylim.low, yref = "y"),
             
             list(type = "rect", 
                  fillcolor = "red", line = list(color = "red"), opacity = opac,  
                  x0 = xlim.low, x1 = xlim.high.neg, xref = "x", 
                  y0 = ylim.low, y1 = ylim.high, yref = "y")),
           
           annotations = list(
             list(x = xlim.high - xlim.high/10, 
                  y = ylim.low - ylim.low/20,
                  text = "Rel. Safe", 
                  font = list(size = 14, family = "Arial Black"),
                  showarrow = F),
             
             list(x = xlim.high - xlim.high/10, 
                  y = ylim.high - ylim.high/20,
                  font = list(size = 14, family = "Arial Black"),
                  text = "Risky", showarrow = F),
             
             list(x = xlim.high.neg - xlim.high.neg/10, 
                  y = ylim.high - ylim.high/20,
                  font = list(size = 14, family = "Arial Black"),
                  text = "Unsafe", showarrow = F),
             
             list(x = plot.mat.sub$Returns[1],
                  y = plot.mat.sub$SD[1],
                  font = list(size = 10, family = "Arial"),
                  text = paste(stock, plot.mat.sub$Type[1]),
                  showarrow = T),
             
             list(x = plot.mat.sub$Returns[2],
                  y = plot.mat.sub$SD[2],
                  font = list(size = 10, family = "Arial"),
                  text = paste(stock, plot.mat.sub$Type[2]),
                  showarrow = T),
             
             list(x = plot.mat.sub$Returns[3],
                  y = plot.mat.sub$SD[3],
                  font = list(size = 10, family = "Arial"),
                  text = paste(stock, plot.mat.sub$Type[3]),
                  showarrow = T)))
  
  plt = plot.mat.sub %>% add_trace(x = Returns, y = SD, name = stock, 
                                   marker = list(symbol = "cross", opacity = 1, size = 10))
  return(plt)
}

