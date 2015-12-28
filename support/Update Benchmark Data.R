###############################################################################
# Function to update benchmark (Example NIFTY 50) data
# This is done so as to reduce the number of data pulls from Yahoo
# Function works as such:
#   1. Reads a list of tickers 
#   2. Downloads data from Yahoo
#   3. Writes to file
#
# Function takes the following arguments:
#   1. freq = frequency of data to be pulled (d, w, m)
#   2. start.date = starting date of data pull
#   
###############################################################################

UpdateBenchmarkData = function(freq = "d", 
                               start.date = "2010-01-01"){
  
  #Load Libraries 
  library(zoo)
  
  # Source GetYahooData function
  source("Yahoo Stock Data Pull.R")
  
  # Read universe of stocks -----------------------------------------------------
  ticker = read.csv("Stock List.csv", stringsAsFactors = F)
  ticker = ticker$Symbol
  
  # Data pull for each stock ----------------------------------------------------
  
  # Pull in the first ticker
  print(ticker[1])  # Print to console
  stock.data = GetYahooData(ticker[1], start.date = start.date, freq = freq, intraday = F)
  
  # Convert to zoo
  stock.data = zoo(stock.data$Adj.Close, order.by = as.Date(x = stock.data$Date))
  
  # Loop through each ticker and pull data
  for (i in 2:length(ticker)) {
    
    print(ticker[i])  # Print to console
    
    z = GetYahooData(stock = ticker[i],
                     start.date = start.date,
                     freq = freq,
                     intraday = F)
    
    z = zoo(z$Adj.Close, order.by = as.Date(z$Date))
    stock.data = merge.zoo(stock.data, z)
  }
  
  # Update column names and remove NAs
  colnames(stock.data) = ticker
  stock.data = na.omit(stock.data)
  stock.data = data.frame(Date = as.character(index(stock.data)), stock.data)
  
  # Write file to disk
  filename = paste0("Benchmarkdata", freq,".csv")
  write.csv(stock.data, file = filename, row.names = F)
}