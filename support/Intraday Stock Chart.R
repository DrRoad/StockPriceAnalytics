###############################################################################
# Function to plot intrday stock data
# Works only for intraday data
# Depends on the the function "GetYahooData" contained in "Yahoo Stock Data Pull.R"
# Works as such:
#   1. Pulls in data for the specified ticker
#   2. Extracts the HH:MM component of the UNIX time stamp in the data returned
#   3. Simply plots the data using boxplots
# Arguments:
#   1. stock = Ticker (YAHOO Tickers only)
#   2. freq = Intraday frequency (2day, 5 day, 10day etc)
#   3. TimeZone = Timezone of stock exchange (defeault Asia / Kolkata)
###############################################################################

IntradayChart <- function(stock = "%5ENSEI",
                          freq = "2d",
                          timezone = "Asia/Kolkata") {

  #Load Libraries -------------------------------------------------------------
  library(ggplot2)
  library(ggthemes)

  # Pull in data --------------------------------------------------------------
  df = GetYahooData(stock = stock, intraday = T, intraLength = freq)

  # Use only closing prices
  df = df[,1:2]

  # Convert dates to times only -----------------------------------------------
  # This will help collapse the data on time
  # Convert from UNIX data to human readable format
  idx = as.POSIXct(df$Date, origin = "1970-01-01", tz = timezone)

  # Extract HH:MM:SS component
  idx = substr(x = idx, start = 12, stop = 19)

  # Extract HH:MM component
  idx = substr(x = idx, start = 1, stop = 5)

  # Combine with original dataframe
  df$Date = idx

  # Chart ---------------------------------------------------------------------
  if (freq == "1d"){

    # If only a 1 day chart is needed use line plot ---------------------------
    plt = ggplot(df, aes(x = Date, y = close)) +
      geom_point(colour = "#ff4d4d", size = 4, shape = 8) +
      ggtitle(paste("Intraday plot for ", stock, "\nFrequency = ", freq)) +
      theme_fivethirtyeight() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))

    print(plt)
  }else{

    # Else use a box plot -----------------------------------------------------
    plt = ggplot(df, aes(x = Date, y = close)) +
      geom_boxplot(col = "#4d79ff") +
      ggtitle(paste("Intraday plot for ", stock, "\nFrequency = ", freq)) +
      theme_fivethirtyeight() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))

    print(plt)
  }

}
