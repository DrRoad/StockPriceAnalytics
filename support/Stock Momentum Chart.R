###############################################################################
# Function to chart iintraday data (actively) and perform rolling regressions.
# Minute by minute closing prices are regressed on :
#   1. Diff calculated as:
#       a. High minus Close
#       b. Close - Low
#       c. Diff = max(a , b)
#   3. Trend of closing prices
# Idea: if the the closing price is being pulled more towards the High price
#       then momentum is being built up and vice versa
# Arguments:
#   1. stock = Ticker Symbol from Yahoo Finance
#   2. timezone = Time zone of data being used. Default = "Asia/Kolkata"
#   3. width = Width of rolling window for regression
#   4. LAG = Time lag used in the Diff variable described above

# Function works thus:
#   1. Pull data for ticker from Yahoo
#   2. Created the Diff variabel described above
#   3. Runs rolling regressions
#   4. Charts prices as well as coefficient on Diff variable with statistical
#      signifiance used as a color guide
###############################################################################

MomentumChart <- function(stock = "%5ENSEI",
                          timezone = "Asia/Kolkata",
                          width = 15,  #For rolling regression
                          LAG = 5){
  # Load libraries --------------------------------------------------------------
  library(forecast)
  library(xts)
  library(ggthemes)
  library(dynlm)
  library(gridExtra)

  # Get data --------------------------------------------------------------------
  df = GetYahooData(stock, intraday = T, intraLength = "1d")

  df$diff = apply(df, 1, function(x){
    a = x[3] - x[2]  # High minus Close
    b = x[2] - x[4]  # Close minus low

    return(max(a, b))
  })

  # df$diff = df$high - df$low
  df = xts(df[,-1], order.by = as.POSIXct(df[,1], origin = "1970-01-01", tz = timezone))

  # Plot ------------------------------------------------------------------------
  df.plot = data.frame(Date = index(df),
                       Close = as.numeric(df[,1]),
                       High = as.numeric(df[,2]),
                       Low = as.numeric(df[,3]),
                       Diff = as.numeric(df[,6]))

  plt1 = ggplot(df.plot, aes(x = Date)) +
    geom_smooth(aes(y = Close, ymax = High, ymin = Low), stat = "identity") +
    ggtitle(paste("Intraday plot for ", stock)) +
    theme_fivethirtyeight() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

  # Some analytics --------------------------------------------------------------
  # Seems that the magnitude of High / Low can be used as an indicator
  # If at a certain point in time the low price >> high price the stock starts
  # to dip and vice versa
  # Fit a rolling regression

  idx = 1:nrow(df)
  fit = rollapply(data = idx, width = width, align = "right", FUN = function(x){
    mat = df[x,]
    mdl = dynlm(close ~ trend(close) + L(diff, -LAG), data = mat)

    #Return the coefficient on diff and its p-value
    mat = c(summary(mdl)$coefficients[2,1],
            summary(mdl)$coefficients[2,4])
    return(mat)
  })

  # Add dates to fit
  # Add NA's in front to help line up the plots later on
  fit = data.frame(Date = df.plot$Date,
                   Beta = c(rep(0, (width - 1)),fit[,1]),
                   Sig = c(rep(0, (width - 1)),fit[,2]))
  fit$Idx = 1:nrow(fit)

  # Plot both prices and regression output together ------------------------------------
  plt2 = ggplot(fit, aes(x = Idx, y = Beta)) +
    geom_bar(aes(fill = Sig), stat = "identity", position = "dodge") +
    ggtitle(paste("Rolling High Low Beta for ", stock, "\nWidth = ", width, "\tLag = ", LAG)) +
    theme_fivethirtyeight() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

  grid.arrange(plt1, plt2, ncol = 1)
}


