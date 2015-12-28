###############################################################################
# SERVER for Stock Price Analytics app
# Handles the following:
#   1. Text input for stock ticker
#   2. Update button for stock ticker
#   3. Historical charting
#   4. Intraday Charting
#   5. Momentum charting
###############################################################################

# Server code -----------------------------------------------------------------
shinyServer(function(input, output){
  observeEvent(input$tickerUpdate,{

    # Get inputs symbol from input
    start.date = input$startdate
    freq = input$freq
    intraLength = input$intraLength
    width = input$width
    LAG = input$LAG
    timezone = input$timezone
    ticker = input$tickerInput
    bench = input$benchmark

    # Plot historical intraday chart
    output$intraPlot = renderPlot(IntradayChart(stock = ticker, freq = intraLength, timezone = timezone))

    # Plot momentum chart
    output$momPlot = renderPlot(MomentumChart(stock = ticker, timezone = timezone, width = width, LAG = LAG))
    
    # Historical charts
    histPlots = HistoricalChart(stock = ticker, start.date = start.date, freq = freq, bench = bench)
    output$histPlot1 = renderPlotly(histPlots[[1]])
    output$histPlot2 = renderPlotly(histPlots[[2]])
    output$histPlot3 = renderPlotly(histPlots[[3]])
    output$histPlot4 = renderPlotly(histPlots[[4]])
    
    #Risk Return Chart
    output$riskreturnPlot = renderPlotly(RiskReturnChart(stock = ticker, freq = freq, start.date = start.date))
  })
})