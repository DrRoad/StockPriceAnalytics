###############################################################################
# UI for Stock Price Analytics app
# Contains the following:
#   1. Text input for stock ticker
#   2. Chart area for historical data
#   3. Chart area for intraday chart (historical)
#   4. Chart area for intraday chart (momentum)
#   5. Chart area for risk return chart
#   6. Settings tab
#   7. Help tab
###############################################################################

# Load library ----------------------------------------------------------------
library(shiny)
library(shinythemes)
library(plotly)

# Source files ----------------------------------------------------------------
source("./support/Intraday Stock Chart.R")
source("./support/Yahoo Stock Data Pull.R")
source("./support/Stock Momentum Chart.R")
source("./support/Historical Stock Chart.R")
source("./support/Risk Return Chart.R")

# UI design -------------------------------------------------------------------
shinyUI(
  navbarPage(
    # Nav bar settings --------------------------------------------------------
    windowtitle = "SPAn",
    theme = shinytheme("flatly"),
    title = span(style = "font-size: 20px", 'Stock Price Analytics (SPAn)', 
                 span(style = 'font-size:10px','v1.0')),
    
    # Analytics tab -----------------------------------------------------------
    tabPanel(
      title = "Analytics",
      
      # Row to input ticker
      wellPanel(  # For arranging the note at the bottom
        fluidRow(
          column(3, textInput("tickerInput", "Ticker (Yahoo Finance)", "AXISBANK.NS")),
          column(3, textInput("benchmark", "Benchmark", "^NSEI")),
          column(2, actionButton("tickerUpdate", "Update"))),
        
        helpText(
          tags$li("Only ticker symbols from Yahoo Finance are supported. Example: ^NSEI is for Nifty 50")
          #,tags$li("Click on 'Update' once. Each tab (below) will populate when clicked (takes a few seconds)"),
          #tags$li("Change ticker symbols and hit update again")
          )),
      
      tags$style(type = "text/css", "#tickerUpdate {margin-top: 25px}"),  # Adjust button height
      
      hr(),  # Horizontal line
      
      tabsetPanel(
        # Historical chart panel has 4 separate charts
        tabPanel(
          title = "Historical Chart", 
          helpText("Note: Click on graph legends to switch on/off items"),
          
          br(), br(),  # Vertical space
          
          fixedRow(
            column(6, plotlyOutput("histPlot1", height = "400px")),
            column(6, plotlyOutput("histPlot2", height = "400px"))),
          
          br(), br(),  # Vertical space
          
          fixedRow(
            column(6, plotlyOutput("histPlot3", height = "400px")),
            column(6, plotlyOutput("histPlot4", height = "400px")))),
        
        tabPanel(
          title = "Risk Return Chart", 
          helpText("Note:",
                   tags$li("Drag zoom if needed"),
                   tags$li("Universe of stocks is NIFTY 50"),
                   tags$li("Log returns are used")),
          
          br(), br(),  # Vertical space
          
          plotlyOutput("riskreturnPlot", height = "800px")),
        
        tabPanel(
          title = "Intraday Chart", 
          helpText("Note:",
                   tags$li("Pulls in most recent intraday data"),
                   tags$li("Specifiy frequency in settings tab")),
          
          br(), br(),  # Vertical space
          
          plotOutput("intraPlot", height = "500px")),
        
        tabPanel(
          title = "Momentum Chart", 
          
          helpText("Note:",
                   tags$li("Pulls in most recent intraday data only (frequency = minute)")),
          
          br(), br(),  # Vertical space
          
          plotOutput("momPlot", height = "500px")))),
    
    # Settings tab ------------------------------------------------------------
    tabPanel(
      title = "Settings",
      
      h4("Note: Settings have reasonable default values. Change only if needed."),
      
      # Row for historical inputs
      wellPanel(
        fixedRow(
          column(2, textInput("startdate", "Start Date for Data Pull", "2010-01-01")),
          column(2, textInput("freq", "Frequency", "d")),
          column(3, offset = 5,  h4(style = "margin-top:35px","Historical / Risk Return Charting"))),
        helpText("Note: d : Daily w : Weekly m : Monthly")),
      
      # Row for Intraday inputs
      wellPanel(
        fixedRow(
          column(2, textInput("intraLength", "Intraday data window length", "3d")),
          column(3, offset = 7,  h4(style = "margin-top:35px","Intraday Charting"))),
        helpText("Note: 1d : one day 2d : two day and so on...")),
      
      # Row for Momentum inputs
      wellPanel(
        fixedRow(
          column(3, numericInput("width", "Window width for rolling regression", 15)),
          column(3, numericInput("LAG", "Lag to be used in rolling regression", 5)),
          column(3, offset = 3,  h4(style = "margin-top:35px","Momentum Charting"))),
        helpText("Note: Refer to help section for details")),
      
      # Row for Time Zone inputs
      wellPanel(
        fixedRow(
          column(3, textInput("timezone", "Time Zone for data being pulled","Asia/Kolkata")),
          column(3, offset = 6,  h4(style = "margin-top:35px","Time Zone"))),
        helpText("Note: Refer to help section for details"))),
    
    # Help tab ----------------------------------------------------------------
    tabPanel(
      title = "Help",
      wellPanel(
        h4("How to use this tool:"),
        hr(style = "border: 3px solid #ccc;"),
        helpText(tags$ul(
          tags$li("Get ticker symbol from Yahoo Finance"),
          tags$li("Simply put in the ticker symbol for stock and benchmark in the textbox(s) in the Analytics tab and click on Update")))),
      
      wellPanel(
        h4("Details of charts:"),
        hr(style = "border: 3px solid #ccc;"),
        
        h5("Historical Charts"),
        helpText(tags$ul(
          tags$li("Four separate charts are shown",
                  tags$ul(
                    tags$li("First chart plots stock prices with traded volume (as bars)"),
                    tags$li("Second chart plots the stock and chosen benchmark"),
                    tags$li("Third chart plots rolling correlations using pre-defined window lengths based on data frequency chosen"),
                    tags$li("Last chart shows volume weighted moving averages at pre-defined window lengths based on data frequency chosen"))),
          tags$li("Use chart legend to toggle items like moving averages and rolling correlations on / off"))),
        
        hr(style = "border: 1px solid #ccc;"),  #Horizontal line
        
        h5("Risk Return Chart"),
        helpText(tags$ul(
          tags$li("Charts returns vs standard deviations at pre-specified window lengths",
         tags$li("Overlays benchmark data (proxy for market) with chosen stock information"),
         tags$li("Compares a stock's 3mo, 6mo and 1yr returns with those of other stocks")))),
        
        hr(style = "border: 1px solid #ccc;"),  #Horizontal line
        
        h5("Intraday Chart"),
        helpText(tags$ul(
          tags$li("Intraday chart plots intraday data (at per min frequency) across a day"),
          tags$li("This done for the specified time horozon (in days)"),
          tags$li("Hence, data for mutiple days is collapsed using time of day as the anchor"),
          tags$li("The chart plots only closing prices"))),
        
        hr(style = "border: 1px solid #ccc;"),  #Horizontal line
        
        h5("Momentum Chart"),
        helpText(tags$ul(
          tags$li("The Momentum Chart tries to visualise intraday momentum using rollng regressions"),
          tags$li("The chart pulls in the most recent intraday data available (either active or one day prior)"),
          tags$li("Minute by minute closing prices are regressed on :",
                  tags$ul(
                    tags$li("Diff calculated as:",
                            tags$ul(
                              tags$li("High minus Close (a)"),
                              tags$li("Close minus Low (b)"),
                              tags$li("Diff = max(a , b)"))),
                    tags$li("Trend of closing prices"))),
          tags$li("The idea is - if the the closing price is being pulled more towards the High price then
                momentum is being built up and vice versa")))),
      
      wellPanel(h6(paste("Last Updated:", "December 13th 2015")))
    )))







