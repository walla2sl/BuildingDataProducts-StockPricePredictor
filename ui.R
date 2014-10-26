library(shiny)
source('global.R')

shinyUI(fluidPage(

  # Application title
  titlePanel("Neural Network Stock Price Predictor"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      helpText("Pick a stock ticker symbol from the drop",
               "down list above to view the past 180 days",
               "of actual closing prices and the predicted",
               "closing price for the next 5 days."),
      uiOutput("symbols"),
      helpText("Note: list above includes all companies",
               "currently in the S&P 500")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("stockPlot"),
      helpText("Note: actual values in Blue; predicted values in Red", align = "center")
    )
  )
))
