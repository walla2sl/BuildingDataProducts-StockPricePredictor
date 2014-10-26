
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(function(input, output) {

  prices <- reactive({
    if(is.null(input$symbols))
      return()
    symbol<-input$symbols
    prices<-getPrices(symbol)
  })
  
  output$symbols<-renderUI({
    
    selectInput("symbols", "Stock Symbol", choices=as.vector(getSymbols()$Ticker))
  }) 
  
  output$stockPlot <- renderPlot({
    if(is.null(prices()))
      return()
    prices<-prices()
    isolate({
      plot(prices$Date,prices$Close,xlab="Date",ylab="Closing Price ($)",
           col=ifelse(prices$Date>=Sys.Date(), "red", "black"),
           main="Stock Price Trend with Neural Network Prediction"
           )
      grid(NULL, nx=NA, lty = 1)
      lines(head(prices$Date,-5), head(prices$Close,-5), col = "blue")
      lines(tail(prices$Date,6), tail(prices$Close,6), col = "red", lty=2)
    })
  })
})
