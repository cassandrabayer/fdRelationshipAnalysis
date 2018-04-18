
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(data.table)
RegData <- data.table(read.csv("depression.csv"))

shinyServer(function(input, output) {
  
  lm1 <- reactive({lm(reformulate(input$IndVar, input$DepVar), data = RegData)})
  
  output$DepPrint <- renderPrint({input$DepVar})
  output$IndPrint <- renderPrint({input$IndVar})
  output$RegSum <- renderPrint({summary(lm1())})
  
})
