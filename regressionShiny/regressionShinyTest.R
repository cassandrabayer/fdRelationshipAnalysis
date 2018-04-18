library(shiny)
library(data.table)

# Shiny -------------------------------------------------------------------
RegData <- data.table(read.csv("depression.csv"))


server <- function(input, output) {
  
  lm1 <- reactive({lm(reformulate(input$IndVar, input$DepVar), data = RegData)})
  
  output$DepPrint <- renderPrint({input$DepVar})
  output$IndPrint <- renderPrint({input$IndVar})
  output$RegSum <- renderPrint({summary(lm1())})
  
}

shinyApp(ui = ui, server = server)