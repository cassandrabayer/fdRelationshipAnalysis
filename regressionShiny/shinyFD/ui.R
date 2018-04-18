
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#


shinyUI(fluidPage(
  headerPanel("Regression Testing"), 
  sidebarPanel(
    p("Select the inputs for the Independent Variable"),
    selectInput(inputId = "IndVar", label = "Independent Variables", multiple = FALSE, choices = list("age", "sex", "tookTreatment", "counterBetweenFlares", "counterDuringFlares", "winter", "summer", "spring", "fall")),
    p("Select the inputs for the Dependent Variable"),
    selectInput(inputId = "DepVar", label = "Dependent Variables", multiple = FALSE, choices = list( "avgPain"))
  ),
  mainPanel(
    verbatimTextOutput(outputId = "RegSum"),
    verbatimTextOutput(outputId = "IndPrint"),
    verbatimTextOutput(outputId = "DepPrint")
    #plotOutput("hist")
  )
))