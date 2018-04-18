library(shiny)
library(data.table)


# Processing --------------------------------------------------------------
fd[, avgPain := 0]
fd[!is.na(trackable_value), avgPain := mean((trackable_value[trackable_type == "Condition" |
                                                               trackable_type == "Symptom"])), by = .(user_id, checkin_date)]

fd[, tookTreatment := 0]
fd[trackable_type == "Treatment", tookTreatment := 1]
fd[tookTreatment==1]

## find average pain time period
# First find the max pain by user
fd[, maxPain := 0]
fd[trackable_type %in% c("Condition", "Symptom"), maxPain := as.numeric(max(trackable_value, na.rm = T)), by = .(user_id)]
# make binary for any time user pain is between max and 75% percentile of pain (maybe 60)
fd[, highPain := 0]
fd[between(x = avgPain, 3,4), highPain := 1]
fd[, lowPain := 0]
fd[highPain == 0, lowPain := 1]

# find days between high pain
fd[, consecutiveDays := 0]
fd[, consecutiveDays := checkin_date - shift(checkin_date), by = user_id]
fdPeriodicity <- unique(fd[,.(user_id, checkin_date, highPain, lowPain, tookTreatment)])
fdPeriodicity <- fdPeriodicity[order(user_id, checkin_date)]
fdPeriodicity[, counter := 0]
fdPeriodicity[, counter := as.numeric(seq_len(.N)), by = .(user_id, rleid(highPain))]
fdPeriodicity[, counterBetweenFlares := 0]
fdPeriodicity[highPain ==0, counterBetweenFlares := counter]
fdPeriodicity[, counterDuringFlares := 0]
fdPeriodicity[highPain ==1, counterDuringFlares := counter]

# merge data back on
fdSubset <- unique(fd[trackable_type %in% c("Condition", "Symptom"),.(user_id, age, sex, checkin_date, avgPain)])
setkey(fdSubset, user_id, checkin_date)
setkey(fdPeriodicity, user_id, checkin_date)
fdPeriodicity <- fdPeriodicity[fdSubset]
fdPeriodicity <- fdPeriodicity[!is.na(avgPain) & !is.na(counter) & !is.na(counterBetweenFlares) & !is.na(counterDuringFlares)]


# Analysis ----------------------------------------------------------------
## relationships between timing and pain and treatments (dummy)
cor(fdPeriodicity$avgPain, fdPeriodicity$counterBetweenFlares)
lm1 <- lm(data = fdPeriodicity, formula = avgPain ~ counterBetweenFlares)
cor(fdPeriodicity$avgPain, fdPeriodicity$counterDuringFlares)
lm2 <- lm(data = fdPeriodicity, formula = avgPain ~ counterDuringFlares)
lm3 <- lm(data = fdPeriodicity, formula = avgPain ~ tookTreatment)
lm4 <-lm(data = fdPeriodicity, formula = avgPain ~ counterDuringFlares + tookTreatment)
lm5 <-lm(data = fdPeriodicity, formula = avgPain ~ counterBetweenFlares + tookTreatment)


## More targetted Analysis
depression <- fd[trackable_type %in% c("Condition", "Symptom","Treatment") &
                   trackable_name %in% c("depression", "depressed","celexa", "citalopram", "bupropion", "wellbutrin", "welbutrin")]
depression[, avgPain := 0]
depression[!is.na(trackable_value), avgPain := mean((trackable_value[trackable_type == "Condition" |
                                                                       trackable_type == "Symptom"])), by = .(user_id, checkin_date)]

depression[, tookTreatmentC := 0]
depression[trackable_name %in% c("celexa", "citalopram"), 
           tookTreatmentC := 1]
depression[, tookTreatmentW := 0]
depression[trackable_name %in% c("welbutrin", "wellbutrin", "bupropion"), 
           tookTreatmentW := 1]

depression[tookTreatment==1]

## find average pain time period
# First find the max pain by user
depression[, maxPain := 0]
depression[trackable_type %in% c("Condition", "Symptom"), maxPain := as.numeric(max(trackable_value, na.rm = T)), by = .(user_id)]

# make binary for any time user pain is between max and 75% percentile of pain (maybe 60)
depression[, highPain := 0]
depression[between(x = avgPain,3, 4), highPain := 1]
depression[, lowPain := 0]
depression[highPain == 0, lowPain := 1]

# find days between high pain
depression[, consecutiveDays := 0]
depression[, consecutiveDays := checkin_date - shift(checkin_date), by = user_id]
depression <- unique(depression[,.(user_id, checkin_date, highPain, lowPain, tookTreatment)])
depression <- depression[order(user_id, checkin_date)]
depression[, counter := 0]
depression[, counter := as.numeric(seq_len(.N)), by = .(user_id, rleid(highPain))]
depression[, counterBetweenFlares := 0]
depression[highPain ==0, counterBetweenFlares := counter]
depression[, counterDuringFlares := 0]
depression[highPain ==1, counterDuringFlares := counter]

# merge data back on
fdSubset <- unique(fd[trackable_type %in% c("Condition", "Symptom"),.(user_id, age, sex, checkin_date, avgPain)])
setkey(fdSubset, user_id, checkin_date)
setkey(depression, user_id, checkin_date)
depression <- depression[fdSubset]
depression <- depression[!is.na(avgPain) & !is.na(counter) & !is.na(counterBetweenFlares) & !is.na(counterDuringFlares) & !is.na(age)]
depression$age <- as.numeric(depression$age)
lmD1 <- lm(data = depression, formula = avgPain ~ tookTreatment)
summary(lmD1)
lmD2 <- lm(data = depression, formula = avgPain ~ tookTreatment + age)
summary(lmD2)

# add variable for winter
depression[, winter := 0]
depression[months(checkin_date) %in% c("December", "January", "February"), winter := 1]

depression[, spring := 0]
depression[months(checkin_date) %in% c("March", "April", "May"), spring := 1]

depression[, summer := 0]
depression[months(checkin_date) %in% c("June", "July", "August"), summer := 1]

depression[, fall := 0]
depression[months(checkin_date) %in% c("September", "October", "November"), fall := 1]

lmD3 <- lm(data = depression, formula = avgPain ~ winter)
summary(lmD3)

lmD4 <- lm(data = depression, formula = avgPain ~ summer)
summary(lmD4)
summary(lmD1)
stop()
write.csv(x = depression,file = "depression2.csv")
# Shiny -------------------------------------------------------------------
# source("loadData.R")
RegData <- data.table(read.csv(file = "depression2.csv"))

ui <- fluidPage(
  headerPanel("Regression Testing"), 
  sidebarPanel(
    p("Select the inputs for the Dependent Variable"),
    selectInput(inputId = "IndVar", label = "Independent Variables", multiple = FALSE, choices = list("age", "sex", "tookTreatmentC", "tookTreatmentW", "counterBetweenFlares", "counterDuringFlares", "winter", "summer", "spring", "fall")),
    p("Select the inputs for the Independent Variable"),
    selectInput(inputId = "DepVar", label = "Dependent Variables", multiple = FALSE, choices = list( "avgPain"))
  ),
  mainPanel(
    verbatimTextOutput(outputId = "RegSum"),
    verbatimTextOutput(outputId = "IndPrint"),
    verbatimTextOutput(outputId = "DepPrint")
    #plotOutput("hist")
  )
)

server <- function(input, output) {
  
  lm1 <- reactive({lm(reformulate(input$IndVar, input$DepVar), data = RegData)})
  
  output$DepPrint <- renderPrint({input$DepVar})
  output$IndPrint <- renderPrint({input$IndVar})
  output$RegSum <- renderPrint({summary(lm1())})
  
}

shinyApp(ui = ui, server = server)