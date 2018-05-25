library(shiny)
library(xtable)

UNpop.estimates <- read.csv("UNDESA_2017PopEst.csv", header=T, sep = ",", dec=".")
locations = unique(c(paste(UNpop.estimates$location)))

ui <- fluidPage(
  titlePanel("Inclusive System"),
  mainPanel(
    selectInput(inputId = "country", "Select region/country", choices=locations, selected = "Brazil")),
  # Output: Tabset w/ plot, summary, and table ----
  tabsetPanel(type = "tabs",
              tabPanel("Scenario A",
                       sidebarPanel(titlePanel("Child Benefit"),
                                    sliderInput(inputId="childageA", "Age limits:",
                                                min = 0, max = 18, value =c(0,17), dragRange = T
                                    ),
                                    sliderInput(inputId="childcoverageA", "Coverage (%):",
                                                min = 0, max = 100, value =50, dragRange = T, post="%"
                                    ),
                                    numericInput(inputId="childtransferA", "Monthly transfer value ($):",
                                                 min = 0, max = 1000, value =150
                                    ),
                                    tableOutput("childvaluesA"),
                                    width = 4
                                    
                       ),
                       sidebarPanel(titlePanel("Disability Benefit"),
                                    sliderInput(inputId="disabageA", "Age limits:",
                                                min = 0, max = 100, value =c(18,59), dragRange = T
                                    ),
                                    sliderInput(inputId="disabcoverageA", "Coverage (%):",
                                                min = 0, max = 100, value =50, dragRange = T, post="%"
                                    ),
                                    numericInput(inputId="disabtransferA", "Monthly transfer value ($):",
                                                 min = 0, max = 1000, value =250
                                    ),
                                    tableOutput("disabvaluesA"),
                                    width = 4
                       ),
                       sidebarPanel(titlePanel("Old Age Benefit"),
                                    sliderInput(inputId="oldageA", "Age limits:",
                                                min = 50, max = 100, value =c(60,100), dragRange = T
                                    ),
                                    sliderInput(inputId="oldcoverageA", "Coverage (%):",
                                                min = 0, max = 100, value =50, dragRange = T, post="%"
                                    ),
                                    numericInput(inputId="oldtransferA", "Monthly transfer value ($):",
                                                 min = 0, max = 1000, value =250
                                    ),
                                    tableOutput("oldvaluesA"),
                                    width = 4
                       )
              ),
              tabPanel("Scenario B",
                       sidebarPanel(titlePanel("Child Benefit"),
                                    sliderInput(inputId="childageB", "Age limits:",
                                                min = 0, max = 18, value =c(0,17), dragRange = T
                                    ),
                                    sliderInput(inputId="childcoverageB", "Coverage (%):",
                                                min = 0, max = 100, value =50, dragRange = T, post="%"
                                    ),
                                    numericInput(inputId="childtransferB", "Monthly transfer value ($):",
                                                 min = 0, max = 1000, value =150
                                    ),
                                    tableOutput("childvaluesB"),
                                    width = 4
                       ),
                       sidebarPanel(titlePanel("Disability Benefit"),
                                    sliderInput(inputId="disabageB", "Age limits:",
                                                min = 0, max = 100, value =c(18,59), dragRange = T
                                    ),
                                    sliderInput(inputId="disabcoverageB", "Coverage (%):",
                                                min = 0, max = 100, value =50, dragRange = T, post="%"
                                    ),
                                    numericInput(inputId="disabtransferB", "Monthly transfer value ($):",
                                                 min = 0, max = 1000, value =250
                                    ),
                                    tableOutput("disabvaluesB"),
                                    width = 4
                       ),
                       sidebarPanel(titlePanel("Old Age Benefit"),
                                    sliderInput(inputId="oldageB", "Age limits:",
                                                min = 50, max = 100, value =c(60,100), dragRange = T
                                    ),
                                    sliderInput(inputId="oldcoverageB", "Coverage (%):",
                                                min = 0, max = 100, value =50, dragRange = T, post="%"
                                    ),
                                    numericInput(inputId="oldtransferB", "Monthly transfer value ($):",
                                                 min = 0, max = 1000, value =250
                                    ),
                                    tableOutput("oldvaluesB"),
                                    width = 4
                       )
              )
  )
)

server <- function(input, output) {
  
  #### Scenario A
  #### Children
  sliderChildValuesA <- reactive({
    agemin <- input$childageA[1] + 3
    agemax <- input$childageA[2] + 3
    if (agemin==agemax) {
      data.frame(
        Year = c("2018", "2030"),
        Recipients= as.character(format(UNpop.estimates[(UNpop.estimates$year == 2018 & UNpop.estimates$location==input$country) | (UNpop.estimates$year == 2030 & UNpop.estimates$location==input$country), agemin]*10*input$childcoverageA
                                        ,big.mark=",")),
        Cost = as.character(format(rowSums(UNpop.estimates[(UNpop.estimates$year == 2018 & UNpop.estimates$location==input$country) | (UNpop.estimates$year == 2030 & UNpop.estimates$location==input$country), agemin], na.rm=TRUE)*10*input$childcoverageA*input$childtransferA
                                   , digits=6 ,big.mark=",")),       
        stringsAsFactors = FALSE)
    }
    else {
      data.frame(
        Year = c("2018", "2030"),
        Recipients= as.character(format(rowSums(UNpop.estimates[(UNpop.estimates$year == 2018 & UNpop.estimates$location==input$country) | (UNpop.estimates$year == 2030 & UNpop.estimates$location==input$country),c(agemin:agemax)], na.rm=TRUE)*10*input$childcoverageA
                                        , nsmall=0 ,big.mark=",")),
        Cost = as.character(format(rowSums(UNpop.estimates[(UNpop.estimates$year == 2018 & UNpop.estimates$location==input$country) | (UNpop.estimates$year == 2030 & UNpop.estimates$location==input$country),c(agemin:agemax)], na.rm=TRUE)*10*input$childcoverageA*input$childtransferA
                                   , digits=6 ,big.mark=",")),
        stringsAsFactors = FALSE)
    }
  })
  
  #### Disability
  sliderDisabValuesA <- reactive({
    agemin <- input$disabageA[1] + 3
    agemax <- input$disabageA[2] + 3
    if (agemin==agemax) {
      data.frame(
        Year = c("2018", "2030"),
        Recipients= as.character(format(UNpop.estimates[(UNpop.estimates$year == 2018 & UNpop.estimates$location==input$country) | (UNpop.estimates$year == 2030 & UNpop.estimates$location==input$country), agemin]*10*input$disabcoverageA*2/100
                                        ,big.mark=",")),
        Cost = as.character(format(rowSums(UNpop.estimates[(UNpop.estimates$year == 2018 & UNpop.estimates$location==input$country) | (UNpop.estimates$year == 2030 & UNpop.estimates$location==input$country), agemin], na.rm=TRUE)*10*input$disabcoverageA*2/100*input$disabtransferA
                                   , digits=6 ,big.mark=",")),       
        stringsAsFactors = FALSE)
    }
    else {
      data.frame(
        Year = c("2018", "2030"),
        Recipients= as.character(format(rowSums(UNpop.estimates[(UNpop.estimates$year == 2018 & UNpop.estimates$location==input$country) | (UNpop.estimates$year == 2030 & UNpop.estimates$location==input$country),c(agemin:agemax)], na.rm=TRUE)*10*input$disabcoverageA*2/100
                                        , nsmall=0 ,big.mark=",")),
        Cost = as.character(format(rowSums(UNpop.estimates[(UNpop.estimates$year == 2018 & UNpop.estimates$location==input$country) | (UNpop.estimates$year == 2030 & UNpop.estimates$location==input$country),c(agemin:agemax)], na.rm=TRUE)*10*input$disabcoverageA*2/100*input$disabtransferA
                                   , digits=6 ,big.mark=",")),
        stringsAsFactors = FALSE)
    }
  })
  
  #### Old age
  sliderOldValuesA <- reactive({
    agemin <- input$oldageA[1] + 3
    agemax <- input$oldageA[2] + 3
    if (agemin==agemax) {
      data.frame(
        Year = c("2018", "2030"),
        Recipients= as.character(format(UNpop.estimates[(UNpop.estimates$year == 2017 & UNpop.estimates$location==input$country) | (UNpop.estimates$year == 2030 & UNpop.estimates$location==input$country), agemin]*10*input$oldcoverageA
                                        ,big.mark=",")),
        Cost = as.character(format(rowSums(UNpop.estimates[(UNpop.estimates$year == 2018 & UNpop.estimates$location==input$country) | (UNpop.estimates$year == 2030 & UNpop.estimates$location==input$country), agemin], na.rm=TRUE)*10*input$oldcoverageA*input$oldtransferA
                                   , digits=6 ,big.mark=",")),       
        stringsAsFactors = FALSE)
    }
    else {
      data.frame(
        Year = c("2018", "2030"),
        Recipients= as.character(format(rowSums(UNpop.estimates[(UNpop.estimates$year == 2017 & UNpop.estimates$location==input$country) | (UNpop.estimates$year == 2030 & UNpop.estimates$location==input$country),c(agemin:agemax)], na.rm=TRUE)*10*input$oldcoverageA
                                        , nsmall=0 ,big.mark=",")),
        Cost = as.character(format(rowSums(UNpop.estimates[(UNpop.estimates$year == 2018 & UNpop.estimates$location==input$country) | (UNpop.estimates$year == 2030 & UNpop.estimates$location==input$country),c(agemin:agemax)], na.rm=TRUE)*10*input$oldcoverageA*2/100*input$oldtransferA
                                   , digits=6 ,big.mark=",")),
        stringsAsFactors = FALSE)
    }
  })
  
  
  #### Scenario B
  #### Children
  sliderChildValuesB <- reactive({
    agemin <- input$childageB[1] + 3
    agemax <- input$childageB[2] + 3
    if (agemin==agemax) {
      data.frame(
        Year = c("2018", "2030"),
        Recipients= as.character(format(UNpop.estimates[(UNpop.estimates$year == 2018 & UNpop.estimates$location==input$country) | (UNpop.estimates$year == 2030 & UNpop.estimates$location==input$country), agemin]*10*input$childcoverageB
                                        ,big.mark=",")),
        Cost = as.character(format(rowSums(UNpop.estimates[(UNpop.estimates$year == 2018 & UNpop.estimates$location==input$country) | (UNpop.estimates$year == 2030 & UNpop.estimates$location==input$country), agemin], na.rm=TRUE)*10*input$childcoverageB*input$childtransferB
                                   , digits=6 ,big.mark=",")),       
        stringsAsFactors = FALSE)
    }
    else {
      data.frame(
        Year = c("2018", "2030"),
        Recipients= as.character(format(rowSums(UNpop.estimates[(UNpop.estimates$year == 2018 & UNpop.estimates$location==input$country) | (UNpop.estimates$year == 2030 & UNpop.estimates$location==input$country),c(agemin:agemax)], na.rm=TRUE)*10*input$childcoverageB
                                        , nsmall=0 ,big.mark=",")),
        Cost = as.character(format(rowSums(UNpop.estimates[(UNpop.estimates$year == 2018 & UNpop.estimates$location==input$country) | (UNpop.estimates$year == 2030 & UNpop.estimates$location==input$country),c(agemin:agemax)], na.rm=TRUE)*10*input$childcoverageB*input$childtransferB
                                   , digits=6 ,big.mark=",")),
        stringsAsFactors = FALSE)
    }
  })
  
  #### Disability
  sliderDisabValuesB <- reactive({
    agemin <- input$disabageB[1] + 3
    agemax <- input$disabageB[2] + 3
    if (agemin==agemax) {
      data.frame(
        Year = c("2018", "2030"),
        Recipients= as.character(format(UNpop.estimates[(UNpop.estimates$year == 2018 & UNpop.estimates$location==input$country) | (UNpop.estimates$year == 2030 & UNpop.estimates$location==input$country), agemin]*10*input$disabcoverageB*2/100
                                        ,big.mark=",")),
        Cost = as.character(format(rowSums(UNpop.estimates[(UNpop.estimates$year == 2018 & UNpop.estimates$location==input$country) | (UNpop.estimates$year == 2030 & UNpop.estimates$location==input$country), agemin], na.rm=TRUE)*10*input$disabcoverageB*2/100*input$disabtransferB
                                   , digits=6 ,big.mark=",")),       
        stringsAsFactors = FALSE)
    }
    else {
      data.frame(
        Year = c("2018", "2030"),
        Recipients= as.character(format(rowSums(UNpop.estimates[(UNpop.estimates$year == 2018 & UNpop.estimates$location==input$country) | (UNpop.estimates$year == 2030 & UNpop.estimates$location==input$country),c(agemin:agemax)], na.rm=TRUE)*10*input$disabcoverageB*2/100
                                        , nsmall=0 ,big.mark=",")),
        Cost = as.character(format(rowSums(UNpop.estimates[(UNpop.estimates$year == 2018 & UNpop.estimates$location==input$country) | (UNpop.estimates$year == 2030 & UNpop.estimates$location==input$country),c(agemin:agemax)], na.rm=TRUE)*10*input$disabcoverageB*2/100*input$disabtransferB
                                   , digits=6 ,big.mark=",")),
        stringsAsFactors = FALSE)
    }
  })
  
  #### Old age
  sliderOldValuesB <- reactive({
    agemin <- input$oldageB[1] + 3
    agemax <- input$oldageB[2] + 3
    if (agemin==agemax) {
      data.frame(
        Year = c("2018", "2030"),
        Recipients= as.character(format(UNpop.estimates[(UNpop.estimates$year == 2017 & UNpop.estimates$location==input$country) | (UNpop.estimates$year == 2030 & UNpop.estimates$location==input$country), agemin]*10*input$oldcoverageB
                                        ,big.mark=",")),
        Cost = as.character(format(rowSums(UNpop.estimates[(UNpop.estimates$year == 2018 & UNpop.estimates$location==input$country) | (UNpop.estimates$year == 2030 & UNpop.estimates$location==input$country), agemin], na.rm=TRUE)*10*input$oldcoverageB*input$oldtransferB
                                   , digits=6 ,big.mark=",")),       
        stringsAsFactors = FALSE)
    }
    else {
      data.frame(
        Year = c("2018", "2030"),
        Recipients= as.character(format(rowSums(UNpop.estimates[(UNpop.estimates$year == 2017 & UNpop.estimates$location==input$country) | (UNpop.estimates$year == 2030 & UNpop.estimates$location==input$country),c(agemin:agemax)], na.rm=TRUE)*10*input$oldcoverageB
                                        , nsmall=0 ,big.mark=",")),
        Cost = as.character(format(rowSums(UNpop.estimates[(UNpop.estimates$year == 2018 & UNpop.estimates$location==input$country) | (UNpop.estimates$year == 2030 & UNpop.estimates$location==input$country),c(agemin:agemax)], na.rm=TRUE)*10*input$oldcoverageB*input$oldtransferB
                                   , digits=6 ,big.mark=",")),
        stringsAsFactors = FALSE)
    }
  })
  
  
  # Show the values in an HTML table ----
  output$childvaluesA <- renderTable({
    sliderChildValuesA()
  })
  output$disabvaluesA <- renderTable({
    sliderDisabValuesA()
  })
  output$oldvaluesA <- renderTable({
    sliderOldValuesA()
  })
  
  # Show the values in an HTML table ----
  output$childvaluesB <- renderTable({
    sliderChildValuesB()
  })
  output$disabvaluesB <- renderTable({
    sliderDisabValuesB()
  })
  output$oldvaluesB <- renderTable({
    sliderOldValuesB()
  })
}

shinyApp(ui = ui, server = server)
