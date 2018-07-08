library(shiny)
library(xtable)

data <- read.csv("disabilitydatabase_2014.csv", header=T, sep = ",", dec=".")
locations = unique(c(paste(data$Country)))


ui <- fluidPage(
  titlePanel("Disability Database"),
  br(),
  sidebarLayout(
    sidebarPanel(
      
      selectInput(inputId = "country", "Select Country", choices=locations, selected = "Brazil"),
      br(),
      plotOutput("plotrec"),
      br(),
      plotOutput("plottran"),
      br(),
      p("Developed by",
        a("Development Pathways", 
          href = "http://www.developmentpathways.co.uk"), 
        "using Shiny by RStudio. Values in the bar plots are random and do not necessarily match that in text.")
      ),
  mainPanel(
    h2(textOutput(outputId = "name")),
      h4("Selection Mechanism"),
        p(textOutput(outputId = "targeting")),
      h4("Number of recipients"),
        p(textOutput(outputId = "recipients")),
      h4("Value of monthly transfer (USD)"),
        p(textOutput(outputId = "trans_usd")),
      h4("Value of monthly transfer (local currency)"),
        p(textOutput(outputId = "trans_lcu")),    
      h4("Value of transfer annually (as % of GDP per capita)"),
        p(textOutput(outputId = "trans_gdp")),
      h4("Budget of Scheme"),
        p(textOutput(outputId = "budget")),
      h4("Cost of scheme (as% of GDP)"),
        p(textOutput(outputId = "cost")),
      h4("Mechanism for identifying Disability"),
        p(textOutput(outputId = "mechanism")),
      h5("Comments"),
        p(textOutput(outputId = "comments")),
      h5("Sources"),
        p(textOutput(outputId = "sources"))
  )
)
)
server <- function(input, output) {
  
  #### Scenario A
  #### Children
  schemename <- reactive({
    paste(data[data$Country==input$country, 2])
  })
  schemetargeting <- reactive({
    paste(data[data$Country==input$country, 3])
  })
  schemerecipients <- reactive({
    paste(data[data$Country==input$country, 4])
  })
  schemetrans_usd <- reactive({
    paste(data[data$Country==input$country, 5])
  })
  schemetrans_lcu <- reactive({
    paste(data[data$Country==input$country, 6])
  })
  schemetrans_gdp <- reactive({
    paste(data[data$Country==input$country, 7])
  })
  schemebudget <- reactive({
    paste(data[data$Country==input$country, 8])
  })
  schemecost <- reactive({
    paste(data[data$Country==input$country, 9])
  })
  schememechanism <- reactive({
    paste(data[data$Country==input$country, 10])
  })
  schemecomments <- reactive({
    paste(data[data$Country==input$country, 11])
  })
  schemesources <- reactive({
    paste(data[data$Country==input$country, 12])
  })
  
  plotrecipient <- reactive({
    c(10, 13, 3, 8, data[data$Country==input$country, 13])
  })

  plottransfers <- reactive({
    c(0.8, 1.2, 0.3, 0.8, data[data$Country==input$country, 14])
  })

  argnames <- reactive({
    c("World", "LAC", "AFR", "ASIA", abbreviate(input$country, minlength = 6, strict = T))
  })
  
  # Show the values in an HTML table ----
  output$name <- renderText({
    schemename()
  })
  output$targeting <- renderText({
    schemetargeting()
  })
  output$recipients <- renderText({
    schemerecipients()
  })
  output$trans_usd <- renderText({
    schemetrans_usd()
  })
  output$trans_lcu <- renderText({
    schemetrans_lcu()
  })
  output$trans_gdp <- renderText({
    schemetrans_gdp()
  })
  output$budget <- renderText({
    schemebudget()
  })
  output$cost <- renderText({
    schemecost()
  })
  output$mechanism <- renderText({
    schememechanism()
  })
  output$comments <- renderText({
    schemecomments()
  })
  output$sources <- renderText({
    schemesources()
  })
  
  output$plotrec <- renderPlot({
    
    # Render a barplot
    barplot(plottransfers(), names.arg = argnames(),
            main="Recipients Covered",
            ylab="Percentage of persons with disabilities covered",
            xlab="Region / country")
  })
  
  output$plottran <- renderPlot({
    
    # Render a barplot
    barplot(plottransfers(), names.arg = argnames(),
            main="Value of Transfers",
            ylab="Transfer value as % of GDP per capita",
            xlab="Region / country")
  })
  
}

shinyApp(ui = ui, server = server)
