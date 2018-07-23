library(shiny)
library(xtable)

data <- read.csv("test_DBD.csv", header=T, sep = ",", dec=".")
locations = unique(c(paste(data$country)))

ui <- fluidPage(
  titlePanel("Disability Benefits Database"),
  br(),
  sidebarLayout(
    sidebarPanel(
      
      selectInput(inputId = "country", "Select Country", choices=locations, selected = textOutput("random")),
      uiOutput("schemes"),
      br(),
      # plotOutput("plotrec"),
      # br(),
      h4("Transfer value of programme compared to regional averages", style="text-align:center; color:rgb(239, 93, 59)"),
      plotOutput("plottran"),
      br(),
      p("Developed by",
        a("Development Pathways", 
          href = "http://www.developmentpathways.co.uk"), 
        "using Shiny by RStudio.", style="font-size:75%;")
    ),
    mainPanel(
      h2(textOutput(outputId = "scheme_name"), style="color:rgb(239, 93, 59)"),
      h4("Selection Criteria", style="color:rgb(51,79,127)"),
      p(textOutput(outputId = "targeting")),
      h4("Poverty-targeted?", style="color:rgb(51,79,127)"),
      p(textOutput(outputId = "meanstest")),
      h4("Number of recipients", style="color:rgb(51,79,127)"),
      p(textOutput(outputId = "num_rec")),
      h4("Value of monthly transfer (USD Purchasing Power Parity or PPP)", style="color:rgb(51,79,127)"),
      p(textOutput(outputId = "trans_ppp")), 
      h4("Value of monthly transfer (local currency)", style="color:rgb(51,79,127)"),
      p(textOutput(outputId = "trans_lcu")),    
      h4("Value of transfer annually (as % of GDP per capita)", style="color:rgb(51,79,127)"),
      p(textOutput(outputId = "trans_gdp")),
      h4("Expenditure on Scheme (Local Currency)", style="color:rgb(51,79,127)"),
      p(textOutput(outputId = "exp_lcu")),
      h4("Expenditure on scheme: % of GDP", style="color:rgb(51,79,127)"),
      p(textOutput(outputId = "exp_gdp")),
      h4("Further information", style="color:rgb(51,79,127)"),
      p(textOutput(outputId = "info")),
      h5("Comments", style="color:rgb(51,79,127)"),
      p(textOutput(outputId = "comments")),
      br(),
      p("Note: On scheme expenditure figures, where these were not available (indicated by an *) these were obtained by multiplying the number of recipients by the minimum annual transfer value.", style="font-size:75%;")
    )
  )
)
server <- function(input, output) {
  
  
  
  output$schemes <- renderUI({
    selectInput("schemename", "Select a scheme:", choices = as.character(data[data$country==input$country, 6]))
  })
  
  random_fun <- reactive({
    paste(sample(locations,1))
    })
  
  schemename <- reactive({
    paste(data[data$country==input$country & data$scheme_name==input$schemename, 6])
  })
  schemetargeting <- reactive({
    paste(data[data$country==input$country & data$scheme_name==input$schemename, 7])
  })
  schememeanstest <- reactive({
    paste(data[data$country==input$country & data$scheme_name==input$schemename, 8])
  })
  schemerecipients <- reactive({
    paste(data[data$country==input$country & data$scheme_name==input$schemename, 9])
  })
  schemetrans_ppp <- reactive({
    paste(data[data$country==input$country & data$scheme_name==input$schemename, 10])
  })
  schemetrans_lcu <- reactive({
    paste(data[data$country==input$country & data$scheme_name==input$schemename, 11])
  })
  schemetrans_gdp <- reactive({
    paste(data[data$country==input$country & data$scheme_name==input$schemename, 12])
  })
  schemebudget <- reactive({
    paste(data[data$country==input$country & data$scheme_name==input$schemename, 13])
  })
  schemecost <- reactive({
    paste(data[data$country==input$country & data$scheme_name==input$schemename, 14])
  })
  schememechanism <- reactive({
    paste(data[data$country==input$country & data$scheme_name==input$schemename, 15])
  })
  schemecomments <- reactive({
    paste(data[data$country==input$country & data$scheme_name==input$schemename, 16])
  })
  
  # plotrecipient <- reactive({
  #   c(10, 13, 3, 8, data[data$country==input$country & data$scheme_name==input$schemename, 17])
  # })
  
  plottransfers <- reactive({
    c(data[data$country==input$country & data$scheme_name==input$schemename, 24], 
      data[data$country==input$country & data$scheme_name==input$schemename, 25], 
      data[data$country==input$country & data$scheme_name==input$schemename, 26], 
      data[data$country==input$country & data$scheme_name==input$schemename, 27], 
      data[data$country==input$country & data$scheme_name==input$schemename, 18])
  })
  
  argnames <- reactive({
    c("Americas", "Asia", "Oceania", "Africa", "Prog.")
  }) 
  
  colorpalette <- c( "#9CC4E7" , "#6F30A1", "#FAB41F", "#298B9C", "#EF5D3B")
  
  # Show the values in an HTML table ----

  output$scheme_name <- renderText({
    schemename()
  })
  output$targeting <- renderText({
    schemetargeting()
  })
  output$meanstest <- renderText({
    schememeanstest()
  })
  output$num_rec <- renderText({
    schemerecipients()
  })
  output$trans_ppp <- renderText({
    schemetrans_ppp()
  })
  output$trans_lcu <- renderText({
    schemetrans_lcu()
  })
  output$trans_gdp <- renderText({
    schemetrans_gdp()
  })
  output$exp_lcu <- renderText({
    schemebudget()
  })
  output$exp_gdp <- renderText({
    schemecost()
  })
  output$info <- renderText({
    schememechanism()
  })
  output$comments <- renderText({
    schemecomments()
  })
  
  output$random <- renderText({
    random_fun()
    })
  
  # output$plotrec <- renderPlot({
  #  
  #  # Render a barplot
  #  barplot(plotrecipient(), names.arg = argnames(),
  #          main="Recipients Covered",
  #          ylab="Percentage of persons with disabilities covered (%)",
  #          xlab="Region / country")
  #})
  
  output$plottran <- renderPlot({
    
    # Render a barplot
    par(bg=rgb(245,245,245, max = 255))
    barplot(plottransfers(), names.arg = argnames(),
            #main="Value of Transfers",
            col = colorpalette,
            ylab="Transfer value as % of GDP per capita",
            #xlab="Region / country",
            las = 2
           )
  })
  
}

shinyApp(ui = ui, server = server)
