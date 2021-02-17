ratesByInstances <- list(
  
  "ui" = fluidPage(
    flowLayout(
      selectInput("ratesByInstancesVariable", "Variable", c("FemaleCode", "MaleCode")),
      selectInput("ratesByInstancesFilter", "Search", statMode(cache()$master$consolidated[,"FemaleCode"]))
    ),
    plotlyOutput("successRate")
  ),
  
  "server" = function(input, output, session){
    
    output$successRate <- renderPlotly(aggregateRatesByInstances(input, cache()))
    
    observe({
      updateSelectInput(
        session, 
        "ratesByInstancesFilter",
        choices = sort(as.numeric(unlist(unique(cache()$master$consolidated[,input$ratesByInstancesVariable])))),
        selected = input$ratesByInstancesFilter
      )
    })
    
  }
  
)