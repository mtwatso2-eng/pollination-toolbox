seasonSuccessRate <- list(
  
  "ui" = fluidPage(
    selectInput("lineGroup", label = "Group lines by", choices = c("Program", "Group", "Pollinator", "Don't group")),
    plotlyOutput("seasonSuccessRate")
  ),
  
  "server" = function(input, output, session){

    output$seasonSuccessRate <- renderPlotly(plotSeasonSuccessRate(input, cache()))
    
  }
  
)
