pollinationCounts <- list(
  
  "ui" = fluidPage(
    selectInput("fillGroup", label = "Group by", choices = list("Dedication Status" = "DedicationStatus", "Program", "Group")),
    bsCollapsePanel(title = "Options for Pollination Counts",
      dateRangeInput("dateRange", "Select dates for pollination counts",
        start = as.Date(Sys.time(), tz = "EST"),
        end = as.Date(Sys.time(), tz = "EST"),
        width = "100%"
      ),
      flowLayout(
        checkboxInput("percentBetterThanAverage", "Compare pollination rate to everyone's average?"),
        checkboxInput("percentBetterThanPersonalAverage", "Compare pollination rate to personal average?", value = TRUE)
      )
    ),
    wellPanel(textOutput("untouchedGroups")),
    plotOutput("plotPollinationCounts"),
    DT::dataTableOutput("tabulatePollinationCounts")
  ),
  
  "server" = function(input, output, session){
    
    output$untouchedGroups <- renderText(untouchedGroups(input, cache()))
    
    output$plotPollinationCounts <- renderPlot(plotPollinationCounts(input, cache()))
    
    output$tabulatePollinationCounts <- renderDT(dataTablewithTooltips(
      tabulatePollinationCounts(input, cache()),
      options = list(dom = "tli"),
      rownames = FALSE
    ))

  }
  
)

