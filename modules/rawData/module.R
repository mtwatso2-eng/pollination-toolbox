rawData <- list(
  
  "ui" = fluidPage(
    flowLayout(
      selectInput(
        "whichRawData",
        label = "Select data",
        choices = list(
          "Consolidated" = "cache()$master$consolidated",
          "Pollinations" = "cache()$readOnly$pollinations",
          "Failed Pollinations" = "cache()$readOnly$failedPollinations",
          "Capsule Collections" = "cache()$readOnly$capsuleCollections",
          "Cleaned Seed" = "cache()$readOnly$cleanedSeed",
          "Weather" = "cache()$readOnly$weather"
        )
      ),
      textAreaInput("rawDataFunction", label = "R pipe for analysis (optional)", width = 400, height = 34)
    ),
    DT::dataTableOutput("rawDataTable")
  ),
  
  "server" = function(input, output, session){
    
    output$rawDataTable <- renderDT(dataTablewithTooltips(
      # note: this line allows code injection, which may compromise "hidden" data...
      # ...(allowed because Pollination Toolbox currently has no "hidden" data)
      eval(parse(text = paste(input$whichRawData, input$rawDataFunction))),
      rownames = FALSE,
      extensions = "Buttons",
      options = list(
        lengthMenu = list(c(10, 25, 50, 100, -1), 
          c("10", "25", "50","100", "All")),
        dom = "Bfrtlip",
        buttons = list(
          list(
            extend = "csv",
            filename = paste(input$whichRawData,
              "Summary",
              exportOptions = list(modifier = list(page = "all")))
          )
        )
      )
    ))
     
  }
  
)
