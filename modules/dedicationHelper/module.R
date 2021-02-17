dedicationHelper <- list(
  
  "ui" = fluidPage(
    DT::dataTableOutput("dedicationHelperTable")
  ),
  
  "server" = function(input, output, session){
    
    output$dedicationHelperTable <- renderDT(dataTablewithTooltips(
      tabulateDedicationHelper(input, cache()),
      rownames = FALSE,
      extensions = "Buttons",
      options = list(
        lengthMenu = list(c(10, 25, 50, 100, -1), 
          c("10", "25", "50","100", "All")),
        dom = "Bfrtlip",
        buttons = list(
          list(
            extend = "csv",
            filename = paste(input$dedicationHelperGroup,
            "Summary",
            exportOptions = list(modifier = list(page = "all")))
          )
        )
      )
    ))
  
  }
  
)
