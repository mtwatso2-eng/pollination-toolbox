printLabels <- list(

  "ui" = fluidPage(
    selectInput("printingGroup", "What group do you want printed?", choices = unique(cache()$master$crosses[,"Group"])),
    DT::dataTableOutput("printableTable")
  ),

  "server" = function(input, output, session){
    
    output$printableTable <- renderDT(
      getPrintableTable(input, cache()),
      server = FALSE,
      extensions = "Buttons",
      options = list(
        dom = "Bfrtlip",
        lengthMenu = list(c(10, 25, 50, 100, -1), 
          c("10", "25", "50","100", "All")),
        buttons = list(
          list(extend = "csv", filename = paste(input$printingGroup)),
          list(extend = "excel", filename = paste(input$printingGroup))
        ), text = "Download"
      ),
      rownames = FALSE
    )
    
  }
  
)
