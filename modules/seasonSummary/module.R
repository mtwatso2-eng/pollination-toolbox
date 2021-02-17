seasonSummary <- list(
  
  "ui" = fluidPage(
    selectInput(
      inputId = "seasonSummaryGroup", 
      label = "Group by", 
      choices = c("Totals", "Group", "Cross", "Program")
    ),
    bsCollapsePanel(title = "Options for Season Summary",
      flowLayout(
        dateInput("projectionDate", "Capsule projection date", value = Sys.Date(), min = Sys.Date()),
        # numericInput("hourlyWage", "Hourly Wage (for cost-per-pollination estimates)", value = NA),
        textAreaInput("seasonSummaryFunction", "R pipe for analysis (optional)", width = 400, height = 34)
      ),
      checkboxInput("toDoCapsules", "Include to-do capsules?"),
      checkboxInput("costPerSeed", 'Include cost-per-seed estimates?')
    ),
    DT::dataTableOutput("seasonSummaryTable")
  ),
  
  "server" = function(input, output, session){
    
    output$seasonSummaryTable <- renderDT(dataTablewithTooltips(
      # note: this line allows code injection, which may compromise "hidden" data...
      # ...(allowed because Pollination Toolbox currently has no "hidden" data)
      eval(parse(text = paste("tabulateSeasonSummary(input, cache())", input$seasonSummaryFunction))),
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
