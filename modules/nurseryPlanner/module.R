nurseryPlanner <- list(
  
  "ui" = fluidPage(
    bsCollapsePanel(title = "Advanced Options",
      flowLayout(
        numericInput("optimizationTime", "Time per optimization batch", value = 1)
      )
    ),
    pickerInput("whatParents",
      "What parents (GH codes)?",
      choices = sort(unique(read.csv("crosses.csv", stringsAsFactors = F)$FemaleCode)),
      multiple = T,
      selected = c(40, 41, 42, 44, 47, 50, 51, 58)
    ),
    flowLayout(
      numericInput("plotRows", "How many rows?", 4),
      numericInput("plotColumns", "How many plants per row?", 4)
    ),
    flowLayout(
      actionButton("randomizeNursery", "Randomize Nursery"),
      actionButton("optimizeNursery", "Optimize Nursery")
    ),
    br(),
    rHandsontableOutput("appearanceTable"),
    br(),
    textOutput("nurserySeedScore")
  ),
  
  "server" = function(input, output, session){
    
    randomizeCount <<- 0
    optimizeCount <<- 0
    
    output$appearanceTable <- renderRHandsontable((function(){
      
      input$randomizeNursery
      input$optimizeNursery

      if(input$randomizeNursery[1] > randomizeCount){
        randomizeCount <<- input$randomizeNursery[1]
        thisNursery <- buildNursery(input)
        return(
        thisNursery %>%
          matrix(., nrow = isolate(input$plotRows), byrow = T) %>%
          rhandsontable(., colHeaders = 1:ncol(.), rowHeaders = 1:nrow(.),
            nurserySeedScores = thisNursery %>% nurserySeedScores(., c(input$plotRows, input$plotColumns), crosses)
          ) %>%
          hot_heatmap(., renderer = NonValueBasedHeatmapRenderer())
      )}

      if(input$optimizeNursery[1]){
        optimizeCount <<- input$optimizeNursery[1]
        thisNursery <- optimizeNursery(unlist(isolate(input$appearanceTable$data)), input)
        return(
         thisNursery %>%
          matrix(., nrow = isolate(input$plotRows), byrow = T) %>%
          rhandsontable(., colHeaders = 1:ncol(.), rowHeaders = 1:nrow(.), 
            nurserySeedScores = thisNursery %>% nurserySeedScores(., c(input$plotRows, input$plotColumns), crosses)
          ) %>%
          hot_heatmap(
            renderer = NonValueBasedHeatmapRenderer()
          )
        )}
      
    })())
      
    output$nurserySeedScore <- renderText(writeNurserySeedScore(input))

  }
  
)

nurserySeedScore <- function(nursery, nurseryDimensions, crosses){
  seedScores <- seq_along(nursery) %>% sapply(function(x){
    female <- nursery[x]
    males <- c(
      ifelse(x %% nurseryDimensions[1] != 1, nursery[x - 1], NA),
      ifelse(x %% nurseryDimensions[1] != 0, nursery[x + 1], NA),
      ifelse(x > nurseryDimensions[1], nursery[x - nurseryDimensions[1]], NA),
      ifelse(x < length(nursery) -  nurseryDimensions[1], nursery[x + nurseryDimensions[1]], NA)
    )
    males <- males[!is.na(males)]
    return(mean(map_dbl(males, function(y){
      thisCross <- unlist(crosses[crosses$Cross == paste0(female, "x", y), "SeedPerPlantPerSeason"])
      return(ifelse(length(thisCross) > 0, thisCross, 0))
    }), na.rm = T))
  })
  return(sum(seedScores, na.rm = T))
}

nurserySeedScores <- function(nursery, nurseryDimensions, crosses){
  seedScores <- seq_along(nursery) %>% sapply(function(x){
    female <- nursery[x]
    males <- c(
      ifelse(x %% nurseryDimensions[1] != 1, nursery[x - 1], NA),
      ifelse(x %% nurseryDimensions[1] != 0, nursery[x + 1], NA),
      ifelse(x > nurseryDimensions[1], nursery[x - nurseryDimensions[1]], NA),
      ifelse(x < length(nursery) -  nurseryDimensions[1], nursery[x + nurseryDimensions[1]], NA)
    )
    males <- males[!is.na(males)]
    return(mean(map_dbl(males, function(y){
      thisCross <- unlist(crosses[crosses$Cross == paste0(female, "x", y), "SeedPerPlantPerSeason"])
      return(ifelse(length(thisCross) > 0, thisCross, 0))
    }), na.rm = T))
  })
  return(seedScores)
}
