nurseryPlanner <- list(
  
  "ui" = fluidPage(
    bsCollapsePanel(title = "Advanced Options",
      flowLayout(
        numericInput("optimizationTime", "Seconds per optimization batch", value = 1)
      )
    ),
    pickerInput("whatParents",
      "What parents?",
      choices = sort(union(cache()$master$crosses$FemaleParent, cache()$master$crosses$MaleParent)),
      multiple = T,
      options = list(`actions-box` = TRUE), 
      selected = NULL
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
          
    validate(need(!is.null(input$whatParents), ""))
      
      input$randomizeNursery
      input$optimizeNursery
      
      if(input$randomizeNursery[1] > randomizeCount){
        randomizeCount <<- input$randomizeNursery[1]
        thisNursery <- buildNursery(isolate(input))
        return(
        thisNursery %>%
          matrix(., nrow = isolate(input$plotRows), byrow = T) %>%
          rhandsontable(., colHeaders = 1:ncol(.), rowHeaders = 1:nrow(.),
            nurserySeedScores = thisNursery %>% nurserySeedScores(., c(isolate(input$plotRows), isolate(input$plotColumns)), crosses)
          ) %>%
          hot_heatmap(renderer = NonValueBasedHeatmapRenderer())
      )}

      if(input$optimizeNursery[1] > optimizeCount){
        optimizeCount <<- input$optimizeNursery[1]
        thisNursery <- optimizeNursery(unlist(isolate(input$appearanceTable$data)), input, crosses)
        return(
         thisNursery %>%
          matrix(., nrow = isolate(input$plotRows), byrow = T) %>%
          rhandsontable(., colHeaders = 1:ncol(.), rowHeaders = 1:nrow(.), 
            nurserySeedScores = thisNursery %>% nurserySeedScores(., c(isolate(input$plotRows), isolate(input$plotColumns)), crosses)
          ) %>%
          hot_heatmap(renderer = NonValueBasedHeatmapRenderer())
        )}
      
    })())
      
    output$nurserySeedScore <- renderText(writeNurserySeedScore(input, crosses))

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
      thisCross <- unlist(crosses[crosses$Parents == paste(female, "x", y), "SeedPerPlantPerSeason"])
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
      thisCross <- unlist(crosses[crosses$Parents == paste(female, "x", y), "SeedPerPlantPerSeason"])
      return(ifelse(length(thisCross) > 0, thisCross, 0))
    }), na.rm = T))
  })
  return(seedScores)
}

NonValueBasedHeatmapRenderer <- function(color_scale = c("#ED6D47", "#17F556")){
  renderer <- gsub("\n", "", 
  "
    function (instance, td, row, col, prop, value, cellProperties) {
      Handsontable.renderers.TextRenderer.apply(this, arguments);
      heatmapScale  = chroma.scale(['%s1', '%s2']);
      if (instance.heatmap[col]) {
        if (instance.params){
          collength = instance.params.data.length;
          rowlength = instance.params.data.flat().length / collength;
          position = (row * rowlength) + col;
          nurserySeedScores = instance.params.nurserySeedScores;
          mn = Math.min(...nurserySeedScores);
          mx = Math.max(...nurserySeedScores);
          pt = ((nurserySeedScores[position] - mn) / (mx - mn));
          td.style.backgroundColor = heatmapScale(pt).hex();
        }
      }
    }
  ")
  # pt = (parseInt(value, 10) - mn) / (mx - mn);
  renderer <- gsub("%s1", color_scale[1], renderer)
  renderer <- gsub("%s2", color_scale[2], renderer)
  return(renderer)
}
