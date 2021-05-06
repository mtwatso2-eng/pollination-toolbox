nurseryPlanner <- list(
  
  "ui" = fluidPage(
    bsCollapsePanel(title = "Advanced Options",
      flowLayout(
        numericInput("optimizationTime", "Seconds per optimization batch", value = 1)
      )
    ),
    pickerInput("whatParents",
      "What parents (GH codes)?",
      choices = sort(as.numeric(union(pa$FemaleCode, pa$MaleCode))),
      multiple = T,
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
    
    updateTable <<- T
    updateTableAgain <<- T
    observeEvent(input$appearanceTable, {
      if(!updateTableAgain){
        updateTable <<- T
        updateTableAgain <<- T
        return()
      }
      updateTableAgain <<- F
      if(updateTable){
        output$appearanceTable <- renderRHandsontable({
          thisNursery <- unlist(isolate(input$appearanceTable$data))
          return(
            thisNursery %>%
              matrix(., nrow = isolate(input$plotRows), byrow = T) %>%
              rhandsontable(., colHeaders = 1:ncol(.), rowHeaders = 1:nrow(.),
                nurserySeedScores = thisNursery %>% nurserySeedScores(., c(isolate(input$plotRows), isolate(input$plotColumns)), crosses())
              ) %>%
              hot_heatmap(renderer = NonValueBasedHeatmapRenderer())
          )
        })
      }
    })
    
    output$appearanceTable <- renderRHandsontable((function(){
      
      input$randomizeNursery
      input$optimizeNursery
      
      if(input$randomizeNursery[1] > randomizeCount){
        randomizeCount <<- input$randomizeNursery[1]
        thisNursery <- buildNursery(isolate(input))
        return(
        thisNursery %>%
          matrix(., nrow = isolate(input$plotRows), byrow = T) %>%
          rhandsontable(., colHeaders = 1:ncol(.), rowHeaders = 1:nrow(.),
            nurserySeedScores = thisNursery %>% nurserySeedScores(., c(isolate(input$plotRows), isolate(input$plotColumns)), crosses())
          ) %>%
          hot_heatmap(renderer = NonValueBasedHeatmapRenderer())
      )}

      if(input$optimizeNursery[1] > optimizeCount){
        optimizeCount <<- input$optimizeNursery[1]
        thisNursery <- optimizeNursery(unlist(isolate(input$appearanceTable$data)), input, crosses())
        return(
         thisNursery %>%
          matrix(., nrow = isolate(input$plotRows), byrow = T) %>%
          rhandsontable(., colHeaders = 1:ncol(.), rowHeaders = 1:nrow(.), 
            nurserySeedScores = thisNursery %>% nurserySeedScores(., c(isolate(input$plotRows), isolate(input$plotColumns)), crosses())
          ) %>%
          hot_heatmap(renderer = NonValueBasedHeatmapRenderer())
        )}
      
    })())
      
    output$nurserySeedScore <- renderText(writeNurserySeedScore(input, crosses()))

  }
  
)

crosses <- function(){
  
  cache <- cache()
  cleanedSeed <- cache$readOnly$cleanedSeed
  
  lastCapsuleCollectionDate <- (
      cache$master$consolidated$CapsuleCollectionTimestamp %>%
      na.omit %>%
      max %>% 
      as.Date
    )
  
  aggregated <- cache$master$consolidated %>%
    group_by(Cross) %>%
    mutate(
      across(where(is.numeric), function(x){mean(x, na.rm = TRUE)}),
      IsFortyPlusDaysOld = (Date <= lastCapsuleCollectionDate - 40),
      ThisWeek = sum(Date >= Sys.Date() - 7, na.rm = TRUE),
      Old = sum(IsFortyPlusDaysOld, na.rm = TRUE),
      Total = n(),
      Dedicated = sum(DedicationStatus == "Dedicated", na.rm = TRUE),
      Compatibilities = sum(DedicationStatus == "Compatibility", na.rm = TRUE),
      CapsuleCollections = sum((!is.na(CapsuleCollectionTimestamp))),
      SuccessRate = ifelse(Old > 0, round(CapsuleCollections / Old, 2), NA)
    ) %>%
    slice(1) %>%
    full_join(cleanedSeed, by = c("Parents" = "Cross")) %>%
    mutate(
      across(SeedPerCapsule:last_col(), function(x){round(weighted.mean(x, CapsuleCollections, na.rm = TRUE), 2)}),
      across(where(is.integer), function(x){round(sum(x, na.rm = TRUE))}),
      SuccessRate = ifelse(CapsuleCollections <= Old , round(CapsuleCollections / Old, 2), NA)
    ) %>% 
    slice(1) %>% 
    ungroup() %>%
    arrange(desc(Total)) %>%
    select(Cross, Total:last_col()) %>%
    filter(!is.na(Cross)) %>%
    merge(., cache$master$crosses, by = "Cross", all.x = TRUE, sort = FALSE) %>%
    replace_na(list(SeedPerCapsule = 1.3)) %>%
    rowwise() %>%
    mutate(PlantCount = sum(cache$master$dedications$Cross == Cross, na.rm = T)) %>%
    mutate(PlantCount = ifelse(PlantCount, PlantCount, 1)) %>%
    group_by(FemaleCode) %>%
    mutate(
      SeasonFlowerCountEstimate = max(Total / PlantCount)
    ) %>%
    ungroup() %>%
    mutate(
      SuccessRate = SuccessRate * (Compatibilities > 4),
      SeedPerPol = SeedPerCapsule * SuccessRate,
      SeedPerPlantPerSeason = SeasonFlowerCountEstimate * SeedPerPol
    ) %>%
    select(Cross, SeedPerPlantPerSeason)
  return(aggregated)
  
}

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
