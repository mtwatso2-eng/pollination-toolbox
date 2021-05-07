writeNurserySeedScore <- function(input, crosses){
  
  validate(need(!is.null(input$appearanceTable$data), ""))
  
  whatParents <- input$whatParents
    
  thisNursery <- input$appearanceTable$data %>% unlist()
        
  nurseryDimensions <- c(input$plotRows, input$plotColumns)
  thisNurserySeedScore <- try(silent = T, thisNursery %>%
    nurserySeedScore(., nurseryDimensions, crosses) %>%
    unlist() %>%
    round()
  )
  thisNurseryParentCounts <- paste0(unlist(map(map(whatParents, ~grep(.x, thisNursery)), length)), " ", whatParents, "'s")
    
  if(class(thisNurserySeedScore) != "try-error"){
    return(paste0("Nursery Seed Score: ", thisNurserySeedScore, ". Parent counts: ", paste(thisNurseryParentCounts, collapse = ", ")))
  }
  else
    return("")
  
}

