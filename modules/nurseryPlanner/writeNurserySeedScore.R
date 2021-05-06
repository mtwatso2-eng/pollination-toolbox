writeNurserySeedScore <- function(input){
  
  parents <- input$whatParents
  thisNursery <- input$appearanceTable$data %>% unlist()
        
  nurseryDimensions <- c(input$plotRows, input$plotColumns)
  thisNurserySeedScore <- try(silent = T, thisNursery %>%
    nurserySeedScore(., nurseryDimensions, crosses) %>%
    unlist() %>%
    round()
  )
  thisNurseryParentCounts <- paste0(unlist(map(map(parents, ~grep(.x, thisNursery)), length)), " ", parents, "'s")
    
  if(class(thisNurserySeedScore) != "try-error"){
    return(paste0("Nursery Seed Score: ", thisNurserySeedScore, ". Parent counts: ", paste(thisNurseryParentCounts, collapse = ", ")))
  }
  else
    return("")
  
}

