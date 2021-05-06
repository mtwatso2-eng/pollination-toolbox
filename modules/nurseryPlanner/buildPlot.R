buildNursery <- function(input){
  
  parents <- input$whatParents
  nurseryDimensions <- c(input$plotRows, input$plotColumns)
  
  bestNursery <- sample(
    parents,
    nurseryDimensions[1] * nurseryDimensions[2],
    replace = T
  ) %>%
   as.integer()
  
  return(bestNursery)
  
}
