buildNursery <- function(input){
  
  whatParents <- input$whatParents
  
  nurseryDimensions <- c(input$plotRows, input$plotColumns)
  
  bestNursery <- sample(
    whatParents,
    nurseryDimensions[1] * nurseryDimensions[2],
    replace = T
  )
  
  return(bestNursery)
  
}
