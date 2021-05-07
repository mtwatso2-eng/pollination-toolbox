optimizeNursery <- function(nursery, input, crosses){
whatParents <- input$whatParents
nurseryDimensions <- c(input$plotRows, input$plotColumns)
bestNursery <- nursery[]
bestSuccess <- nurserySeedScore(bestNursery, nurseryDimensions, crosses)
thisNursery <- bestNursery

startTime <- Sys.time()
endTime <- startTime + isolate(input$optimizationTime)

# the genetic algorithm
while(Sys.time() < endTime){
  if(round(runif(1, 0, 1))){
      # swap clone with clone from thisNursery, or...
      swapSpots <- sample(1:length(thisNursery), 2)
      thisNursery %<>% replace(swapSpots, .[rev(swapSpots)])
  } else{
      # ...swap clone with new clone
      thisNursery[sample(1:length(thisNursery), 1)] <- sample(whatParents, 1)
  }
  
  if(nurserySeedScore(thisNursery, nurseryDimensions, crosses) >= (bestSuccess * 1)){
    bestNursery <- thisNursery
    bestSuccess <- nurserySeedScore(bestNursery, nurseryDimensions, crosses)
  }
    
    

}

# print(bestNursery %>% matrix(., nrow = nurseryDimensions[1]), byrow = T)
# print(paste0(bestNursery, ",", collapse = ""))
# print(bestSuccess)
# print(paste0(unlist(map(map(whatParents, ~grep(.x, bestNursery)), length)), " ", whatParents, "'s"))

return(bestNursery)

}


