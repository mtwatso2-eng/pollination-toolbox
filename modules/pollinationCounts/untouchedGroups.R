untouchedGroups <- function(input, cache){
  
  trimmed <- cache$master$consolidated %>%
    filter(Date >= input$dateRange[1] & Date <= input$dateRange[2])
  
  untouchedGroupsString <- (paste(
    "Untouched groups:",
    paste(setdiff(cache$master$crosses$Group, trimmed$Group), collapse = ", ")
  ))
  
  return(untouchedGroupsString)
  
}
