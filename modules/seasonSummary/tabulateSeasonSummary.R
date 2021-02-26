tabulateSeasonSummary <- function(input, cache){
  
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
      SuccessRate = ifelse(Old > 0, round(CapsuleCollections / Old, 2), NA),
      ToMatureCapsules = as.integer(round(
        sum(na.rm = TRUE,
          (is.na(CapsuleCollectionTimestamp) & is.na(FailedPollinationTimestamp) & (Date >= Sys.Date() - 40) & (Date < input$projectionDate - 40))
        ) * SuccessRate
      )),
      ToDoCapsules = as.integer(round(
        (Sys.Date() < input$projectionDate - 40) *
        ((input$projectionDate - 40) - Sys.Date()) *
        (ThisWeek / 7) * SuccessRate
      ))
    ) %>%
    slice(1) %>%
    ungroup() %>%
    full_join(cleanedSeed, by = c("Parents" = "Cross"))
  
  if(input$costPerSeed){
    hourlyWage <- 12
    aggregated %<>% 
      mutate(
        CostPerSeed = hourlyWage * (TimeTakenSecs / (SuccessRate * SeedPerCapsule)) / (60 * 60)
      )
  }
  
  if(input$seasonSummaryGroup != "Totals")
    aggregated %<>% group_by(.data[[input$seasonSummaryGroup]])
  
  aggregated %<>%
    mutate(
      across(SeedPerCapsule:last_col(), function(x){round(weighted.mean(x, CapsuleCollections, na.rm = TRUE), 2)}),
      across(where(is.integer), function(x){round(sum(x, na.rm = TRUE))}),
      SuccessRate = ifelse(CapsuleCollections <= Old , round(CapsuleCollections / Old, 2), NA),
      ProjectedCapsules = CapsuleCollections + ToMatureCapsules
    ) %>% 
    relocate(ProjectedCapsules, .after = ToDoCapsules) %>%
    slice(1) %>% 
    ungroup() %>%
    arrange(desc(Total))
  
  if(input$costPerSeed)
    aggregated %<>% mutate(CostPerSeed = paste0("$", CostPerSeed))
  
  if(input$toDoCapsules){
    aggregated %<>% mutate(ProjectedCapsules = CapsuleCollections + ToMatureCapsules + ToDoCapsules)
  }

  if(input$seasonSummaryGroup == "Totals"){
    aggregated %<>% select(Total:last_col())
  }
  else {
    aggregated %<>%
      select(input$seasonSummaryGroup, Total:last_col()) %>%
      filter(!is.na(.data[[input$seasonSummaryGroup]]))
  }
  
  if(input$projectionDate <= Sys.Date())
    aggregated %<>% select(!ToMatureCapsules:ProjectedCapsules)
  else if (!input$toDoCapsules)
    aggregated %<>% select(!ToDoCapsules)
  
  if(input$seasonSummaryGroup == "Cross")
    aggregated %<>% merge(., cache$master$crosses, by = "Cross", all.x = TRUE, sort = FALSE)
  
  return(aggregated)
  
  
}
