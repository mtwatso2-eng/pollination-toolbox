tabulateDedicationHelper <- function(input, cache){
    
  lastFailedPollinationDate <- (
    cache$master$consolidated$FailedPollinationTimestamp %>%
      na.omit %>%
      max %>% 
      as.Date
  )
  
  aggregated <- cache$master$consolidated %>%
    filter(DedicationStatus == "Compatibility") %>%
    group_by(Cross) %>%
    mutate(
      IsTenPlusDaysOld = (Date <= lastFailedPollinationDate - 10),
      Compatibilities = n(),
      Old = sum(IsTenPlusDaysOld, na.rm = TRUE),
      Failed = sum(!is.na(FailedPollinationTimestamp) & IsTenPlusDaysOld, na.rm = TRUE),
      Surviving = (Old - Failed) * ((Old - Failed) >= 0),
      SuccessRateEstimate = round(Surviving / Old, 2),
      CompatibilityCapsuleCollections = sum(!is.na(CapsuleCollectionTimestamp), na.rm = TRUE),
    ) %>%
    slice(1) %>%
    ungroup() %>%
    select(Cross, Compatibilities:CompatibilityCapsuleCollections, Program:Parents) %>%
    filter(!is.na(Cross)) %>%
    arrange(desc(SuccessRateEstimate)) %>%
    arrange(FemaleCode)
  
  return(aggregated)
  
}
