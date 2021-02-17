tabulatePollinationCounts <- function(input, cache){
  
  trimmed <- cache$master$consolidated %>%
    filter(!is.na(Pollinator)) %>%
    group_by(Cross) %>%
    mutate(NormalSecondsPerPollination = mean(TimeTakenSecs, na.rm = TRUE)) %>%
    group_by(Cross, Pollinator) %>%
    mutate(PersonalNormalSecondsPerPollination = mean(TimeTakenSecs, na.rm = TRUE)) %>%
    ungroup() %>%
    filter(Date >= input$dateRange[1] & Date <= input$dateRange[2])
  
  validate(need(nrow(trimmed) > 0, ""))
  
  aggregated <- trimmed %>%
    group_by(Pollinator, .data[[input$fillGroup]]) %>%
    mutate(
      SecondsPerPollination = round(mean(TimeTakenSecs, na.rm = TRUE), 0),
      PercentBetterThanAverage = round(mean(TimeTakenSecs / NormalSecondsPerPollination, na.rm = TRUE), 2),
      PercentBetterThanPersonalAverage = round(mean(TimeTakenSecs / PersonalNormalSecondsPerPollination, na.rm = TRUE), 2),
      Total = n(),
      Count = n()
    ) %>%
    slice(1) %>%
    spread(.data[[input$fillGroup]], Count) %>%
    select(Pollinator, SecondsPerPollination:last_col()) %>%
    group_by(Pollinator) %>%
    mutate(across(c(SecondsPerPollination:PercentBetterThanPersonalAverage), 
      ~ weighted.mean(.x, Total, na.rm = TRUE)
    )) %>%
    mutate(across(c(PercentBetterThanAverage:PercentBetterThanPersonalAverage),
      ~ scales::label_percent()(1 - .x) %>%
      paste0(ifelse(. > 0, "+", ""), .)
    )) %>%
    mutate(across(SecondsPerPollination,
      ~ round(.x)
    )) %>%
    mutate(across(Total:(ncol(.) - 1), function(x){sum(x, na.rm = TRUE)})) %>%
    slice(1) %>%
    ungroup() %>%
    arrange(desc(Total))
  
  if(!input$percentBetterThanAverage)
    aggregated %<>% select(!PercentBetterThanAverage)
  if(!input$percentBetterThanPersonalAverage)
    aggregated %<>% select(!PercentBetterThanPersonalAverage)
  return(aggregated)
  
}