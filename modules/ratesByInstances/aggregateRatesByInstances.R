aggregateRatesByInstances <- function(input, cache){
  
  validate(need(
    (input$ratesByInstancesFilter %in%
      cache$master$consolidated[[input$ratesByInstancesVariable]]),
    paste("No pollinations with", input$ratesByInstancesFilter, "as", input$ratesByInstancesVariable)
  ))
  
  
  daysOld <- 10
  aggregated <- cache$master$consolidated %>%
    filter(
      DedicationStatus == "Compatibility" &
      .data[[input$ratesByInstancesVariable]] == input$ratesByInstancesFilter &
      Date <= ((FailedPollinationTimestamp %>% na.omit %>% max %>% as.Date) - daysOld)
    ) %>%
    group_by(Cross) %>%
    mutate(Instance = getCrossInstance(Crosses)) %>%
    arrange(Instance) %>%
    mutate(
      CumulativeSuccess = FailedPollinationTimestamp %>% 
        cumulativeApply(function(x){sum(is.na(x)) / length(x)}),
      CumulativeSuccessFraction = FailedPollinationTimestamp %>% 
        cumulativeApply(function(x){paste(sum(is.na(x)), "for",length(x))})
    )
  
  plot <- ggplot(
    aggregated,
    aes(x = Instance, y = CumulativeSuccess, label = CumulativeSuccessFraction, label2 = Timestamp, color = Cross)
  ) +
    geom_line() +
    geom_point() +
    ggtitle("Cumulative Success Rate by Instance") +
    ylab("Cumulative Success Rate") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1))
  
  plot <- ggplotly(plot,dynamicTicks = TRUE) %>%
    layout(yaxis = list(tickformat = "%"))
  
  return(plot)
  
}