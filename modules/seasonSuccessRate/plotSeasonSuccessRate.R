plotSeasonSuccessRate <- function(input, cache){
  
  daysOld <- 10
  trimmed <- cache$master$consolidated %>%
    filter(
      Date <= ((FailedPollinationTimestamp %>% na.omit %>% max %>% as.Date) - daysOld) &
      DedicationStatus == "Compatibility"
    )
  
  if(input$lineGroup == "Don't group"){
    aggregated <- trimmed %>%
      group_by(Date) %>%
      summarise(Success = sum(is.na(FailedPollinationTimestamp)) / n()) %>%
      ungroup()

    plot <- ggplot(aggregated, aes_string(x = "Date", y = "Success")) +
      geom_line() +
      geom_point() +
      ggtitle("Pollination Success Rate over Time") +
      ylab("Pollination Success Rate") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) 
    
    plot <- ggplotly(plot, dynamicTicks = TRUE) %>%
      layout(yaxis = list(tickformat = "%"))
    
    return(plot)
  }

  aggregated <- trimmed %>%
    filter(!is.na(.data[[input$lineGroup]])) %>%
    group_by(Date, .data[[input$lineGroup]]) %>%
    summarise(Success = sum(is.na(FailedPollinationTimestamp)) / n())
  
  plot <- ggplot(aggregated, aes_string(x = "Date", y = "Success", color = input$lineGroup)) +
    geom_line() +
    geom_point() +
    ggtitle(paste("Pollination Success Rate over Time, Grouped by", input$lineGroup)) +
    ylab("Pollination Success Rate") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1))
  
  plot <- ggplotly(plot, dynamicTicks = TRUE) %>%
    layout(yaxis = list(tickformat = "%"))
  
  return(plot)
  
}
