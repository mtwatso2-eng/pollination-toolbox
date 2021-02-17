plotPollinationCounts <- function(input, cache){
  
  trimmed <- cache$master$consolidated %>%
    filter(Date >= input$dateRange[1] & Date <= input$dateRange[2])
  
  validate(need(nrow(trimmed) > 0, "No pollinations for selected date range."))
  
  trimmed %<>% mutate(Group = removeSubgroup(Group))
  
  par(mfrow=c(1,2))
  
  # plot pollination counts by pollinator, 
  plotSummary(
    aggregate = countAggregate(data = as.data.frame(trimmed), x = "Pollinator", fill = input$fillGroup),
    dateRange = input$dateRange
  )
  
  plotSummary(
    aggregate = countAggregate(data = as.data.frame(trimmed), x = "Group", fill = input$fillGroup),
    dateRange = input$dateRange
  )
  
  return(recordPlot())
  
}

plotSummary <- function(aggregated, dateRange){
  
  ymax <- aggregated$max * 1.3
  
  main <- paste(
    strftime(dateRange[1], "%D"),
    "-",
    strftime(dateRange[2], "%D")
  )
  
  barplot(
    height = aggregated$bars,
    ylim = c(0, ymax),
    names.arg = aggregated$df[,1],
    main = main,
    xlab = names(df)[1],
    ylab = "# Pollinations",
    col = rainbow(ncol(aggregated$df[-1])),
    las = 2,
    legend = names(aggregated$df[-1]),
    args.legend=list(x="topleft")
  )
  
}
