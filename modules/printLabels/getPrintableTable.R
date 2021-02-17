getPrintableTable <- function(input, cache){
  
    df <-cache$master$crosses
    df <- df[which(df[,"Group"] == input$printingGroup),]
    df <- expandCrosses(df, 1)
    df <- emptyColumns(df)
    df <- df[,c("HumanReadable", "Scanner")]
    df[,"Buffer"] <- rep.int(0, nrow(df))
  
    return(df)
}
