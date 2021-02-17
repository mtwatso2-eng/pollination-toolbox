about <- list(

  # Display the app's README markdown document
  "ui" = fluidPage(
    includeMarkdown("README.md")
  ),

  "server" = function(input, output, session){
    
  }
  
)
