about <- list(

  "ui" = fluidPage(
    readChar("README.txt", file.info("README.txt")$size)
  ),

  "server" = function(input, output, session){
    
  }
  
)
