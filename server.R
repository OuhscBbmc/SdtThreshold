#Starting from the Shiny gallery example: http://shiny.rstudio.com/gallery/kmeans-example.html
#######################################
### Load Packages
require(plyr)
require(grid)
require(ggplot2)
require(ggthemes)
require(colorspace)
require(RColorBrewer)
require(xtable)

#######################################
### Declare paths to external files

#######################################
### Declare globals
# options(shiny.trace=TRUE) #http://stackoverflow.com/questions/23002712/shiny-what-is-the-option-setting-to-display-in-the-console-the-messages-between

#######################################
### Load and tweak data

#######################################
### The file's main function
shinyServer(function(input, output, session) {
  
  #######################################
  ### Translate UI Inputs into values the R calculations can accept.
  # User inputs of predictor values.

  output$Diagnostic <- renderTable({
    d1 <- data.frame(Sys.info())
    d1 <- plyr::rename(d1, c("Sys.info.."="Value"))
    
    d2 <- data.frame(.libPaths())
    d2 <- plyr::rename(d2, c(".libPaths.."="Value"))
    row.names(d2) <- paste0("path", seq_len(nrow(d2)))
    
    d <- rbind(d1, d2)
    return( d )  
  }, include.rownames=T, include.colnames=T) #End Diagnostic Table
})
