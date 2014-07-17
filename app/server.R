# This is based on Joe Cheng's leaflet-shiny demo: https://github.com/jcheng5/leaflet-shiny/blob/master/inst/examples/population/server.R
#######################################
### Load Packages
require(plyr)
require(grid)
require(ggplot2)
require(ggthemes)
require(colorspace)
require(RColorBrewer)

#######################################
# Declare paths to external files
# pathInput <- "../../Data/Derived/BlaBla.rds"

#######################################
### Declare global variables
palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
          "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
#######################################
### Load and tweak data

#######################################
### Declare functions

#######################################
# The file's main function

#Starting the file with the Shiny gallery example: http://shiny.rstudio.com/gallery/kmeans-example.html

shinyServer(function(input, output, session) {
  
  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    iris[, c(input$xcol, input$ycol)]
  })
  
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  
  output$plot1 <- renderPlot({
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })
  
})