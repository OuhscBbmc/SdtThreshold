############################
### Load Packages
# require(ShinyDash) # To install, run devtools::install_github('ShinyDash', 'trestletech') #See https://groups.google.com/forum/#!topic/shiny-discuss/V7WUQA7aAiI
require(scales)
require(RColorBrewer)
require(grid)
require(ggplot2)
# http://shiny.rstudio.com/articles/css.html

############################
### Main UI Function
sliderWidth <- "100%"
shinyUI(fluidPage(
  h1('--minimal branch--'),
  h3('Diagnostic Values'),
  tableOutput('Diagnostic')
)) #End of fluidPage and ShinyUI
