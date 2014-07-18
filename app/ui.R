############################
### Load Packages
# require(ShinyDash) # To install, run devtools::install_github('ShinyDash', 'trestletech') #See https://groups.google.com/forum/#!topic/shiny-discuss/V7WUQA7aAiI
require(scales)
require(RColorBrewer)
require(grid)
require(ggplot2)

############################
### Main UI Function

shinyUI(pageWithSidebar(
  headerPanel('Rob Hamm\'s SDT and Threshold Comparison'),
  sidebarPanel(
    sliderInput(inputId="muN", label="Nondiseased Mean", min=0, max=100, value=35, step=1),
    sliderInput(inputId="muD", label="Diseased Mean", min=0, max=100, value=55, step=1),
    sliderInput(inputId="sigmaN", label="Nondiseased SD", min=0, max=40, value=10, step=1),
    sliderInput(inputId="sigmaD", label="Diseased SD", min=0, max=40, value=7, step=1)#,
    #     selectInput('xcol', 'X Variable', names(iris)),
    #     selectInput('ycol', 'Y Variable', names(iris),
    #                 selected=names(iris)[[2]]),
    #     numericInput('clusters', 'Cluster count', 3,
    #                  min = 1, max = 9)
  ),
  mainPanel(
    plotOutput('plotPdf')
  )
))