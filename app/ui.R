############################
### Load Packages
# require(ShinyDash) # To install, run devtools::install_github('ShinyDash', 'trestletech') #See https://groups.google.com/forum/#!topic/shiny-discuss/V7WUQA7aAiI
require(scales)
require(RColorBrewer)
require(grid)
require(ggplot2)

############################
### Main UI Function
sliderWidth <- "45%"
shinyUI(pageWithSidebar(
  headerPanel('Rob Hamm\'s SDT and Threshold Comparison'),
  sidebarPanel(
    sliderInput(inputId="muN", label="Nondiseased Mean", min=0, max=100, value=35, step=1, width=sliderWidth),
    sliderInput(inputId="muD", label="Diseased Mean", min=0, max=100, value=55, step=1, width=sliderWidth),
    sliderInput(inputId="sigmaN", label="Nondiseased SD", min=0, max=40, value=10, step=1, width=sliderWidth),
    sliderInput(inputId="sigmaD", label="Diseased SD", min=0, max=40, value=7, step=1, width=sliderWidth),
    sliderInput(inputId="uTP", label="u(TP) = Utility of a True, Positive Decision to Treat", min=0, max=1, value=.90, step=.01, width=sliderWidth),
    sliderInput(inputId="uFN", label="u(FN) = Utility of a False, Negative Decision to Treat", min=0, max=1, value=.85, step=.01, width=sliderWidth),
    sliderInput(inputId="uFP", label="u(FP) = Utility of a False, Positive Decision to Treat", min=0, max=1, value=.95, step=.01, width=sliderWidth),
    sliderInput(inputId="uTN", label="u(TN) = Utility of a True, Negative Decision to Treat", min=0, max=1, value=.99, step=.01, width=sliderWidth)    
  ),
  mainPanel(
    plotOutput('plotPdf'),
    plotOutput('plotRoc'),
    plotOutput('plotTxThreshold')
  )
))