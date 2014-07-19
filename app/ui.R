############################
### Load Packages
# require(ShinyDash) # To install, run devtools::install_github('ShinyDash', 'trestletech') #See https://groups.google.com/forum/#!topic/shiny-discuss/V7WUQA7aAiI
require(scales)
require(RColorBrewer)
require(grid)
require(ggplot2)

############################
### Main UI Function
sliderWidth <- "100%"
shinyUI(pageWithSidebar(
  headerPanel('Rob Hamm\'s SDT and Threshold Comparison'),
  sidebarPanel(
    textOutput(outputId='lblCutoff'), #TODO: add a tooltip
    textOutput(outputId='lblSpecificity'), #TODO: add a tooltip with something like '(ie, the area of the blue curve left of the cutoff)'
    textOutput(outputId='lblSensitivity'), #TODO: add a tooltip with something like '(ie, the area of the red curve right of the cutoff)'
    textOutput(outputId='lblTxThreshold'), #TODO: add a tooltip
    sliderInput(inputId="muN", label="Nondiseased Mean", min=0, max=100, value=0, step=1, width=sliderWidth),
    sliderInput(inputId="muD", label="Diseased Mean", min=0, max=100, value=25, step=1, width=sliderWidth),
    sliderInput(inputId="sigmaN", label="Nondiseased SD", min=5, max=40, value=10, step=1, width=sliderWidth),
    sliderInput(inputId="sigmaD", label="Diseased SD", min=5, max=40, value=12, step=1, width=sliderWidth),
    sliderInput(inputId="uTP", label="u(TP) = Utility of a True, Positive Decision to Treat", min=0, max=1, value=.90, step=.01, width=sliderWidth),
    sliderInput(inputId="uFN", label="u(FN) = Utility of a False, Negative Decision to Treat", min=0, max=1, value=.80, step=.01, width=sliderWidth),
    sliderInput(inputId="uFP", label="u(FP) = Utility of a False, Positive Decision to Treat", min=0, max=1, value=.95, step=.01, width=sliderWidth),
    sliderInput(inputId="uTN", label="u(TN) = Utility of a True, Negative Decision to Treat", min=0, max=1, value=.99, step=.01, width=sliderWidth),
    "Robert M. Hamm, PhD; Clinical Decision Making Program; Department of Family Medicine; University of Oklahoma Health Sciences Center; Oklahoma City OK 73190; 405/271-8000  ext 32306; Fax:  405/271-4125; email:  robert-hamm@ouhsc.edu"
  ),
  mainPanel(
    plotOutput('plotBayesian'),
    plotOutput('plotPdf'),    plotOutput('plotTxThreshold'),
    plotOutput('plotRoc')
  )
))