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
  fluidRow(headerPanel('Rob Hamm\'s SDT and Threshold Comparison')),
  fluidRow(headerPanel('--escalate branch b6--')),
  fluidRow(
    column( 
      width = 3,
      wellPanel(tabsetPanel( type = "tabs",
        tabPanel(
          title = "Values",
          h3('Derived Values'),
          tableOutput('Derived'), #TODO: add tooltips
          h3('Diagnostic Values'),
          tableOutput('Diagnostic'), 
          #TODO: add a tooltip with something like '(ie, the area of the blue curve left of the cutoff)'
          #TODO: add a tooltip with something like '(ie, the area of the red curve right of the cutoff)'
          
          h3('Specified Values'),
          sliderInput(inputId="muD", label="Diseased Mean", min=0, max=100, value=25, step=1, width=sliderWidth),
          sliderInput(inputId="sigmaD", label="Diseased SD", min=5, max=40, value=12, step=1, width=sliderWidth),
          sliderInput(inputId="uTP", label="u(TP) = Utility of a True, Positive Decision to Treat", min=0, max=1, value=.90, step=.01, width=sliderWidth),
          sliderInput(inputId="uFN", label="u(FN) = Utility of a False, Negative Decision to Treat", min=0, max=1, value=.80, step=.01, width=sliderWidth),
          sliderInput(inputId="uFP", label="u(FP) = Utility of a False, Positive Decision to Treat", min=0, max=1, value=.95, step=.01, width=sliderWidth),
          sliderInput(inputId="uTN", label="u(TN) = Utility of a True, Negative Decision to Treat", min=0, max=1, value=.99, step=.01, width=sliderWidth)
        ), # End of first tab
        tabPanel(
          title = "Contact",
          h3('Concept and Prototype by Rob Hamm'),          
          "Robert M. Hamm, PhD; Clinical Decision Making Program; Department of Family Medicine [www.oumedicine.com/familymedicine]; University of Oklahoma Health Sciences Center; Oklahoma City OK 73190; 405/271-8000 ext 32306; Fax: 405/271-4125; email: robert-hamm@ouhsc.edu",
          h3('Shiny Implementation by Will Beasley'),          
          "William Howard Beasley, PhD; Biomedical and Behavioral Methodology Core [http://ouhsc.edu/bbmc/]; Department of Pediatrics; University of Oklahoma Health Sciences Center; email: william-beasley@ouhsc.edu"
        ) # End of third tab
      )) #End of wellPanel & tabsetPanel
    ), #End of column
    column(
      width=5,
      plotOutput('plotDummy', height='400px')
      #       plotOutput('plotPdf', height='400px')#,    
      #       plotOutput('plotTxThreshold', height='400px')
    ), #End of column
    column(
      width=4,
#       plotOutput('plotRoc', height='300px'),
      plotOutput('plotBayesian', height='500px') #, width='48%'
    ) #End of column
  ) #End of row  
)) #End of fluidPage and ShinyUI
