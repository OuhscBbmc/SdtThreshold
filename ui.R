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
  fluidRow(headerPanel('--minimal branch--')),
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
          title = "Documentation",
          h3('Overview'),
          "Clinical medicine has talked about thresholds starting with Pauker and Kassirer (1975)
            on treatment thresholds (the probability that this patient has the disease, above which you would
            treat the disease, below which you would not) and Pauker and Kassirer (1980) on test thresholds
            (a low probability, below which you would neither test nor treat; a high probability, above which
            you would treat; and do the test in the middle range and treat according to the test result).",
          h3(''),
          "Signal detection theory, in engineering and psychology, has talked about thresholds
            for calling the situation a signal, famously with World War II analysis of sonar signals and explicitly
            in psychology with Green and Swets as a reference milestone.",
          h3(''),
          "The thresholds in clinical medicine depend only on the utilities of the two errors,
            false positives and false negatives. The thresholds in SDT depend on those utilities as well as
            the prior probabilities. The purpose of this calculator is to illustrate why these two thresholds differ.",
          h3(''),
          "The approach will be to produce three graphs: a Strength of Evidence Graph (from
            signal detection theory), a Pauker and Kassirer threshold graph (from clinical medicine), and
            a Bayes' graph (Bayes' football; from clinical medicine) and illustrate how they integrate. All will
            be illuminated.",
          h3(''),
          "The Strength of Evidence Graph has signal strength or evidence strength on the x axis,
            and the y axis shows the probability density curves for each level of strength of evidence,
            for the hypothesis and for the non hypothesis. Two separate density curves. We define each
            density curve as summing to 1. We pick the point of maximum separation, ignoring base rate and utility of errors.",
          h3(''),
          "We leave out of the Strength of Evidence Graph considerations of the base rate
            as well as considerations of error utility. That is because we consider utility in the Threshold Graph (and pass
            it through to the Bayes' Graph), and the Bayes' Graph allows you to apply the analysis to a patient
            with any base rate, any prior probability. If we also considered those here, then in effect the same
            considerations would be considered twice, and things conceptually would be a confused mess.",
          h3(''),
          "There is another way the evidence, under the two hypotheses, can be graphed:
            the total data could recognize base rate, where the data in each curve occur as they would
            in the environment. This could either be by dividing a total probability of 1 into two parts,
            according to the base rate of H and not-H, or it could be a count of observations with normalization
            coming later. (The choice to ignore base rate has implications for whether the base rate is
            represented in this graph, or not. Pure likelyhood ratio ignores the base rate. Or it can represent the
            total impact of evidence including the base rate.)",
          h3(''),
          "The Threshold Graph represents the utilities that would follow from the actions of treating
            or not treating in two basic conditions: if the patient has the disease (hit and miss), at the right end of the x-axis
            and if the patient does not have the disease (false alarm and correct rejection), at the left end of the 
            x-axis. The actions (treat and not treat) are connected by two straight lines, which cross. The lines
            indicate the expected utility of that action in the case of the action, which is a probability mix between
            having the disease and not having the disease. The point of indifference is the treatment threshold probability.
            This threshold is dependent completely on the utilities of the actions. It is not in any way dependent on
            the patient's actual probability, nor on the prevalence.",
          h3(''),
          "The Bayes Graph shows the effects of positive or negative observations on one test or
            finding. The x-axis is the pretest probability. The Y axis is the post test probability. The curve showing the 
            post test probability following a positive observation, for every possible pre-test probability, is above
            the ascending diagonal. The curve of the post test probability after a negative observation is below
            the diagonal. The threshold (from the threshold graph) is a point on the axis with the most up to
            date probability, the y-axis. Draw a straight horizontal line across the graph from that point.
            Where it intersects the two post-test curves represents the no-test/test threshold (the intersection
            with the positive test curve) and the test/treat threshold (the intersection with the negative test curve).
            Project these down to the x-axis to read the probabilities that have those meanings. This is relevant
            for the pre-test probability because you're considering whether to do a test, i.e., you have not done it.",
          h3(''),
          "The test characteristics which determine the positive test and negative test Bayes
            curves come from the Strength of Evidence graph. At whatever cutoff point on the strength of evidence
            continuum that you decide to call the evidence 'positive' (or, for a 'bin'), the likelihood ratio defines the
            test characteristic to be used in interpreting the results. This may be a simple rule or a complex set of
            rules, for example if multiple bins are used, or if we are considering the evidence from multiple tests
            (conceptually a pair of distributions still)."          
        ), # End of second tab
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
      plotOutput('plotPdf', height='400px')#,    
#       plotOutput('plotTxThreshold', height='400px')
    ), #End of column
    column(
      width=4#,
#       plotOutput('plotRoc', height='300px'),
#       plotOutput('plotBayesian', height='500px') #, width='48%'
    ) #End of column
  ) #End of row  
)) #End of fluidPage and ShinyUI
