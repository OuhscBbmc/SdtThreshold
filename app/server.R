#Starting from the Shiny gallery example: http://shiny.rstudio.com/gallery/kmeans-example.html
#######################################
### Load Packages
require(plyr)
require(grid)
require(ggplot2)
require(ggthemes)
require(colorspace)
require(RColorBrewer)

#######################################
### Declare paths to external files

#######################################
### Declare globals
measurementRange <- c(-40, 100)
stepWidthMeasurement <- 1
stepWidthProbability <- .01
paletteDisease <- RColorBrewer::brewer.pal(n=3, name="Set1")[1:2]; names(paletteDisease) <- c("T", "F")
paletteTest <- RColorBrewer::brewer.pal(n=3, name="Accent")[2:3]; names(paletteTest) <- c("P", "N")
#######################################
### Declare funcions that don't depend on reactives or user inputs.
CalculateNondiseasePdf <- function( score, mu, sigma, baseRate=NA ) {
  baseRate <- ifelse(is.na(baseRate), 0, baseRate) #Notice missing values are set to 0, not 1 like with diseased.
  return( dnorm(x=score, mean=mu, sd=sigma) * (1-baseRate))
}
CalculateDiseasePdf <- function( score, mu, sigma, baseRate=NA ) {
  baseRate <- ifelse(is.na(baseRate), 1, baseRate) #Notice missing values are set to 1, not 0 like with nondiseased.
  return( dnorm(x=score, mean=mu, sd=sigma) * baseRate)
}
CalculateNondiseaseCdf <- function( score, mu, sigma, baseRate=NA ) { # Specificity
  baseRate <- ifelse(is.na(baseRate), 0, baseRate) #Notice missing values are set to 0, not 1 like with diseased.
  return( pnorm(q=score, mean=mu, sd=sigma) * (1-baseRate) )
}
CalculateDiseaseCdf <- function( score, mu, sigma, baseRate=NA ) {
  baseRate <- ifelse(is.na(baseRate), 1, baseRate) #Notice missing values are set to 1, not 0 like with nondiseased.
  return( pnorm(q=score, mean=mu, sd=sigma) * (baseRate) )
}
CalculateUtilityNondiseased <- function( probability, uTN, uFN) {
  return( approx(x=0:1, y=c(uTN, uFN), xout=probability)$y )
}
CalculateUtilityDiseased <- function( probability, uFP, uTP) {
  return( approx(x=0:1, y=c(uFP, uTP), xout=probability)$y )
}
CalculateSpecificity <- function( scores, mu, sigma, baseRate=NA ) { #CalculateNondiseaseCdf
  return( CalculateNondiseaseCdf(scores, mu, sigma, baseRate) )
}
CalculateSensitivity <- function( scores, mu, sigma, baseRate=NA ) { #1 - CalculateDiseaseCdf
  return( 1 - CalculateDiseaseCdf(scores, mu, sigma, baseRate) )
}
CalculatePosteriorPositive <- function( prior, sensitivity, specificity ) {
  return( (sensitivity * prior) / ((sensitivity * prior)+((1-specificity) * (1-prior))) )
}
CalculatePosteriorNegative <- function( prior, sensitivity, specificity ) {
  return( ((1-sensitivity) * prior) / (((1-sensitivity) * prior)+(specificity * (1-prior))) )
}
#######################################
### Load and tweak data

#######################################
### The file's main function
shinyServer(function(input, output, session) {
  
  #######################################
  ### Translate UI Inputs into values the R calculations can accept.
  # User inputs of predictor values.
  userInputs <- reactive({
    list(
      muN = input$muN,
      muD = input$muD,
      sigmaN = input$sigmaN,
      sigmaD = input$sigmaD,
      uTP = input$uTP,
      uFN = input$uFN,
      uFP = input$uFP,
      uTN = input$uTN
    )
  })
  MeasurementData <- reactive({
    s <- userInputs() #'S' stands for Sliders
    d <- data.frame(Score=seq(from=measurementRange[1], to=measurementRange[2], by=stepWidthMeasurement))
    d$NondiseasedPdf <- CalculateNondiseasePdf(score=d$Score,  s$muN, s$sigmaN, baseRate=NA)
    d$DiseasedPdf <- CalculateDiseasePdf(score=d$Score, s$muD, s$sigmaD, baseRate=NA)
    d$NondiseasedCdfL <- CalculateNondiseaseCdf(score=d$Score,  s$muN, s$sigmaN, baseRate=NA) #Specificity; the 'L' stands for "left" of the point.
    d$DiseasedCdfL <- CalculateDiseaseCdf(score=d$Score, s$muD, s$sigmaD, baseRate=NA)
    d$NondiseasedCdfR <- 1- d$NondiseasedCdf #max(d$NondiseasedCdf) - d$NondiseasedCdf
    d$DiseasedCdfR <- 1 - d$DiseasedCdf #max(d$DiseasedCdf) - d$DiseasedCdf #Sensitivity; the 'R' stands for "right" of the point.
    d$LRInstant <- d$DiseasedPdf / d$NondiseasedPdf
    d$LRPlus <- (d$DiseasedCdfR)  / (d$NondiseasedCdfR)
    d$LRMinus <- (d$DiseasedCdfL)  / (d$NondiseasedCdfL)
    d$LRRatio <- d$LRPlus / d$LRMinus
    return( d )
  })
  ProbabilityData <- reactive({
    s <- userInputs()
    ds <- data.frame(Probability=seq(from=0, to=1, by=stepWidthProbability))
    ds$UtilityNondiseased <- CalculateUtilityNondiseased(probability=ds$Probability, uTN=s$uTN, uFN=s$uFN)
    ds$UtilityDiseased <- CalculateUtilityDiseased(probability=ds$Probability, uFP=s$uFP, uTP=s$uTP)
    ds$Prior <- ds$Probability
    # ds$PosteriorPositive <- (SensitivityAtCutoff() * ds$Prior) / ((SensitivityAtCutoff() * ds$Prior)+((1-SpecificityAtCutoff()) * (1-ds$Prior)))
    # ds$PosteriorNegative <- ((1-SensitivityAtCutoff()) * ds$Prior) / (((1-SensitivityAtCutoff()) * ds$Prior)+(SpecificityAtCutoff() * (1-ds$Prior)))
    ds$PosteriorPositive <- CalculatePosteriorPositive(ds$Prior, SensitivityAtCutoff(), SpecificityAtCutoff())
    ds$PosteriorNegative <- CalculatePosteriorNegative(ds$Prior, SensitivityAtCutoff(), SpecificityAtCutoff())
    return( ds )
  })
  PdfDifference <- function( x ) {
    difference <- CalculateNondiseasePdf(score=x,  userInputs()$muN, userInputs()$sigmaN, baseRate=NA) - 
      CalculateDiseasePdf(score=x, userInputs()$muD, userInputs()$sigmaD, baseRate=NA)
    return( difference )
  }
  PdfIntersectX <- reactive({  
    searchRange <- range(c(userInputs()$muD + c(1,-1)*userInputs()$sigmaD, userInputs()$muN + c(1,-1)*userInputs()$sigmaD))
    u <- NULL
    try(
      u <- uniroot(f=PdfDifference, interval=searchRange),
      silent = TRUE
    )
    return( ifelse(!is.null(u), u$root, NA_real_) )    
  })
  PdfIntersectY <- reactive({
    CalculateNondiseasePdf(score=PdfIntersectX(),  userInputs()$muN, userInputs()$sigmaN, baseRate=NA)
  })
  SpecificityAtCutoff <- reactive({
    s <- userInputs()
    CalculateSpecificity( scores=PdfIntersectX(), mu=s$muN, sigma=s$sigmaN, baseRate=NA )
  })
  SensitivityAtCutoff <- reactive({
    s <- userInputs()
    CalculateSensitivity( scores=PdfIntersectX(), mu=s$muD, sigma=s$sigmaD, baseRate=NA )
  })
  ThresholdDifference <- function( probability ) {
    s <- userInputs() #'s' stands for sliders
    difference <- CalculateUtilityNondiseased(probability, uTN=s$uTN, uFN=s$uFN) - 
      CalculateUtilityDiseased(probability, uFP=s$uFP, uTP=s$uTP)
    return( difference )
  }
  ThresholdIntersectX <- reactive({
    #TODO: calculate this as the intersection = (uTN-uFP)/(uTP-uFN); figure out how to respond when it's outside [0, 1].
    u <- NULL
    try(
      u <- uniroot(f=ThresholdDifference, interval=c(0,1)),
      silent = TRUE
    )
    return( ifelse(!is.null(u), u$root, NA_real_) )  
  })
  ThresholdIntersectY <- reactive({
    CalculateUtilityNondiseased(ThresholdIntersectX(),  userInputs()$uTN, userInputs()$uFN)
  })
  peakN <- reactive({
    CalculateNondiseasePdf(score=userInputs()$muN,  userInputs()$muN, userInputs()$sigmaN, baseRate=NA)
  })
  peakD <- reactive({
    CalculateDiseasePdf(score=userInputs()$muD,  userInputs()$muD, userInputs()$sigmaD, baseRate=NA)
  })
  ThresholdPositiveTestDifference <- function( prior ) {
    difference <- CalculatePosteriorPositive(prior, SensitivityAtCutoff(), SpecificityAtCutoff()) - ThresholdIntersectX()
    return( difference )
  }
  ThresholdNegativeTestDifference <- function( prior ) {
    difference <- CalculatePosteriorNegative(prior, SensitivityAtCutoff(), SpecificityAtCutoff()) - ThresholdIntersectX()
    return( difference )
  }
  ThresholdPositiveTestIntersect <- reactive({
    u <- NULL
    try(
      u <- uniroot(f=ThresholdPositiveTestDifference, interval=c(0,1)),
      silent = TRUE
    )
    return( ifelse(!is.null(u), u$root, NA_real_) )  
  })
  ThresholdNegativeTestIntersect <- reactive({
    u <- NULL
    try(
      u <- uniroot(f=ThresholdNegativeTestDifference, interval=c(0,1)),
      silent = TRUE
    )
    return( ifelse(!is.null(u), u$root, NA_real_) )  
  })
  
  #TODO: make these into one table, instead of loose labels
  output$lblCutoff <- reactive({ return( paste("Diagnostic Cutoff:", round(PdfIntersectX())) ) })
  output$lblSpecificity <- reactive({ return( paste("Specificity at Cutoff:", scales::percent(round(SpecificityAtCutoff(), 3))) ) })
  output$lblSensitivity <- reactive({ return( paste("Sensitivity at Cutoff:", scales::percent(round(SensitivityAtCutoff(), 3))) ) })
  output$lblTxThreshold <- reactive({ return( paste("Treatment Threshold:", scales::percent(round(ThresholdIntersectX(), 3))) ) })
  output$lblNoTestTestThreshold <- reactive({ return( paste("NoTest-Test Threshold:", scales::percent(round(ThresholdPositiveTestIntersect(), 3))) ) })
  output$lblTestTreatThreshold <- reactive({ return( paste("Test-Treat Threshold:", scales::percent(round(ThresholdNegativeTestIntersect(), 3))) ) })
  
  output$plotBayesian <- renderPlot({
    thresholdPositiveTestIntersectX <- ThresholdPositiveTestIntersect()
    thresholdPositiveTestIntersectY <- CalculatePosteriorPositive(thresholdPositiveTestIntersectX, SensitivityAtCutoff(), SpecificityAtCutoff())
    thresholdNegativeTestIntersectX <- ThresholdNegativeTestIntersect()
    thresholdNegativeTestIntersectY <- CalculatePosteriorNegative(thresholdNegativeTestIntersectX, SensitivityAtCutoff(), SpecificityAtCutoff())
    
    
    ds <- ProbabilityData()
    g <- ggplot(ds, aes(x=Prior)) +
      geom_path(aes(y=PosteriorPositive), color=paletteTest["P"], size=4, alpha=.5, lineend="round") +
      geom_path(aes(y=PosteriorNegative), color=paletteTest["N"], size=4, alpha=.5, lineend="round") +
      geom_hline(yintercept=ThresholdIntersectX(), linetype="F3", color="gray30", alpha=.5)+
      annotate(geom="segment", x=0, y=0, xend=1, yend=1, size=2, alpha=.2, lineend="round") +
      annotate(geom="segment", x=thresholdPositiveTestIntersectX, y=thresholdPositiveTestIntersectX, xend=thresholdPositiveTestIntersectX, yend=thresholdPositiveTestIntersectY, size=2, alpha=.2,lineend="round") +
      annotate(geom="segment", x=thresholdNegativeTestIntersectX, y=thresholdNegativeTestIntersectX, xend=thresholdNegativeTestIntersectX, yend=thresholdNegativeTestIntersectY, size=2, alpha=.2, lineend="round") +
      
      annotate(geom="text", label="probability given\npositive test", x=0, y=1, hjust=0, vjust=1, color=paletteTest["P"]) +
      annotate(geom="text", label="probability given\nnegative test", x=1, y=0, hjust=1, vjust=0, color=paletteTest["N"]) +
      annotate(geom="text", label="if no test", x=.5, y=.5, hjust=.5, angle=45, color="gray30", alpha=.5) +
      annotate(geom="text", label="tx\nthreshold", x=1, y=ThresholdIntersectX(), hjust=1, vjust=.5, color="gray30", alpha=.5) +
      annotate(geom="text", label="NoTest/Test\nThreshold", x=thresholdPositiveTestIntersectX, y=thresholdPositiveTestIntersectX, hjust=0, vjust=.5, angle=90, color="gray30", alpha=.5) +
      annotate(geom="text", label="Test/Treat\nThreshold", x=thresholdNegativeTestIntersectX, y=ThresholdIntersectX(), hjust=0, vjust=.5, angle=90, color="gray30", alpha=.5) +
      scale_x_continuous(label=scales::percent) +
      scale_y_continuous(label=scales::percent) +
      coord_fixed() +
      theme_bw() +
      labs(title="Bayesian Graph with Treatment Threshold", x="Pretest Probability", y="Postest Probability")
    print(g)
  }) 
  output$plotPdf <- renderPlot({
    ds <- MeasurementData()
    g <- ggplot(ds, aes(x=Score)) +
      geom_path(aes(y=NondiseasedPdf), color=paletteDisease["F"], size=4, alpha=.3, lineend="round") +
      geom_path(aes(y=DiseasedPdf), color=paletteDisease["T"], size=4, alpha=.3, lineend="round") +
      annotate(geom="segment", x=PdfIntersectX(), y=PdfIntersectY(), xend=PdfIntersectX(), yend=0, size=4, alpha=.2, lineend="butt", color=paletteDisease["F"]) +
      annotate(geom="segment", x=PdfIntersectX(), y=PdfIntersectY(), xend=PdfIntersectX(), yend=0, size=4, alpha=.2, lineend="butt", color=paletteDisease["T"]) +
      annotate(geom="text", label="Nondiseased", x=userInputs()$muN, y=peakN(), vjust=0, color=paletteDisease["F"]) +
      annotate(geom="text", label="Diseased", x=userInputs()$muD, y=peakD(), vjust=0, color=paletteDisease["T"]) +
      annotate(geom="text", label="Cutoff", x=PdfIntersectX(), y=0, hjust=-.05, color="gray30", angle=90) +
      theme_bw() +
      labs(title="PDFs", x="Diagnostic Score", y="Probability Density")
    print(g)
  })  
  output$plotRoc <- renderPlot({
    ds <- MeasurementData()
    g <- ggplot(ds, aes(x=1-NondiseasedCdfL, y=DiseasedCdfR)) +
      geom_path(size=4, alpha=.5, lineend="round") +
      scale_x_continuous(label=scales::percent) +
      scale_y_continuous(label=scales::percent) +
      coord_fixed(ratio=1, xlim=c(1.03,-.03), ylim=c(-.03,1.03)) +
      theme_bw() +
      labs(title="ROC", x="1 - Specificity = False Positive Probability", y="Sensitivity = True Positive Probability")
    print(g)
  })
  output$plotTxThreshold <- renderPlot({
    ds <- ProbabilityData()
    s <- userInputs() #'s' stands for sliders
    g <- ggplot(ds, aes(x=Probability)) +
      geom_path(aes(y=UtilityNondiseased), size=4, alpha=.3, color=paletteDisease["F"], lineend="round") +
      geom_path(aes(y=UtilityDiseased), size=4, alpha=.3, color=paletteDisease["T"], lineend="round") +
      annotate(geom="text", label="Utility of not treating,\nas if patient doesn't have disease", x=-Inf, y=-Inf, hjust=0, vjust=-2.5, color=paletteDisease["F"], linespace=-1) +
      annotate(geom="text", label="Utility of treating,\nas if patient has disease", x=-Inf, y=-Inf, hjust=0, vjust=-1, color=paletteDisease["T"], linespace=4) +
      
      annotate(geom="text", label="u(TN)", x=0, y=s$uTN, hjust=0, vjust=0, color=paletteDisease["F"]) +
      annotate(geom="text", label="u(FP)", x=0, y=s$uFP, hjust=0, vjust=0, color=paletteDisease["T"]) +
      annotate(geom="text", label="u(FN)", x=1, y=s$uFN, hjust=1, vjust=0, color=paletteDisease["F"]) +
      annotate(geom="text", label="u(TP)", x=1, y=s$uTP, hjust=1, vjust=0, color=paletteDisease["T"]) +
      
      scale_x_continuous(label=scales::percent) +
      scale_y_continuous(label=scales::percent) +
      coord_cartesian(xlim=c(1.02,-.02)) +
      #       coord_fixed(ratio=1, xlim=c(1.03,-.03), ylim=c(-.03,1.03)) +
      theme_bw() +
      labs(title="Treatment Threshold Probability", x="Probability Patient has the Disease", y="Expected Utility of Treatment")

    if( !is.na(ThresholdIntersectX()) ) {
      g  <- g + 
        annotate(geom="segment", x=ThresholdIntersectX(), y=ThresholdIntersectY(), xend=ThresholdIntersectX(), yend=0, size=4, alpha=.15, lineend="butt", color=paletteDisease["F"]) +
        annotate(geom="segment", x=ThresholdIntersectX(), y=ThresholdIntersectY(), xend=ThresholdIntersectX(), yend=0, size=4, alpha=.15, lineend="butt", color=paletteDisease["T"]) +
        annotate(geom="text", label="Tx Threshold", x=ThresholdIntersectX(), y=-Inf, hjust=-.05, color="gray30", angle=90)
    }
    print(g)
  })  
})