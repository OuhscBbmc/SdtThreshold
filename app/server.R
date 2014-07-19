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
measurementRange <- c(0, 100)
stepWidthMeasurement <- 1
stepWidthProbability <- .01
colorNondiseased <- "blue3"
colorDiseased <- "red3"

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
CalculateNondiseaseCdf <- function( score, baseRate, mu, sigma ) {
  baseRate <- ifelse(is.na(baseRate), 0, baseRate) #Notice missing values are set to 0, not 1 like with diseased.
  return( pnorm(q=score, mean=mu, sd=sigma) * (1-baseRate) )
}
CalculateDiseaseCdf <- function( score, baseRate, mu, sigma ) {
  baseRate <- ifelse(is.na(baseRate), 1, baseRate) #Notice missing values are set to 1, not 0 like with nondiseased.
  return( pnorm(q=score, mean=mu, sd=sigma) * (baseRate) )
}
CalculateUtilityNondiseased <- function( probability, uTN, uFN) {
#   return( ((1 - probability) * uTN) + (probability * uFN) )
  return( approx(x=0:1, y=c(uTN, uFN), xout=probability)$y )
}
CalculateUtilityDiseased <- function( probability, uFP, uTP) {
#   return( (probability * uTP) + ((1-probability) * uFP) )
  return( approx(x=0:1, y=c(uFP, uTP), xout=probability)$y )
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
    #     ds$UtilityDiseased <- (ds$Probability*s$uTP) + ((1-ds$Probability)*s$uFP)
    #     ds$UtilityNondiseased <- ((1-ds$Probability)*s$uTN) + (ds$Probability*s$uFN)
    #     ds$UtilityDiseased <- approx(x=0:1, y=c(s$uFP, s$uTP), xout=ds$Probability)$y
    ds$UtilityNondiseased <- CalculateUtilityNondiseased(probability=ds$Probability, uTN=s$uTN, uFN=s$uFN)
    ds$UtilityDiseased <- CalculateUtilityDiseased(probability=ds$Probability, uFP=s$uFP, uTP=s$uTP)
  
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
  ThresholdDifference <- function( probability ) {
    s <- userInputs() #'s' stands for sliders
    difference <- CalculateUtilityNondiseased(probability, uTN=s$uTN, uFN=s$uFN) - 
      CalculateUtilityDiseased(probability, uFP=s$uFP, uTP=s$uTP)
    return( difference )
  }
  ThresholdIntersectX <- reactive({
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
  output$plotPdf <- renderPlot({
    ds <- MeasurementData()
    g <- ggplot(ds, aes(x=Score)) +
      geom_line(aes(y=NondiseasedPdf), color=colorNondiseased, size=4, alpha=.5) +
      geom_line(aes(y=DiseasedPdf), color=colorDiseased, size=4, alpha=.5) +
      annotate(geom="segment", x=PdfIntersectX(), y=PdfIntersectY(), xend=PdfIntersectX(), yend=0, size=4, alpha=.2, lineend="butt", color=colorNondiseased) +
      annotate(geom="segment", x=PdfIntersectX(), y=PdfIntersectY(), xend=PdfIntersectX(), yend=0, size=4, alpha=.2, lineend="butt", color=colorDiseased) +
      annotate(geom="text", label="Nondiseased", x=userInputs()$muN, y=peakN(), vjust=-.5, color=colorNondiseased) +
      annotate(geom="text", label="Diseased", x=userInputs()$muD, y=peakD(), vjust=-.5, color=colorDiseased) +
      annotate(geom="text", label="Cutoff", x=PdfIntersectX(), y=0, hjust=-.05, color="gray30", angle=90) +
      theme_bw() +
      labs(title="PDFs", x="Diagnostic Score", y="Probability Density")
    print(g)
  })  
  output$plotRoc <- renderPlot({
    ds <- MeasurementData()
    g <- ggplot(ds, aes(x=1-NondiseasedCdfL, y=DiseasedCdfR)) +
      geom_line(size=4, alpha=.5) +
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
      geom_line(aes(y=UtilityNondiseased), size=4, alpha=.3, color=colorNondiseased, lineend="mitre") +
      geom_line(aes(y=UtilityDiseased), size=4, alpha=.3, color=colorDiseased) +
      annotate(geom="text", label="Utility of not treating,\nas if patient doesn't have disease", x=-Inf, y=-Inf, hjust=0, vjust=-2.5, color=colorNondiseased, linespace=-1) +
      annotate(geom="text", label="Utility of treating,\nas if patient has disease", x=-Inf, y=-Inf, hjust=0, vjust=-1, color=colorDiseased, linespace=4) +
      
      annotate(geom="text", label="u(TN)", x=0, y=s$uTN, hjust=0, vjust=0, color=colorNondiseased) +
      annotate(geom="text", label="u(FP)", x=0, y=s$uFP, hjust=0, vjust=0, color=colorDiseased) +
      annotate(geom="text", label="u(FN)", x=1, y=s$uFN, hjust=1, vjust=0, color=colorNondiseased) +
      annotate(geom="text", label="u(TP)", x=1, y=s$uTP, hjust=1, vjust=0, color=colorDiseased) +
      
      scale_x_continuous(label=scales::percent) +
      scale_y_continuous(label=scales::percent) +
      coord_cartesian(xlim=c(1.02,-.02)) +
      #       coord_fixed(ratio=1, xlim=c(1.03,-.03), ylim=c(-.03,1.03)) +
      theme_bw() +
      labs(title="Treatment Threshold Probability", x="Probability Patient has the Disease", y="Expected Utility of Treatment")

    if( !is.na(ThresholdIntersectX()) ) {
      g  <- g + 
        annotate(geom="segment", x=ThresholdIntersectX(), y=ThresholdIntersectY(), xend=ThresholdIntersectX(), yend=0, size=4, alpha=.15, lineend="butt", color=colorNondiseased) +
        annotate(geom="segment", x=ThresholdIntersectX(), y=ThresholdIntersectY(), xend=ThresholdIntersectX(), yend=0, size=4, alpha=.15, lineend="butt", color=colorDiseased) +
        annotate(geom="text", label="Tx Threshold", x=ThresholdIntersectX(), y=-Inf, hjust=-.05, color="gray30", angle=90)
    }
    print(g)
  })
  
})