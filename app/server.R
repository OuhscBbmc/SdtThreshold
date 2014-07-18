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
# pathInput <- "../../Data/Derived/BlaBla.rds"

#######################################
### Declare globals
measurementRange <- c(0, 100)
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
      sigmaD = input$sigmaD
    )
  })
  ds <- reactive({
    d <- data.frame(Score=seq(from=measurementRange[1], to=measurementRange[2], by=1))
    d$NondiseasedPdf <- CalculateNondiseasePdf(score=d$Score,  userInputs()$muN, userInputs()$sigmaN, baseRate=NA)
    d$DiseasedPdf <- CalculateDiseasePdf(score=d$Score, userInputs()$muD, userInputs()$sigmaD, baseRate=NA)
    d$NondiseasedCdfL <- CalculateNondiseaseCdf(score=d$Score,  userInputs()$muN, userInputs()$sigmaN, baseRate=NA) #Specificity; the 'L' stands for "left" of the point.
    d$DiseasedCdfL <- CalculateDiseaseCdf(score=d$Score, userInputs()$muD, userInputs()$sigmaD, baseRate=NA)
    d$NondiseasedCdfR <- 1- d$NondiseasedCdf #max(d$NondiseasedCdf) - d$NondiseasedCdf
    d$DiseasedCdfR <- 1 - d$DiseasedCdf #max(d$DiseasedCdf) - d$DiseasedCdf #Sensitivity; the 'R' stands for "right" of the point.
    d$LRInstant <- d$DiseasedPdf / d$NondiseasedPdf
    d$LRPlus <- (d$DiseasedCdfR)  / (d$NondiseasedCdfR)
    d$LRMinus <- (d$DiseasedCdfL)  / (d$NondiseasedCdfL)
    d$LRRatio <- d$LRPlus / d$LRMinus
    return( d )
  })
  
  FxToMinimize <- function( x ) {
    difference <- CalculateNondiseasePdf(score=x,  userInputs()$muN, userInputs()$sigmaN, baseRate=NA) - 
                  CalculateDiseasePdf(score=x, userInputs()$muD, userInputs()$sigmaD, baseRate=NA)
    return( difference )
  }
  
  intersectX <- reactive({  
    # searchRange <- c(0, 100) 
    searchRange <- range(c(userInputs()$muD + c(1,-1)*userInputs()$sigmaD, userInputs()$muN + c(1,-1)*userInputs()$sigmaD))
    # message(searchRange)
    return( uniroot(f=FxToMinimize, interval=searchRange )$root )    
  })
  intersectY <- reactive({
    CalculateNondiseasePdf(score=intersectX(),  userInputs()$muN, userInputs()$sigmaN, baseRate=NA)
  })
  peakN <- reactive({
    CalculateNondiseasePdf(score=userInputs()$muN,  userInputs()$muN, userInputs()$sigmaN, baseRate=NA)
  })
  peakD <- reactive({
    CalculateDiseasePdf(score=userInputs()$muD,  userInputs()$muD, userInputs()$sigmaD, baseRate=NA)
  })
  
  output$plotPdf <- renderPlot({
    d <- ds()
    g <- ggplot(d, aes(x=ds$Score)) +
      geom_line(aes(y=NondiseasedPdf), color=colorNondiseased, size=4, alpha=.5) +
      geom_line(aes(y=DiseasedPdf), color=colorDiseased, size=4, alpha=.5) +
      annotate(geom="segment", x=intersectX(), y=intersectY(), xend=intersectX(), yend=0, size=4, alpha=.2, lineend="butt", color=colorNondiseased) +
      annotate(geom="segment", x=intersectX(), y=intersectY(), xend=intersectX(), yend=0, size=4, alpha=.2, lineend="butt", color=colorDiseased) +
      annotate(geom="text", label="Nondiseased", x=userInputs()$muN, y=peakN(), vjust=-.5, color=colorNondiseased) +
      annotate(geom="text", label="Diseased", x=userInputs()$muD, y=peakD(), vjust=-.5, color=colorDiseased) +
      annotate(geom="text", label="Cutoff", x=intersectX(), y=0, hjust=-.05, color="gray30", angle=90) +
      theme_bw()
    print(g)
  })
  
})