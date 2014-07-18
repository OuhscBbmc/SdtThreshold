base::rm(list=base::ls(all=TRUE)) #Clear memory from previous runs.

baseRate <- 0.3
muN <- 35 #'N' stands for 'Nondiseased'
sigmaN <- 10
muD <- 70 #'D' stands for 'Diseased'
sigmaD <- 7
measurementRange <- c(0, 100)

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

ds <- data.frame(Score=seq(from=measurementRange[1], to=measurementRange[2], by=1))
ds$NondiseasedPdf <- CalculateNondiseasePdf(score=ds$Score,  muN, sigmaN, baseRate=NA)
ds$DiseasedPdf <- CalculateDiseasePdf(score=ds$Score, muD, sigmaD, baseRate=NA)
ds$NondiseasedCdfL <- CalculateNondiseaseCdf(score=ds$Score,  muN, sigmaN, baseRate=NA) #Specificity; the 'L' stands for "left" of the point.
ds$DiseasedCdfL <- CalculateDiseaseCdf(score=ds$Score, muD, sigmaD, baseRate=NA)
ds$NondiseasedCdfR <- 1- ds$NondiseasedCdf #max(ds$NondiseasedCdf) - ds$NondiseasedCdf
ds$DiseasedCdfR <- 1 - ds$DiseasedCdf #max(ds$DiseasedCdf) - ds$DiseasedCdf #Sensitivity; the 'R' stands for "right" of the point.
# ds$Line <- NA_real_
# ds$Constant <- NA_real_
ds$LRInstant <- ds$DiseasedPdf / ds$NondiseasedPdf
ds$LRPlus <- (ds$DiseasedCdfR)  / (ds$NondiseasedCdfR)
ds$LRMinus <- (ds$DiseasedCdfL)  / (ds$NondiseasedCdfL)
ds$LRRatio <- ds$LRPlus / ds$LRMinus
