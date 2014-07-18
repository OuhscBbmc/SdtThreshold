base::rm(list=base::ls(all=TRUE)) #Clear memory from previous runs.
require(ggplot2)


baseRate <- 0.3
muN <- 35 #'N' stands for 'Nondiseased'
sigmaN <- 15
muD <- 70 #'D' stands for 'Diseased'
sigmaD <- 20
measurementRange <- c(0, 100)
colorNondiseased <- "blue3"
colorDiseased <- "red3"

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

FxToMinimize <- function( x ) {
  difference <- (CalculateNondiseasePdf(score=x,  muN, sigmaN, baseRate=NA) - CalculateDiseasePdf(score=x, muD, sigmaD, baseRate=NA))
  return( difference )
}
searchRange <- range(c(muD + c(1,-1)*sigmaD, muN + c(1,-1)*sigmaD))

# FxToMinimize(54)
# intersectX <- optimize(f=FxToMinimize, interval=c(30, 80) )$minimum
intersectX <- uniroot(f=FxToMinimize, interval=searchRange)$root
intersectY <- CalculateNondiseasePdf(score=intersectX,  muN, sigmaN, baseRate=NA)
peakN <- CalculateNondiseasePdf(score=muN,  muN, sigmaN, baseRate=NA)
peakD <- CalculateDiseasePdf(score=muD,  muD, sigmaD, baseRate=NA)

ggplot(ds, aes(x=ds$Score)) +
  geom_line(aes(y=NondiseasedPdf), color=colorNondiseased, size=4, alpha=.5) +
  geom_line(aes(y=DiseasedPdf), color=colorDiseased, size=4, alpha=.5) +
  annotate(geom="segment", x=intersectX, y=intersectY, xend=intersectX, yend=0, size=4, alpha=.2, lineend="butt", color=colorNondiseased) +
  annotate(geom="segment", x=intersectX, y=intersectY, xend=intersectX, yend=0, size=4, alpha=.2, lineend="butt", color=colorDiseased) +
  annotate(geom="text", label="Nondiseased", x=muN, y=peakN, vjust=-.5, color=colorNondiseased) +
  annotate(geom="text", label="Diseased", x=muD, y=peakD, vjust=-.5, color=colorDiseased) +
  annotate(geom="text", label="Cutoff", x=intersectX, y=0, hjust=-.05, color="gray30", angle=90) +
  theme_bw()
