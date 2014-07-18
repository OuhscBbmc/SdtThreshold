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
ds$NonDiseasedDistribution <- CalculateNondiseasePdf(score=ds$Score,  muN, sigmaN, baseRate=NA)
ds$DiseasedDistribution <- CalculateDiseasePdf(score=ds$Score, muD, sigmaD, baseRate=NA)
ds$CumulativeNondiseased <- CalculateNondiseaseCdf(score=ds$Score,  muN, sigmaN, baseRate=NA)
ds$CumulativeDiseased <- CalculateDiseaseCdf(score=ds$Score, muD, sigmaD, baseRate=NA)
ds$ComplementCumulativeNondiseased <- max(ds$CumulativeNondiseased) - ds$CumulativeNondiseased
ds$ComplementCumulativeDiseased <- max(ds$CumulativeDiseased) - ds$CumulativeDiseased