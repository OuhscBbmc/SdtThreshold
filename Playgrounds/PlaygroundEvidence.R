base::rm(list=base::ls(all=TRUE)) #Clear memory from previous runs.

baseRate <- 0.3
mu <- 35
sigma <- 20
mean <- 70
sd <- 19
degreeCount <- 100L

CalculateNondiseaseDistribution <- function( degree, baseRate, mu, sigma ) {
#   term1 <- 1 / (sigma * sqrt(2*pi))
#   term2 <- exp(-(degree - mu)^2 / (2*sigma^2))
#   term3 <- 1 - baseRate
#   return(term1 * term2 * term3)
  return( dnorm(x=degree, mean=mu, sd=sigma) * (1-baseRate))
}

CalculateDiseaseDistribution <- function( degree, baseRate, mean, sd ) {
#   term1 <- 1 / (sd * sqrt(2*pi))
#   term2 <- exp(-(degree - mean)^2 / (2*sd^2))
#   term3 <- baseRate
#   return(term1 * term2 * term3)
  return( dnorm(x=degree, mean=mean, sd=sd) * baseRate)
}
# CalculateCumulativeNondiseased <- function( degree, baseRate, mu, sigma ) {
#   return( pnorm(q=degree, mean=mu, sd=sigma) * (1-baseRate) )
# }
# CalculateCumulativeDiseased <- function( degree, baseRate, mu, sigma ) {
#   return( pnorm(q=degree, mean=mean, sd=sd) * (baseRate) )
# }

ds <- data.frame(Degree=0:100)
ds$NonDiseasedDistribution <- CalculateNondiseaseDistribution(degree=ds$Degree, baseRate,  mu, sigma)
ds$DiseasedDistribution <- CalculateDiseaseDistribution(degree=ds$Degree, baseRate,  mean, sd)
ds$CumulativeNondiseased <- cumsum(ds$NonDiseasedDistribution)
ds$CumulativeDiseased <- cumsum(ds$DiseasedDistribution)
ds$ComplementCumulativeNondiseased <- max(ds$CumulativeNondiseased) - ds$CumulativeNondiseased
ds$ComplementCumulativeDiseased <- max(ds$CumulativeDiseased) - ds$CumulativeDiseased