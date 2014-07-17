base::rm(list=base::ls(all=TRUE)) #Clear memory from previous runs.

mu <- 35
sigma <- 20
mean <- 70
sd <- 19
degreeCount <- 100L


CalculateNonDiseaseDistribution <- function( degree, mu, sigma ) {
#   term1 <- 1 / (sigma * sqrt(2*pi))
#   term2 <- exp(-(degree - mean)^2 / (2*sigma^2))
#   return( term1 * term2 )
}

CalculateNonDiseaseDistribution( 2, mu, sigma)