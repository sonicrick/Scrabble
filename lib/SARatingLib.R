library(plyr)

calcSAExpWin <- function(myrating, opprating) {
  logit <- 172
  expWin <- 1 / (1 + exp((opprating - myrating)/logit))
  return(expWin)
}

calcSANewRating <- function(curRat, expWin, actWin) {
  bands <- c(-Inf, 1000, 1800, 2000, Inf)
  mult <- c(20, 20, 16, 10, 1)
  #to-do: check actual band for below 1000 i.e. first band
  
  return(curRat + (actWin-expWin)*mult[cut(curRat, breaks=bands, labels=FALSE)])
  #to-do: refractive multipliers where people cross new band halfway
  
  #to-do: iterative provisional calculation
}