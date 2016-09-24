
# Base Weibull distribution 
 k <- 2
 lambda <- 5
# Concentration Parameter
 alpha <- .5

x.grid <- seq(0, 15)

W.CDF <- pweibull(x.grid, shape = k, scale=lambda)
plot(W.CDF~x.grid, type = "l")

### Unfortunately, rdirichlet is not provided in base R ###
Dir.draw <- function(parms){
  l <- length(parms)
  v <- rep(0, times = l)
  for(i in 1:l){
  v[i] <- rgamma(1, shape = 1, scale = parms[i])
  }
  return(v / sum(v))
}

## Generate a random CDF ##
CDF.draw <- function(base.CDF, alpha){
  l <- length(base.CDF)
  dir.draw <- Dir.draw(alpha * c(base.CDF[1], diff(base.CDF[1:(l-1)]), 1-base.CDF[l]) )
  cumsum(dir.draw)
}
 
lines(CDF.draw(W.CDF, .3)~x.grid, type = "l", lty = 2, col = "blue")
 

