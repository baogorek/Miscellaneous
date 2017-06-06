# Simulates random CDFs using the Dirichlet distribution 
# is this also the Dirichlet Process?
# If so, this example needs to be explicit about the approximation being made

k <- 2
lambda <- 5
alpha <- .5

x_grid <- seq(0, 15, .1)

w_cdf <- pweibull(x_grid, shape = k, scale = lambda)
plot(w_cdf ~ x_grid, type = "l", lwd = 3)

rdirichlet <- function(alpha){
  v <- rgamma(length(alpha), shape = 1, scale = alpha)
  v / sum(v)
}

#' Generate a random CDF
rCDF <- function(base_cdf, alpha) {
  l <- length(base_cdf)
  bucket_probs <- c(base_cdf[1], diff(base_cdf[1:(l - 1)]), 1 - base_cdf[l])
  x <- rdirichlet(alpha *  bucket_probs)
  cumsum(x)
}

for (i in 1:10) {
  cdf_draw <- rCDF(w_cdf, .3)
  lines(cdf_draw ~ x_grid, type = "l", lty = 2, col = "blue")
}
