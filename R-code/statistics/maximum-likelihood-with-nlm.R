#' Maximum likelhood exercise
#'
#' Find better estimates of the shape and scale parameter using the principle of
#' maximum likelihood.

y <- rgamma(1000, shape = 2.1, scale = 3.0)
hist(y)

x <- seq(0, 25, .01)
density_x <- dgamma(x, shape = 2.7, scale = 3.4)  

hist(y, freq = FALSE, main = "Not the best fit")
lines(density_x ~ x, col = "blue")  

f <- function(params) {
  -1 * sum(dgamma(y, shape = params[1], scale = params[2], log = TRUE))
}

results <- nlm(f, c(1, 1))
density_x <- dgamma(x, shape = results$estimate[1], scale = results$estimate[2])  
hist(y, freq = FALSE, main = "The ML fit")
lines(density_x ~ x, col = "blue")  
