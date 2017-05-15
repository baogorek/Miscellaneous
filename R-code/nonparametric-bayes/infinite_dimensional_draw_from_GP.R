rm(list = ls())
######### Drawing a Random Function from a GAUSSIAN PROCESS ##########################################
# Randomly drawing from the class of functions f:[-5,5]->R with properties
# (1) E(f(x)) = 0 for x in [-5,5]
# (2) Var(f(x)) = 1 for x in [-5,5]
# (3) Cov(f(x), f(x`)) = exp(-.5*(x-x`)^2) 
# Note that (1) and (2) are simply pointwise (marginal) requirements - (3) is where it gets interesting
#######################################################################################################

# Though we're in an infinite dimensional space, we can only ever make finite queries #
x <- seq(from = -5, to = 5, by = .5)
# But we are free to make ANY finite dimensional query of our functional input, 
 # not just taking necessarily evenly-spaced samples as in time-series #
n <- length(x)

# The step below will take care of (1) and (2), (but not (3)!) with the following #
z <- matrix(rnorm(n), ncol = 1) # a row vector containing independent, normally distributed elements, with mean 0 and var 1
#We're not finished, but pretend that are:
plot(z~x, type = "l", col = "red", main = "Pointwise Criterion satisfied, but zero correlation")

# Construct the desired covariance matrix, K:
K <- matrix(rep(0, times = n^2), nrow=n)
### Inefficient, I know: ###
  for(row in 1:n){
    for(col in 1:n){
      K[row,col] <- exp(-.5* (x[row]-x[col])^2 )
    }
  }
      
# Want to "see" K?
image(K)
head(K) # Note how high the correlations are when the x-coordinates are nearby

# If I want y ~ N(0, SIGMA), then I need the "matrix square root" of SIGMA to transform standard normal vector z # #
root.K <- t(chol(K ))
all.equal(K, root.K %*% t(root.K) ) # before, I said you can make any finite query you want. Well,
# I kind of lied. You need to be able to perform this operation, without numerical problems 
# This is a very ill-conditioned matrix! And there are better ways to do this.

### If I did this right, f.x should be Multivariate Normal with a covariance matrix of K ###
f.x <- root.K %*% z       
plot(f.x ~x, type = "l", col = "blue", main = "This is a random draw from an \n infinite dimensional function space!")   
    


