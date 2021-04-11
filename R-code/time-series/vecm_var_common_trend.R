# Working through slides: http://statmath.wu.ac.at/~hauser/LVs/FinEtricsQF/FEtrics_Chp4.pdf

library(mvtnorm)
library(dplyr)

# The Unstable VAR(1) from slide 17
## Question: Don't I need a VAR(2) for an ECM model?

N <- 200

Sigma_epsilon <- matrix(c(1, 0, 0, 1), ncol=2)
epsilon <- rmvnorm(N, mean = c(0, 0), sigma = Sigma_epsilon)

Pi <- matrix(c(.5, -1, -.25, .5), ncol=2, byrow=TRUE)

X <- matrix(NA, nrow=N, ncol=2)
X[1, ] <- 0

for (t in 2:N) {
  x_lag1 <- X[t - 1, ]
  x <- Pi %*% x_lag1 + epsilon[t - 1, ]
  X[t, ] <- t(x)
}

plot(X[, 1], ylim = c(-30, 30))
lines(X[, 2])

Lambda <- diag(eigen(Pi)$values)
L <- solve(eigen(Pi)$vectors)

# The "common trend" equation on slide 18
L %*% Pi %*% solve(L)
Lambda

## new variables (coordinates) y_t and eta_t
### L %*% x for every x is X %*% t(L) in matrix form
### This is slide 19
Y <- X %*% t(L)
plot(Y[, 1])  # The common trend
plot(Y[, 2])

## Slide 20
### solve(L) %*% y = x
InvLLambda = solve(L) %*% Lambda  # Note that these only load on y1

loading1 <- InvLLambda[1, 1]
loading2 <- InvLLambda[2, 1]

x1_recover = Y[, 1] * loading1 + epsilon[, 1]
x2_recover = Y[, 1] * loading2 + epsilon[, 2]

x1_recover %>% head()
x2_recover %>% head()

X %>% head()

## Slide 21 - The cointegrating relation
### Now that we have x1 and x2 as a function of y1 alone,
### create a linear combination such that y1 cancels out

z <- X[, 1] - (loading1 / loading2) * X[, 2]
plot(z)

## Cointegrating vector is 1, beta where beta =
- (loading1 / loading2)




### For later:
#### QR decomposition to find the orthogonal complement matrix:
# https://stat.ethz.ch/pipermail/r-help/2004-November/060131.html
A<-matrix(rnorm(40),10,4)
B <- t(qr.Q(qr(A),complete=TRUE)[,5:10])
B%*%A
