# Open questions -> How do non-linearities affect things?
# How does model selection come into play?
# How do Horvitz Thompson, the Bayesian Approach, and propensity scoring compare?

rm(list = ls())
N <- 1500000

# Known regressors #
x1 <- 2 + rnorm(N)
x2 <- -3 + rnorm(N)

# Unseen regressor affecting the Propensity to Treat#
w <- .25*x1 - .35*x2 + .5*rnorm(N) # More unseen information in w can be added by increasing rnorm coefficient
r <- 6 + rnorm(N) # Instrument: involved in selection but not main equation 

# Treatment #
trt.propensity <- -2.1 + 1.3*w + .9*rnorm(N) # Error term must be multivariate normal with main equation. Make sure there are enough Ts!
T <- rbinom(N,1, pnorm(trt.propensity)) # pnorm is like the sigmoid function, but for probit
table(T)

# true model #
y <- .2*x1 - .25*x2  + .1*T + -.6*w +  .7*rnorm(N) # I can adjust the estimated treatment effect by adjusting the wieght of the confounding covariate w

y.lm <- lm(y ~ x1 + x2 + T)
summary(y.lm)

# The probit regression #
T.probit <- glm(T ~ x1 + x2, family = binomial(link = "probit"))
summary(T.probit)

z <- T.probit$linear.predictor

Heckmans.lambda <-  ifelse(T==1, -dnorm(z)/pnorm(z), dnorm(z)/(1-pnorm(z)) )

# Regression with Heckman's correction #
y.lm.Heckman <- lm(y ~ x1 + x2 +   T + Heckmans.lambda) # We're  playing with the new error term that is created?
summary(y.lm.Heckman)

# My Summary: it still seems to work somewhat...at least the sign flips in the right direction
# and the instrument, but takes much more data now #
