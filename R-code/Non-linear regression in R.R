

# Based on the source: http://cran.r-project.org/doc/contrib/Fox-Companion/appendix-nonlinear-regression.pdf

# Prereqs: To get the data set USPop, make sure to 
# > set.internet2(TRUE)
# > install.packages("car")
# The nls() function comes with base R - no need to install the car package to use it in the future

library(car) # only to load the data set "USPop"

data(USPop)
attach(USPop)

plot(year, population) # not linear! More like a logistic growth curve from calculus class. 
                      # I.E., population(t)  = beta1/(1 + exp(beta2 + beta3*t))
                      # Think about: why is this "non-linear"?

time <- seq(from = 0, to = 21, by = 1) # setting the year 1790 to be "time 0"

pop.mod <- nls(  population ~ beta1/(1 + exp(beta2 + beta3*time)) ,
                  start = list(beta1 = 350, beta2 = 4.5, beta3 = -0.3) 
      # non-linear regression requires initial "guesses"; choosing them can be an art (see paper).
              )

summary(pop.mod) # Show the estimates

### Look at the fitted results ###
plot(year, population) 
lines(year, fitted.values(pop.mod), lwd=2, col = "blue") # pretty good!

# Residual analysis
plot(year, residuals(pop.mod), type="b") # Not exactly white noise - still some work to do!
abline(h=0, lty=2)


