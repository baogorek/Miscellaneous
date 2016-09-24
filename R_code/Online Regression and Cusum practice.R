### Online algorithm for linear regression via stochastic gradient descent ###

rm(list = ls())
N <- 100

x <- rnorm(N)
y <- 3 + 1.5*x + .7*rnorm(N)

#starting values
alpha <- 4
beta <- .9

rate <- 0.01

for(i in 1:3){
  u <- runif(N)
  for(j in 1:N){
    alpha <- alpha - 2*rate*(alpha + beta*x[order(u)[j]] - y[order(u)[j]] ) 
    beta <- beta - 2*rate*(alpha + beta*x[order(u)[j]] - y[order(u)[j]] )*x[order(u)[j]]
    
    cat("Iteration ", i, ", record ", j, ", alpha: ", alpha, ", beta: ", beta, "\n")
    
  }
}

### cumsum ###
rm(list = ls())
library(qcc)

## Two separate "regimes" producing data ###
regime.1 <- 15 + .5*rnorm(100)
regime.2 <- 16 + .5*rnorm(100) # Mean shift of +1

#Add outliers##
regime.1[sample(1:100, 10)] <-  15 + 4*rnorm(10)
regime.2[sample(1:100, 10)] <-  16 + 4*rnorm(10)

## Hide regime change and outliers ##
y <- c( regime.1, regime.2)

### The Individual Charts
# Regime 1 IR chart
qcc(y[1:100],  type="xbar.one")
# Regime 2 IR chart
qcc(y[100:200],  type="xbar.one")
# Full Series IR chart
qcc(y,  type="xbar.one")
# The run-rules tell you that something is going on, but where do you draw the line?
# This chart is good at catching our outliers, however!


## CUSUM needs true subgroups, so we will have to define windows ###
window <- trunc(c(0:199)/5) # Defining 40 five-day windows
y.groups <- qcc.groups(y, window)

## Imagine CUMSUM in a sequential setting ##

# Before Regime Shift:
q <- cusum(y.groups[1:20]) # Notice that cumsum has not picked up the outliers

#Begin Regime Shift 
cusum(y.groups[1:21])
cusum(y.groups[1:22])
cusum(y.groups[1:23]) # Has CUMSUM caught it yet?
cusum(y.groups[1:24])
cusum(y.groups[1:25]) # I bet it has by now!
cusum(y.groups[1:26])

cusum(y.groups)


