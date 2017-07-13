# The dynamics of the price of a stock in the Black Scholes options pricing model

delta <- .01
t <- seq(0, 1, by = delta)
T <- length(t)
x <- rep(1, times = T)

for(i in 2:T){
  x[i] <- x[i - 1] + .1 * x[i - 1] * delta + .5 * x[i - 1] * rnorm(1, 0, .01)
}

plot(x ~ t, type = "l")


### A linear feedback model, with goal G ###

delta <- .01
t <- seq(0,2, by = delta)
T <- length(t)
x <- rep(0, times = T)

G <- 10
r = 2.5
x[0] <- 5

for(i in 2:T){
  x[i] <- x[i - 1] + r * (G - x[i - 1]) * delta + x[i - 1] * rnorm(1, 0, .01)
}

 plot(x~t, type = "l")
