library(dplyr)

# Best paper so far:

#https://www.researchgate.net/profile/Robin_Candau/publication/15242395_Fatigue_and_fitness_modelled_from_the_effects_of_training_on_performance/links/55720f2608ae7536374cdc09/Fatigue-and-fitness-modelled-from-the-effects-of-training-on-performance.pdf

# Article describing the Cp
# https://www.researchgate.net/publication/20910238_Modeling_human_performance_in_running

# Facts about this hammer-thrower:
# 1. Up to day 147, training alternated between intensive and reduced, w_bar = 34
# 2. Up to day 259, training was reduced to prepare for competition, w_bar = 24

train_df <- data.frame(day = 1:259, day_of_week = 0:258 %% 7)
train_df$w <- with(train_df, w <-
    -24 * (day_of_week == 0) +
   12 * (day_of_week == 1) +
   8  * (day_of_week == 2) + 
    0 * (day_of_week == 3) +
    6 * (day_of_week == 4) +
    -8 * (day_of_week == 5) +
    6 * (day_of_week == 6))

train_df$w <- train_df$w + ifelse(train_df$day <= 147, 34, 24)

# To get the mean's right at 34 and 24, during first period, athlete
# Almost rests on Sunday. During second period, complete rest on Sunday

plot(train_df$w ~ train_df$day)

exp_decay <- function(t, tau) {
  exp(-t / tau)
}

s <- 5
# Go from 1 to 4
# f(n-1), f(n-2), f(n-3), f(1)
training <- train_df$w

convolve_training <- function(training, n, tau) {
  sum(training[1:(n - 1)] * exp_decay((n - 1):1, tau))
}


fitness <- sapply(1:nrow(train_df),
                  function(n) convolve_training(train_df$w, n, 60)) 

fatigue <- sapply(1:nrow(train_df),
                  function(n) convolve_training(train_df$w, n, 13)) 

perf <- 496 + .069 * fitness - .27 * fatigue + 2 * rnorm(nrow(train_df))


plot(perf ~ train_df$day)
plot(fitness)
plot(fatigue ~ train_df$day, col = "blue", main = "fatigue")
lines(train_df$w ~ train_df$day, type = "b", cex = .5, col = "green")

# Recover parameters using non-linear regression

rss <- function(theta) {
  int <- theta[1]
  k1 <- theta[2] # fitness
  k2 <- theta[3]
  tau1 <- theta[4] # fitness
  tau2 <- theta[5]

  fitness <- sapply(1:nrow(train_df),
                    function(n) convolve_training(train_df$w, n, tau1)) 

  fatigue <- sapply(1:nrow(train_df),
                   function(n) convolve_training(train_df$w, n, tau2)) 

  perf_hat <- int + k1 * fitness - k2 * fatigue
  return(sum((perf - perf_hat) ^ 2))
}

 
optim_results <- optim(c(400, .05, .15, 20, 5), rss, method = "BFGS",
                       hessian = TRUE, control = list(maxit = 1000))
print(optim_results)
sqrt(diag(solve(optim_results$hessian)))


# Implement Spline Regression approach

# The ultimate convolving function for the training data is:
combined_fn <- function(t) {
  0.069 * exp_decay(t, 60) - 0.27 * exp_decay(t, 13)
}

plot(combined_fn(1:259) ~ c(1:259))

# First, a check to make sure convolving with this one fn acutally works
convolve_training2 <- function(training, n) {
  sum(training[1:(n - 1)] * combined_fn((n - 1):1))
}


total <- sapply(1:nrow(train_df),
                function(n) convolve_training2(train_df$w, n)) 

plot(496 + total ~ train_df$day) # yep, it works

# Next step is to "cheat" and get a set of knots that will lead to good approx
library(splines)
my_spline <- ns(1:259, Boundary.knots = c(1, 259), knots = c(14, 40, 100))
output <- combined_fn(1:259)

my_lm <- lm(output ~ my_spline)
plot(output ~ c(1:259))
lines(predict(my_lm) ~ c(1:259), col = "blue") # Not bad!






