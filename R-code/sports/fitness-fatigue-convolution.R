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
  sum(training[1:(n - 1)] * exp_decay((n - 1):1, tau) 
}


fitness <- sapply(1:nrow(train_df),
                  function(n) convolve_training(train_df$w, n, 60)) 

fatigue <- sapply(1:nrow(train_df),
                  function(n) convolve_training(train_df$w, n, 13)) 

perf <- 496 + .069 * fitness - .27 * fatigue


plot(perf ~ train_df$day)
plot(fitness)
plot(fatigue ~ train_df$day, col = "blue", main = "fatigue")
lines(train_df$w ~ train_df$day, type = "b", cex = .5, col = "green")


