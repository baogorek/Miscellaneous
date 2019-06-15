# started with train_df from 
# https://gist.github.com/baogorek/6d682e42079005b3bde951e98ebae89e

library(dplyr)
library(nlme)

T <- 2000
train_df <- data.frame(day = 1:T, day_of_week = 0:(T-1) %% 7)                   
train_df$period <- ifelse(train_df$day <= 147, "build-up", "competition")       
train_df$w <- with(train_df, w <-                                               
  -24 * (day_of_week == 0) +                                                    
   12 * (day_of_week == 1) +                                                    
    8 * (day_of_week == 2) +                                                    
    0 * (day_of_week == 3) +                                                    
    6 * (day_of_week == 4) +                                                    
   -8 * (day_of_week == 5) +                                                    
    6 * (day_of_week == 6))                                                     

set.seed(1523)
train_df$w <- rpois(nrow(train_df),                                             
                    train_df$w + ifelse(train_df$period == "build-up", 34, 24)) 
                                                                                
exp_decay <- function(t, tau) {                                                 
  exp(-t / tau)                                                                 
}                                                                               
                                                                                
convolve_training <- function(training, n, tau) {                               
  sum(training[1:(n - 1)] * exp_decay((n - 1):1, tau))                          
}                                                                               
                                                                                
fitness <- sapply(1:nrow(train_df),                                             
                  function(n) convolve_training(train_df$w, n, 60))             
                                                                                
fatigue <- sapply(1:nrow(train_df),                                             
                  function(n) convolve_training(train_df$w, n, 13))             
                                                                                
E_perf <- 496 + .07 * fitness - .27 * fatigue
                  
train_df$perf <- E_perf + 4.0 * rnorm(nrow(train_df))

train_aug <- train_df %>%
  mutate(perf_lag1 = lag(perf, n = 1, order_by = day),
         perf_lag2 = lag(perf, n = 2, order_by = day),
         perf_lag3 = lag(perf, n = 3, order_by = day),
         train_lag1 = lag(w, n = 1, order_by = day),
         train_lag2 = lag(w, n = 2, order_by = day))


lag_regr <- lm(perf ~ perf_lag1 + perf_lag2 + train_lag1 + train_lag2,
               data = train_aug)
summary(lag_regr)

lag_regr <- gls(perf ~ perf_lag1 + perf_lag2 + train_lag1 + train_lag2,
                data = train_aug[5:nrow(train_aug), ],
                corARMA(form = ~day, p = 0, q = 2))
summary(lag_regr)


# Regression parameters:

mu <- 496
tau1 <- 1 # 60
tau2 <- .75 # 13
theta1 <- exp(-1 / tau1) 
theta2 <- exp(-1 / tau2)
k1 <- 1.3 
k2 <- 3.5 

## Perf lag 1
cat("Theoretical coefficient for intercept is\n",
    mu * (1 + theta1 + theta2 - theta1 * theta2), "\n")

## Perf lag 1
cat("Theoretical coefficient for performance lagged once is\n",
    theta1 + theta2, "\n")

## Perf lag 2
cat("Theoretical coefficient for performance lagged twice is\n",
    theta1 * theta2, "\n")

## Training lag 1
cat("Theoretical coefficient for training lagged once is\n",
    k1 * theta1 + k2 * theta2, "\n")

## Training lag 2
cat("Theoretical coefficient for training lagged twice is\n",
    theta1 * theta2 * (k2 - k1), "\n")





# sim
T <- 100
u <- rpois(T, lambda = 2)
prev_x <- 0
x <- c(prev_x)
for (t in 2:T) {
    newval <- .5 * prev_x + u[t - 1]
    x <- c(x, newval)
    prev_x <- newval
}

y <- 2 * x + rnorm(T)
sim_df <- data.frame(t = 1:T, x = x, y = y)

library(dplyr)
library(nlme)
sim_df <- sim_df %>%
  mutate(y_lag = lag(y, 1, order_by = t),
         u_lag = lag(u, 1, order_by = t)) %>%
  filter(complete.cases(.))

my_reg <- gls(y ~ y_lag + u_lag, data = sim_df,
                corARMA(value = -.5, form = ~t, p = 0, q = 1, TRUE))
summary(my_reg)


