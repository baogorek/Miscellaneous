# https://github.com/baogorek/Miscellaneous/blob/master/R-code/sports/fitness-fatigue-convolution.ipynb
# docker run -it --entrypoint=bash -v /mnt/c/devl/Miscellaneous/R-code/sports/collab:/home/rstudio kosugitti/rstan-env
# docker run --rm -p 8787:8787 -v /mnt/c/devl/Miscellaneous/R-code/sports/collab:/home/rstudio -e DISABLE_AUTH=TRUE kosugitti/rstan-env

library(rstan)
library(dplyr)

# Simulating data -----------------------------------------
train_df <- data.frame(day = 1:259, day_of_week = 0:258 %% 7)                   
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

set.seed(45345)
train_df$perf <- E_perf + 7.0 * rnorm(nrow(train_df))

# Bayesian Inference with Stan ---------------------------------
stan_data <- list(T = nrow(train_df), p = train_df$perf, w = train_df$w)

# TODO: think about the initial condition
fit <- stan('ffm.stan', data = stan_data, chains = 4, iter = 3000, warmup=2000, seed=2343)

# Diagnostics
pars <- names(a)[1:6]
pdf('plots/stanplot3.pdf')
plot(fit, pars  = pars, show_density=TRUE)
plot(fit, plotfun = "hist", pars = pars, include = FALSE)
plot(fit, plotfun = "trace", pars = pars, inc_warmup = TRUE)
plot(fit, plotfun = "rhat")
dev.off()

check_hmc_diagnostics(fit)
check_divergences(fit)
check_treedepth(fit)
check_energy(fit)

a <- extract(fit)
a

mean(a$p0)
mean(a$k_fitness)
mean(a$k_fatigue)
mean(a$tau_fitness)
mean(a$tau_fatigue)

png('plots/one.png')
par(mfrow=c(2, 1))
hist(rnorm(1000, 60, 10), main = 'samples from prior of tau_fitness')
hist(a$tau_fitness, main = 'samples from posterior of tau_fitness')
dev.off()


# Fitting with R's optim and RSS loss ----------------------

get_performance <- function(theta) {
  # requires train df in the global env - not clean
  int  <- theta[1] # performance baseline
  k1   <- theta[2] # fitness weight
  k2   <- theta[3] # fatigue weight
  tau1 <- theta[4] # fitness decay
  tau2 <- theta[5] # fatigue decay

  fitness <- sapply(1:nrow(train_df),
                    function(n) convolve_training(train_df$w, n, tau1))

  fatigue <- sapply(1:nrow(train_df),
                    function(n) convolve_training(train_df$w, n, tau2))

  int + k1 * fitness - k2 * fatigue
}
                    

rss <- function(theta) {
  sum((train_df$perf - get_performance(theta)) ^ 2)
}

optim_results <- optim(c(400, .05, .15, 20, 5), rss, method = "BFGS",
                       hessian = TRUE, control = list(maxit = 1000))

theta_hat <- optim_results$par
train_df$perf_hat <- get_performance(theta_hat)

png('plots/one.png')
plot(perf ~ day, data = train_df)
points(perf_hat ~ day, data = train_df, type = 'b', col = 'blue')
dev.off()


