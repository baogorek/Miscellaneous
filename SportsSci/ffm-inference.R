library(dplyr)
library(KFAS)

source('helper-functions.R')
source('sim.R')


# Simulating data --------------------------------------------------------------------
load_spec <- list(list(start=3, end = 5, start_level=10, end_level=20, noise_sd = 0),
                  list(start=30, end = 100, start_level=50, end_level=50, noise_sd = 1))

training_df <- simulate_ffm(T=200, load_spec=load_spec,
                            p_0=496, k_g=.07, k_h=.27, tau_g=60, tau_h=13,
                            sigma_e=10)


# Fitting with R's optim and RSS loss ------------------------------------------------
rss <- function(theta) {
  p_0  <- theta[1] # performance baseline
  k_g   <- theta[2] # fitness weight
  k_h   <- theta[3] # fatigue weight
  tau_g <- theta[4] # fitness decay
  tau_h <- theta[5] # fatigue decay

  sum((training_df$perf - get_E_perf(training_df$w, p_0, k_g, k_h, tau_g, tau_h)) ^ 2)
}

optim_results <- optim(c(400, .05, .15, 20, 5), rss, method = "BFGS",
                       hessian = TRUE, control = list(maxit = 1000))

(theta_hat <- optim_results$par)

training_df$perf_hat <- do.call(get_E_perf, append(list(w=training_df$w), as.list(theta_hat)))

plot(perf ~ t, data = training_df)
points(perf_hat ~ t, data = training_df, type = 'b', col = 'blue')

# Fitting with Kalman Filter ------------------------------------------------
T <- nrow(training_df)

# Starting values - First, setting them to the right values
p_0 <- 496
k_g <- .07
k_h <- .27
tau_g <- 60
tau_h <- 13 
sigma_e <- 10

# TODO: think about how to deal with the time index
# You might want to create a set of numeric vectors out of the gate

# prior distribution of fitness and fatigue
mu_t <- c(500, 200)
Sigma_t <- matrix(c(100 ^2, 0, 0, 50 ^2), ncol=2) # need a way to think about this prior

# Transition matrix
T_mat <- matrix(c(exp(-1 / tau_g), 0, 0, exp(-1 / tau_h)), ncol=2)

# State intercept - each row is the fitness and fatigue effect to the next day's workout
u_mat <- matrix(rep(c(exp(-1 / tau_g), exp(-1 / tau_h)), T), ncol=2,
		byrow=TRUE) * training_df$w  # element-wise multiplication

# Measurement matrix
H_mat <- matrix(c(k_g, -k_h), ncol=2)

# Variance matrices
Q_mat <- matrix(c(0, 0, 0, 0), ncol=2) #  0-state error for starters
R_mat <- matrix(c(sigma_e ^ 2), ncol=1) #  0-state error for starters

# Time t=2 is where things get started
P_t <- T_mat %*% Sigma_t %*% t(T_mat) + Q_mat
S_t <- H_mat %*% P_t %*% t(H_mat) + R_mat

y_t <- training_df$perf[2]
systematic <- u_mat[1, ] + T_mat %*% mu_t
mu_t <- systematic + P_t %*% H_mat %*% solve(S_t) %*% (y_t - H_mat %*% systematic)  # TODO: something is wrong
Sigma_t <- P_t - P_t %*% t(H_mat) %*% solve(S_t) %*% H_mat %*% P_t


ffm <- SSModel(perf ~ 1 + SSMtrend(degree=1, Q=0), data = training_df)

