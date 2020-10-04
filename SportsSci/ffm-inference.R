library(dplyr)

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

# Time-stable Kalman Filter matrices -----------------------------------------
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

# Setting up structures to be predicted ----------------------------------
log_likelihood <- numeric(T)
log_likelihood[1] <- 0  # First is a prior only

# Posterior mean and variance of state given all data up to time t := row
mu <- matrix(rep(NA, 2 * T), ncol=2)  # I.e., fitness and fatigue
Sigma <- matrix(rep(NA, 4 * T), ncol=4) # Rows contain vectorized Sigma_t

# prior distribution of fitness and fatigue
mu[1, ] <- c(500, 200)
Sigma[1, ] <- c(25 ^ 2, 0, 0, 25 ^ 2)

# State Prior to Likelihood to State Posterior - the Kalman updating equations
for (t in 2:T) {
  y_t <- training_df$perf[t]
  Sigma_lag1 <- matrix(Sigma[t - 1, ], nrow=2)

  # Prior mean and variance of state
  systematic_t <- u_mat[t - 1, ] + T_mat %*% mu[t - 1, ]
  P_t <- T_mat %*% Sigma_lag1 %*% t(T_mat) + Q_mat

  # Likelihood of performance given systematic state update
  log_likelihood[t] <- dnorm(y_t, p_0 + H_mat %*% systematic_t, R_mat, log=TRUE)
  e_t <- y_t - (p_0 + H_mat %*% systematic_t)

  # Posterior of state (fitness and fatigue) given all data up to time t
  S_t <- H_mat %*% P_t %*% t(H_mat) + R_mat
  S_t_inv <- solve(S_t)  # Computing inverses is discouraged, but here it's 1D
  mu[t, ] <- (systematic_t + P_t %*% t(H_mat) %*% S_t_inv %*% e_t)
  Sigma[t, ] <- as.vector(P_t - P_t %*% t(H_mat) %*% S_t_inv %*% H_mat %*% P_t)
}

perf_hat <- p_0 + mu %*% t(H_mat)  # Filtered predictions of performance

plot(training_df$perf, main="Filtered predictions vs actuals")
points(perf_hat, col='blue', type = 'b')

plot(mu[, 1], col='blue', type='b', ylim=c(0, 3000),
     main="Fitness (blue) and Fatigue (red)")
points(mu[, 2], col='red', type='b')

