library(dplyr)

source('helper-functions.R')
source('sim.R')


# Simulating data --------------------------------------------------------------------
load_spec <- list(list(start=3, end = 5, start_level=10, end_level=20, noise_sd = 0),
                  list(start=30, end = 100, start_level=50, end_level=50, noise_sd = 1))

training_df <- simulate_ffm(T=200, load_spec=load_spec,
                            p_0=496, k_g=.07, k_h=.27, tau_g=60, tau_h=13,
                            sigma_e=10, seed=255)

# Fitting with R's optim and RSS loss ------------------------------------------------
rss <- function(theta) {
  p_0  <- theta[1] # performance baseline
  k_g   <- theta[2] # fitness weight
  k_h   <- theta[3] # fatigue weight
  tau_g <- theta[4] # fitness decay
  tau_h <- theta[5] # fatigue decay

  sum((training_df$perf - get_E_perf(training_df$w, p_0, k_g, k_h, tau_g, tau_h)) ^ 2)
}

optim_results <- optim(c(400, .05, .15, 20, sqrt(35)), rss, method = "BFGS",
                       hessian = TRUE, control = list(maxit = 1000))

(theta_hat <- optim_results$par)

training_df$perf_hat <- do.call(get_E_perf, append(list(w=training_df$w), as.list(theta_hat)))

plot(perf ~ t, data = training_df)
points(perf_hat ~ t, data = training_df, type = 'b', col = 'blue')

# Fitting with Kalman Filter ------------------------------------------------

do_ffm_kalman <- function(training_df, p_0, k_g, k_h, tau_g, tau_h, sigma_e,
			  prior_mean_fitness=0, prior_mean_fatigue=0,
			  prior_sd_fitness=25, prior_sd_fatigue=25,
			  prior_ff_corr=0) {

  if (FALSE) {
    # for interactive running
    p_0 <- 400
    k_g <- .05
    k_h <- .15
    tau_g <- 20
    tau_h <- 5 
    sigma_e <- sqrt(35) 
    prior_mean_fitness <- 0
    prior_mean_fatigue <- 0
    prior_sd_fitness <- 1 
    prior_sd_fatigue <- 1 
    prior_ff_corr <- 0
  }

  # Length of the time series
  T <- nrow(training_df)

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
  R_mat <- matrix(c(sigma_e ^ 2), ncol=1) # 1x1 matrix with meas.err. variance
  
  # Setting up structures to be predicted ----------------------------------
  log_likelihood <- numeric(T)
  
  # Posterior mean and variance of state given all data up to time t := row
  mu <- matrix(rep(NA, 2 * T), ncol=2)  # I.e., fitness and fatigue
  Sigma <- matrix(rep(NA, 4 * T), ncol=4) # Rows contain vectorized Sigma_t
  
  # prior distribution of fitness and fatigue
  #TODO: I'm missing an opportunity to filter. mu[1, ] should be the filtered value, not the prior
  mu[1, ] <- c(prior_mean_fitness, prior_mean_fatigue)
  Sigma[1, ] <- c(prior_sd_fitness ^ 2,
		  rep(prior_ff_corr * prior_sd_fitness * prior_sd_fatigue, 2),
                  prior_sd_fatigue ^ 2) 
 
  # Initial log likelihood value
  log_likelihood[1] <- dnorm(training_df$perf[1],
			     mean = p_0 + H_mat %*% mu[1, ],
			     sqrt(H_mat %*%
				  solve(matrix(Sigma[1, ], ncol=2)) %*%
				  t(H_mat) + R_mat[1, 1]), log=TRUE)

  # State Prior to Likelihood to State Posterior - the Kalman updating equations
  for (t in 2:T) {
    y_t <- training_df$perf[t]
    Sigma_lag1 <- matrix(Sigma[t - 1, ], nrow=2)
  
    # Prior mean and variance of state
    systematic_t <- u_mat[t - 1, ] + T_mat %*% mu[t - 1, ]
    P_t <- T_mat %*% Sigma_lag1 %*% t(T_mat) + Q_mat
  
    # Likelihood of performance given systematic state update
    log_likelihood[t] <- dnorm(y_t, mean=p_0 + H_mat %*% systematic_t,
			       sd = sqrt(H_mat %*% solve(Sigma_lag1) %*%
				         t(H_mat) + R_mat[1, 1]), log=TRUE)
    e_t <- y_t - (p_0 + H_mat %*% systematic_t)
  
    # Posterior of state (fitness and fatigue) given all data up to time t
    S_t <- H_mat %*% P_t %*% t(H_mat) + R_mat
    S_t_inv <- 1 / S_t  # solve(S_t) more generally, but it's 1-D
    mu[t, ] <- (systematic_t + P_t %*% t(H_mat) %*% S_t_inv %*% e_t)
    Sigma[t, ] <- as.vector(P_t - P_t %*% t(H_mat) %*% S_t_inv %*% H_mat %*% P_t)


   dnorm(y_t, mean=p_0 + H_mat %*% mu[t, ],
			       sd = sqrt(H_mat %*% solve(matrix(Sigma[t, ], ncol=2)) %*%
				         t(H_mat) + R_mat[1, 1]), log=TRUE)

  }
  
  perf_hat <- p_0 + mu %*% t(H_mat)  # Filtered predictions of performance

  list(perf_hat=perf_hat, mu=mu, Sigma=Sigma, log_likelihood=log_likelihood)
}


fit_ffm_via_kalman <- function(starting_theta,
			       prior_mean_fitness=0, prior_mean_fatigue=0,
			       prior_sd_fitness=25, prior_sd_fatigue=25,
			       prior_ff_corr=0) {

  get_negloglike <- function(theta) {
    p_0  <- theta[1] # performance baseline
    k_g   <- theta[2] # fitness weight
    k_h   <- theta[3] # fatigue weight
    tau_g <- theta[4] # fitness decay
    tau_h <- theta[5] # fatigue decay
    sigma_e <- theta[6] # standard deviation of error
  
    filtered <- do_ffm_kalman(training_df, p_0, k_g, k_h, tau_g, tau_h,
			      sigma_e,
			      prior_mean_fitness, prior_mean_fatigue,
			      prior_sd_fitness, prior_sd_fatigue,
			      prior_ff_corr)
    -sum(filtered$log_likelihood)
  }
  
  optim(starting_theta, get_negloglike, method = "BFGS")
}

# Fitting the FFM using the functions above ----------------------
starting_theta <- c(400, .05, .15, 20, 5, sqrt(35))
ffm_optim <- fit_ffm_via_kalman(starting_theta,
		                prior_mean_fitness=0, prior_mean_fatigue=0,
		                prior_sd_fitness=35, prior_sd_fatigue=35,
		                prior_ff_corr=0)

(theta_hat <- ffm_optim$par)


ffm_fit <- do_ffm_kalman(training_df, theta_hat[1], theta_hat[2], theta_hat[3],
	                 theta_hat[4], theta_hat[5], theta_hat[6],
                         prior_mean_fitness=0, prior_mean_fatigue=0,
                         prior_sd_fitness=35, prior_sd_fatigue=35,
                         prior_ff_corr=0)


# Predicted ("filtered") performance values for every time step
plot(training_df$perf, main="Filtered predictions vs actuals")
points(ffm_fit$perf_hat, col='blue', type = 'b')

# State estimation of fitness and fatigue at each time point
plot(ffm_fit$mu[, 1], col='blue', type='b', ylim=c(0, 3000),
     main="Fitness (blue) and Fatigue (red)")
points(ffm_fit$mu[, 2], col='red', type='b')

