library(MASS)
library(dplyr)

source('helper-functions.R')


create_pulsing_load_spec <- function(T, spacing = 20, pulse_width = 10,
				     level = 50) {
  load_spec <- list()
  for (start in seq(1, T, spacing)) {
    load_window <- list(start = start, end = start + pulse_width - 1,
			start_level = level, end_level = level, noise_sd = 0)
    load_spec[[length(load_spec) + 1]] <- load_window
  } 
  load_spec
}

create_training_impulse <- function(load_spec, T = NA) {
  if (is.na(T)) {
    T <- max(unlist(as.data.frame(do.call('rbind', load_spec))$end))
  }

  w <- numeric(T)
  for (load in load_spec) {
    length_out <- load$end - load$start + 1
    w[load$start:load$end] <- (seq(load$start_level, load$end_level, length.out=length_out)
  			     + rnorm(length_out, 0, load$noise_sd))
  }
  w <- ifelse(w < 0, 0, w)  # noise could make training impulse negative
  return(w) 
}

simulate_ffm <- function(T, load_spec, p_0, k_g, k_h, tau_g, tau_h, sigma_e,
			 delta = NA, gamma = NA, 
			 tau_h2 = NA,
			 fitness_0 = 0, fatigue_0 = 0, seed = 0) {
  set.seed(seed)
 
  w <- create_training_impulse(T, load_spec) 

  # Hill function transformation of training impulse (optional) ------ 
  w_raw <- w
  if (!is.na(delta) & !is.na(gamma)) {
     w <- (w ^ gamma / (delta ^ gamma + w ^ gamma))
  }

  fitness <- (fitness_0
  	      + sapply(1:T, function(t) convolve_training(w[1:t], tau_g)))

  # VDR filter on fatigue (optional) -------------------------
  w_fatigue <- w
  if (!is.na(tau_h2)) {
    w_fatigue <- sapply(1:T, function(t) ewma_training(w[1:t], tau_h2))
  }
 
  fatigue <- (fatigue_0
  	      + sapply(1:T, function(t) convolve_training(w_fatigue[1:t], tau_h)))
  
  E_perf <- p_0 + k_g * fitness - k_h * fatigue
  perf <- E_perf + rnorm(T, 0, sigma_e)
  
  data.frame(t=1:T, w_raw, w, perf)
}


if (FALSE) {
  # for interactive purposes
  p_0 <- 400
  k_g <- .05
  k_h <- .15
  tau_g <- 20
  tau_h <- 5 
  xi <- sqrt(35) 
  sigma_g <- 15
  sigma_h <- 10 
  sigma_gh <- 5 
  prior_mean_fitness <- 75
  prior_mean_fatigue <- 15 
  prior_sd_fitness <- 1 
  prior_sd_fatigue <- 1 
  prior_ff_corr <- 0
}

simulate_kalman <- function(load_spec, kalman_model) {
  # Create exogenous training loads
  w <- create_training_impulse(load_spec)
  T <- length(w)
 
  # Setting up structures ----------------------------------
  y <- numeric(T)
  X <- matrix(rep(NA, 2 * T), ncol=2)  # State matrix w/ fitness and fatigue

  with(kalman_model, {
    for (n in 1:T) {
      # A priori mean and variance of state -- 
      if (n == 1) { 
        # simulate unconditional: x_0
        X[n, ] <- mvrnorm(1, x_0, M_0)
      } else {
        # simulate conditional: x_n | x_(n - 1)
        X[n, ] <- mvrnorm(1, A %*% X[n - 1, ] + B * w[n - 1], Q)
      }
      # Simulate conditional: y_n | x_n
      y[n] <- rnorm(1, p_0 + C %*% X[n, ], xi) #sqrt(xi ^ 2 + C %*% Q %*% t(C)))
    }
    data.frame(t = 1:T, w, y, true_fitness = X[, 1], true_fatigue = X[, 2])
  })
}


# Examples -----------------------------
if (FALSE) {

  load_spec <- list(list(start=3, end = 100, start_level=20, end_level=50, noise_sd = 1),
                    list(start=100, end = 180, start_level=50, end_level=30, noise_sd = 1))
 
  training_df <- simulate_ffm(T=200, load_spec=load_spec,
			      p_0=496, k_g=20 *.07, k_h=20*.27, tau_g=60, tau_h=13,
			      sigma_e=10,
			      delta = 20, gamma = 2.5,
			      fitness_0 = 0, fatigue_0 = 0)

  plot(training_df$w, main='final training impulses')

  plot(training_df$w_raw, main='Raw (black) and Hill impulses')
  points(20 * training_df$w, col='red')

  plot(perf ~ t, data=training_df)
  plot(w ~ t, data=training_df)


  # Create training impulse
  load_spec <- create_pulsing_load_spec(400, 50, 15)
  w <- create_training_impulse(load_spec)
  plot(w)

  sim_df <- simulate_kalman(load_spec, p_0 = 500, k_g = .2, k_h = .1,
                            tau_g = 60, tau_h = 15,
  			    xi = 20, sigma_g = 10, sigma_h = 3, sigma_gh = 1,
  			    prior_mean_fitness = 40, prior_mean_fatigue = 5,
  			    prior_sd_fitness = 10, prior_sd_fatigue = 5,
  			    prior_ff_corr=.5)
} 



## Turner's values
#T <- 200
#tau_g <- 61
#tau_h <- 5.5
#alpha <- 1.16 # ODE nonlinearity power for fitness
#beta <- .85 # ODE nonlinearity power for fatigue
#k_g <- .10
#k_h <- .12
#p_0 <- 155
#fitness_0 <- 70.9 # initial condition for fitness
#fatigue_0 <- 24.5 # initial condition for fitness
#sigma_e <- 10

simulate_turner <- function(T, load_spec, p_0, k_g, k_h, tau_g, tau_h, sigma_e,
                            alpha = 1, beta = 1,
			    fitness_0 = 0, fatigue_0 = 0, seed = 0) {
  set.seed(seed)
  w <- create_training_impulse(T, load_spec) 
 
  fitness <- get_euler_path_turner(w, tau_g, k_g, alpha, fitness_0)
  fatigue <- get_euler_path_turner(w, tau_h, k_h, beta, fatigue_0)

  E_perf <- p_0 + fitness - fatigue
  perf <- E_perf + rnorm(T, 0, sigma_e)
  
  data.frame(t=1:T, w, perf)
}


