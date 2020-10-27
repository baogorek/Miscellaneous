library(dplyr)
source('helper-functions.R')


create_training_impulse <- function(T, load_spec) {
  #T <- max(unlist(as.data.frame(do.call('rbind', load_spec))$end))
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

# Example -----------------------------
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


