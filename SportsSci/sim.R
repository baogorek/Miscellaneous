library(dplyr)
source('helper-functions.R')

simulate_ffm <- function(T, load_spec, p_0, k_g, k_h, tau_g, tau_h, sigma_e,
			 fitness_0=0, fatigue_0=0, seed=0) {
  
  set.seed(seed)
  w <- numeric(T)
  for (load in load_spec) {
    length_out <- load$end - load$start + 1
    w[load$start:load$end] <- (seq(load$start_level, load$end_level, length.out=length_out)
  			     + rnorm(length_out, 0, load$noise_sd))
  }
  w <- ifelse(w < 0, 0, w)  # noise could make training impulse negative
  
  
  # initial fitness and fatigue
  initial_fitness_effects <- fitness_0 * exp(-(1:T) / tau_g)
  initial_fatigue_effects <- fatigue_0 * exp(-(1:T) / tau_h)
  
  
  fitness <- (initial_fitness_effects
  	    + sapply(1:T, function(t) convolve_training(w[1:t], tau_g)))
  fatigue <- (initial_fatigue_effects
  	    + sapply(1:T, function(t) convolve_training(w[1:t], tau_h)))
  
  
  E_perf <- p_0 + k_g * fitness - k_h * fatigue
  perf <- E_perf + rnorm(T, 0, sigma_e)
  
  data.frame(t=1:T, w, perf)
}

# Example -----------------------------
if (FALSE) {

  load_spec <- list(list(start=3, end = 5, start_level=10, end_level=20, noise_sd = 0),
                    list(start=30, end = 100, start_level=50, end_level=50, noise_sd = 1))
 
  training_df <- simulate_ffm(T=200, load_spec=load_spec,
			      p_0=496, k_g=.07, k_h=.27, tau_g=60, tau_h=13,
			      sigma_e=10)
  plot(perf ~ t, data=training_df)
  plot(w ~ t, data=training_df)
} 

