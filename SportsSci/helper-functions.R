
# Helper functions
convolve_training <- function(w, tau) {
  T <- length(w)
  if (T <= 1) return(0)

  yesterday_to_first_day <- (T - 1):1
  first_day_to_yesterday <- 1:(T - 1)
  sum(w[first_day_to_yesterday] * exp(-yesterday_to_first_day / tau))
}

ewma_training <- function(w, tau) {
  T <- length(w)
  if (T <= 1) return(0)

  today_to_first_day <- T:1
  first_day_to_today <- 1:T
  sum(w[first_day_to_today] * exp(-today_to_first_day / tau))
}


get_E_perf <- function(w, p_0, k_g, k_h, tau_g, tau_h,
		       tau_h2=NA,
		       fitness_0=0, fatigue_0=0) {
  # w: vector of training implulses
  # p_0, k_g, k_h, tau_g, tau_h: FFM parameters
  # fitness_0, fatigue_0: optional starting values of fitness and fatigue
  T <- length(w)
  initial_fitness_effects <- fitness_0 * exp(-(1:T) / tau_g)
  initial_fatigue_effects <- fatigue_0 * exp(-(1:T) / tau_h)
  fitness <- (initial_fitness_effects
            + sapply(1:T, function(t) convolve_training(w[1:t], tau_g)))

  # VDR filter on fatigue (optional) -------------------------
  w_fatigue <- w
  if (!is.na(tau_h2)) {
      w_fatigue <- sapply(1:T, function(t) ewma_training(w[1:t], tau_h2))
  }
  fatigue <- (initial_fatigue_effects
              + sapply(1:T, function(t) convolve_training(w_fatigue[1:t], tau_h)))

  p_0 + k_g * fitness - k_h * fatigue
}

hill_transform <- function(w, kappa, gamma, delta) {
  # w: vector (or scalar) of training implulses
  # kappa, gamma, delta: Hill function parameters
  kappa * w ^ gamma / (w ^ gamma +  delta ^ gamma)
}

# Testing -----------------------------------------------------------------

# test 1 - third performance periods needs two lagged training periods
exp_2days <- c(exp(-2 / 1.5), exp(-1 / 1.5))
convolve_training(c(1,1,1), 1.5) == sum(exp_2days)

# test 2 - last training value doesn't affect current performance period
convolve_training(c(1, 1, 20), 1.5) == convolve_training(c(1, 1, 1), 1.5)

# test 3 - third measurment period is weighting linear combination of first two exponentials
convolve_training(c(3, 5, 6), 1.5) == exp_2days %*% c(3, 5)

# test 4 - initital training period is always 0
convolve_training(c(1), 1.5)
convolve_training(c(99), 1.5)
