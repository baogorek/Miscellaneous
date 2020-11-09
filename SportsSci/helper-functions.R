source('utils.R')

fisher_transform <- function(rho) {
  .5 * log((1 + rho) / (1 - rho))
}


inv_fisher_transform <- function(z) {
  # gets back to correlation scale
  (exp(2 * z) - 1) / (exp(2 * z) + 1)
}


create_kalman_model <- function(p_0 = 400, k_g = .1, k_h = .3, tau_g = 50, tau_h = 15,
                                xi = 20,
                                sigma_g = 8, sigma_h = 4, rho_gh = -.3,
                                initial_g = 35, initial_h = 20,
                                initial_sd_g = 10, initial_sd_h = 5,
                                initial_rho_gh = 0) {
  # Transition matrix
  A <- matrix(c(exp(-1 / tau_g), 0, 0, exp(-1 / tau_h)), ncol=2)

  # State intercept - each row is the fitness and fatigue effect to the next day's workout
  B <- matrix(c(exp(-1 / tau_g), exp(-1 / tau_h)), ncol=1)

  # Measurement matrix
  C <- matrix(c(k_g, -k_h), ncol=2)

  # Variances
  Q <- matrix(c(sigma_g ^ 2, rep(rho_gh * sigma_g * sigma_h, 2),
                sigma_h ^ 2), ncol=2)
  xi <- xi  # aka "R" matrix

  # prior distribution of fitness and fatigue
  x_0 <- c(initial_g, initial_h)
  M_0 <- matrix(c(initial_sd_g ^ 2,
                 rep(initial_rho_gh * initial_sd_g * initial_sd_h, 2),
                 initial_sd_h ^ 2), ncol=2)
  
  model <- list(A = A, B = B, C = C, Q = Q, xi = xi, x_0 = x_0, M_0 = M_0, p_0 = p_0)
  class(model) <- "kalmanfilter"
  model
}


## Example
if (FALSE) {
kalman_model <- create_kalman_model(p_0 = 400, k_g = .1, k_h = .3, tau_g = 50, tau_h = 15,
                                    xi = 20,
                                    sigma_g = 8, sigma_h = 4, rho_gh = -.3,
                                    initial_g = 35, initial_h = 20,
                                    initial_sd_g = 10, initial_sd_h = 5,
                                    initial_rho_gh = 0)

}


print.kalmanfilter <- function(mod, ...) {
   with(mod, {
     cat("\n------- Your Kalman Filter Model -----\n")
     cat("------- State Model ------------------\n")
     mat_op_print("x_(n + 1) = [", A=A, "] * ", "x_n + [", B=B, "] * w_n + v_n")
     cat("where\n")
     mat_op_print("v_n ~ N(", c(0, 0), ", ", Q = Q, ")")
     cat("and\n")
     mat_op_print("x_0 =[", x_0, "], Var(x_0) = [", M_0=M_0, "]")
     cat("\n------- Meansurement Model ------------\n")
     mat_op_print("y_n =", p_0, "+ [", C=C, "] * x_n + eta_n")
     cat("\nwhere\n")
     mat_op_print("eta_n ~ N(", 0, ", ", xi = xi, "^ 2)")
   })
}


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


get_euler_path_turner <- function(w, tau, k, power=1, initial=0) {
  # works for either fitness or fatigue
  T <- length(w)
  f <- numeric(T) 

  # t == 1
  f[1] <- initial # this is time 0 in math notation
  for (t in 2:T) {
    f[t] <- max(0, f[t - 1] + k * w[t - 1] - (1 / tau) * f[t - 1] ^ power)
  }
  f
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


