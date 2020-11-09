library(dplyr)

source('helper-functions.R')
source('sim.R')

options(scipen = 999)  # No scientific notation

# Simulation 1: basic FFM and Min MSE (same as ML) ---------------------------------------
load_spec <- list(list(start = 3, end = 5, start_level=10, end_level=20, noise_sd = 0),
                  list(start = 30, end = 100, start_level=50, end_level=50, noise_sd = 1))

training_df <- simulate_ffm(T=200, load_spec=load_spec,
                            p_0=496, k_g=.07, k_h=.27, tau_g=60, tau_h=13,
                            sigma_e=10, seed=255)

## Fitting with R's optim and RSS loss ------------------------------------------------
rss <- function(theta) {
  p_0  <- theta[1] # performance baseline
  k_g   <- theta[2] # fitness weight
  k_h   <- theta[3] # fatigue weight
  tau_g <- theta[4] # fitness decay
  tau_h <- theta[5] # fatigue decay

  sum((training_df$perf - get_E_perf(training_df$w, p_0, k_g, k_h, tau_g, tau_h)) ^ 2)
}

# Increase the coefficients since Hill transform has decreased training impulse
optim_results <- optim(c(400, 20 * .05, 20 * .15, 20, 10), rss, method = "BFGS",
                       hessian = TRUE, control = list(maxit = 1000, reltol=1E-14))

(theta_hat <- optim_results$par)

training_df$perf_hat <- do.call(get_E_perf, append(list(w=training_df$w),
						   as.list(theta_hat)))

plot(perf ~ t, data = training_df)
points(perf_hat ~ t, data = training_df, type = 'b', col = 'blue')


# Simulation 2: Using Hill function ---------------------------------------------
load_spec <- list(list(start=3, end = 100, start_level=20, end_level=50, noise_sd = 1),
                  list(start=100, end = 180, start_level=50, end_level=30, noise_sd = 1))

training_df <- simulate_ffm(T=200, load_spec=load_spec,
                            p_0=496, k_g=20 *.07, k_h=20*.27, tau_g=60, tau_h=13,
                            sigma_e=10,
                            delta = 20, gamma = 2.5,
                            fitness_0 = 0, fatigue_0 = 0)


## Bad Optimization: Hill function using one-shot optim ---------------------------------
rss <- function(theta) {
  p_0  <- theta[1] # performance baseline
  k_g   <- theta[2] # fitness weight
  k_h   <- theta[3] # fatigue weight
  tau_g <- theta[4] # fitness decay
  tau_h <- theta[5] # fatigue decay
  gamma <- theta[6] # fatigue decay
  delta <- theta[7] # fatigue decay
  
  w_hill <- hill_transform(training_df$w_raw, 1, gamma, delta)
  sum((training_df$perf - get_E_perf(w_hill, p_0, k_g, k_h, tau_g, tau_h)) ^ 2)
}

optim_results <- optim(c(400, 20 * .05, 20 * .15, 30, 10, 3, 15), rss, method = "BFGS",
                       hessian = TRUE, control = list(maxit = 1000, reltol=1E-14))

optim_results
(theta_hat <- optim_results$par)

w_hill <- hill_transform(training_df$w_raw, 1, gamma=theta_hat[6], delta=theta_hat[7])
training_df$perf_hat <- do.call(get_E_perf, append(list(w=w_hill),
						   as.list(theta_hat[1:5])))

plot(perf ~ t, data = training_df)
points(perf_hat ~ t, data = training_df, type = 'b', col = 'blue')

## Better Optimization: Hill function using profile likelihood ---------------------------

combos <- expand.grid(gamma = c(1.5, 2.5, 5.0, 30), delta = c(1, 10, 20, 50, 100))
combos['RSS'] <- NA
combos['p_0'] <- NA
combos['k_g'] <- NA
combos['k_h'] <- NA
combos['tau_g'] <- NA
combos['tau_h'] <- NA

# Paul : Show it graphically - have a method to choose gamma and delta
# TRimp - Bannister's trimp

for (i in 1:nrow(combos)) {
  cat("Trying Hill function combination gamma =", gamma, ", delta =", delta, "\n")
  gamma <- combos[i, 'gamma'] 
  delta <- combos[i, 'delta'] 
  
  w_hill <- hill_transform(training_df$w_raw, 1, gamma, delta)
  
  rss <- function(theta) {
    p_0  <- theta[1] # performance baseline
    k_g   <- theta[2] # fitness weight
    k_h   <- theta[3] # fatigue weight
    tau_g <- theta[4] # fitness decay
    tau_h <- theta[5] # fatigue decay
    sum((training_df$perf - get_E_perf(w_hill, p_0, k_g, k_h, tau_g, tau_h)) ^ 2)
  }
  tryCatch({ 
    optim_results <- optim(c(400, 20 * .05, 20 * .15, 30, 10), rss, method = "BFGS",
                           hessian = FALSE, control = list(maxit = 1000, reltol=1E-14))
    
    (theta_hat <- optim_results$par)
    combos[i, 'RSS'] <- optim_results$value
    combos[i, 'p_0'] <- optim_results$par[1] 
    combos[i, 'k_g'] <- optim_results$par[2] 
    combos[i, 'k_h'] <- optim_results$par[3] 
    combos[i, 'tau_g'] <- optim_results$par[4] 
    combos[i, 'tau_h'] <- optim_results$par[5] 
  }, error = function(e) {
    cat("--Problem optimizing for gamma =", gamma, ", delta =", delta, "\n")
  })
}

best_row <- combos[which.min(combos$RSS), ]

w_hill <- hill_transform(training_df$w_raw, 1, gamma=best_row$gamma, delta=best_row$delta)
training_df$perf_hat <- get_E_perf(w=w_hill, p_0 = best_row$p_0,
				   k_g = best_row$k_g, k_h = best_row$k_h,
				   tau_g=best_row$tau_g, tau_h=best_row$tau_h)

plot(perf ~ t, data = training_df)
points(perf_hat ~ t, data = training_df, type = 'b', col = 'blue')

# Simulation 3: VDR and Min MSE (same as ML) ---------------------------------------
load_spec <- list(list(start = 3, end = 5, start_level=10, end_level=20, noise_sd = 0),
                  list(start = 30, end = 100, start_level=50, end_level=50, noise_sd = 1))

training_df <- simulate_ffm(T=200, load_spec=load_spec,
                            p_0=496, k_g=.07, k_h=.27, tau_g=60, tau_h=13,
			    tau_h2 = 2.5,
                            sigma_e=10,
			    seed=0)

## Fitting with R's optim and RSS loss ------------------------------------------------
rss <- function(theta) {
  p_0  <- theta[1] # performance baseline
  k_g   <- theta[2] # fitness weight
  k_h   <- theta[3] # fatigue weight
  tau_g <- theta[4] # fitness decay
  tau_h <- theta[5] # fatigue decay
  tau_h2 <- theta[6] # fatigue decay
  sum((training_df$perf - get_E_perf(training_df$w, p_0, k_g, k_h, tau_g, tau_h,
				     tau_h2 = tau_h2)) ^ 2)
}

optim_results <- optim(c(400, .05, .15, 30, 10, .5), rss, method = "BFGS",
                       hessian = FALSE, control = list(maxit = 1000, reltol=1E-14))

(theta_hat <- optim_results$par)
training_df$perf_hat <- get_E_perf(w=training_df$w, p_0=theta_hat[1], k_g=theta_hat[2],
				   k_h=theta_hat[3], tau_g = theta_hat[4], tau_h = theta_hat[5],
				   tau_h2 = theta_hat[6])

plot(perf ~ t, data = training_df)
points(perf_hat ~ t, data = training_df, type = 'b', col = 'blue')


# Fitting with Kalman Filter ------------------------------------------------
filter <- function(df, kalman_model) {
  
  if (length(setdiff(c('w', 'y'), names(df))) > 0 ) {
    stop("df must have column 'w' (impulse) and 'y' (performance)")
  }
  # Data set extractions --
  T <- nrow(df)
  w <- df$w
  y <- df$y

  # Setting up structures ----------------------------------
  loglike <- numeric(T)
  X <- matrix(rep(NA, 2 * T), ncol=2)  # State vectors as rows
  M <- matrix(rep(NA, 4 * T), ncol=4) # Vectorized state vcovs as rows 
  
  with(kalman_model, {
    # The Kalman updating equations
    for (n in 1:T) {
      # A priori mean and variance of state -- 
      if (n == 1) { 
        z_n <- x_0
        P_n <- M_0
      } else {
        z_n <- A %*% X[n - 1, ] + B * w[n - 1]
        P_n <- Q + A %*% matrix(M[n - 1, ], ncol = 2) %*% t(A)
      }
 
      # Likelihood of perf measurement n --
      S_n <- xi ^ 2 + C %*% P_n %*% t(C)  # pre-fit residual covariance
      e_n <- y[n] - (p_0 + C %*% z_n)
      loglike[n] <- dnorm(e_n, mean = 0, sd = sqrt(S_n), log=TRUE)
    
      # A posteriori mean and variance of state --
      K_n <- P_n %*% t(C) %*% (1 / S_n)  # Kalman Gain
      X[n, ] <- z_n + K_n %*% e_n
      M[n, ] <- as.vector((diag(2) - K_n %*% C) %*% P_n)
    }
    df$y_hat <- p_0 + X %*% t(C)  # Filtered predictions of performance
    list(df = df, X = X, M = M, loglike = loglike)
  })
}


extract_and_transform_params <- function(kalman_model) {
  with(kalman_model, {
    k_g <- C[1, 1]
    k_h <- -1 * C[1, 2]
    tau_g <- -1 / log(A[1, 1])
    tau_h <- -1 / log(A[2, 2])
    sigma_g <- sqrt(Q[1, 1])
    sigma_h <- sqrt(Q[2, 2])
    rho_gh <- Q[1, 2] / (sigma_g * sigma_h)

  #c(log(p_0), # 1
  #  log(k_g), # 2
  #  log(k_h), # 3
  #  log(tau_g), # 4
  #  log(tau_h), # 5
  #  log(sigma_g), # 6
  #  log(sigma_h), # 7
  #  #qlogis(rho_gh / 2 + .5), # 8
  #  log(xi) # 9
  #) 
  c(p_0,
    k_g,
    k_h,
    tau_g,
    tau_h,
    xi,
    sigma_g,
    sigma_h,
    rho_gh)
  })
}

update <- function(kalman_model, theta) {
    mod <- kalman_model

    #mod$p_0 <- exp(theta[1])
    #mod$C[1, 1] <- exp(theta[2])
    #mod$C[1, 2] <- -1 * exp(theta[3]) 
    #mod$A[1, 1] <- exp(-1 / exp(theta[4]))
    #mod$A[2, 2] <- exp(-1 / exp(theta[5]))
    #mod$B[1, 1] <- mod$A[1, 1]
    #mod$B[2, 1] <- mod$A[2, 2]
    #mod$Q[1, 1] <- exp(theta[6]) ^ 2
    #mod$Q[2, 2] <- exp(theta[7]) ^ 2
    #rho <- 2 * plogis(theta[8]) - 1
    #mod$Q[1, 2] <- sqrt(mod$Q[1, 1] * mod$Q[2, 2]) * rho
    #mod$Q[2, 1] <- mod$Q[1, 2]
    #mod$xi <- exp(theta[9])

    mod$p_0 <- theta[1]
    mod$C[1, 1] <- theta[2]
    mod$C[1, 2] <- -1 * theta[3] 
    mod$A[1, 1] <- exp(-1 / theta[4])
    mod$A[2, 2] <- exp(-1 / theta[5])
    mod$B[1, 1] <- exp(-1 / theta[4])
    mod$B[2, 1] <- exp(-1 / theta[5])
    mod$xi <- theta[6]
    mod$Q[1, 1] <- theta[7] ^ 2
    mod$Q[2, 2] <- theta[8] ^ 2
    mod$Q[1, 2] <- theta[7] * theta[8] * theta[9]
    mod$Q[2, 1] <- theta[7] * theta[8] * theta[9]

  mod
}

# Operations with Kalman Filter 

## Recovering parameters using simulation
kalman_model <- create_kalman_model(p_0 = 500, k_g = .1, k_h = .3,
                                    tau_g = 60, tau_h = 15,
			            xi = 10,
				    sigma_g = 20, sigma_h = 15, rho_gh = .5,
			            initial_g = 40, initial_h = 20,
			            initial_sd_g = 20, initial_sd_h = 20,
			            initial_rho_gh =0)

print(kalman_model)


# Bring in the Python simulation
sim_df <- read.csv("python_sim.csv")

load_spec <- create_pulsing_load_spec(10300, 70, 40)
sim_df2 <- simulate_kalman(load_spec, kalman_model)
sim_df2$w

theta <- extract_and_transform_params(kalman_model)

theta[1] <- 500
theta[7] <- 10 # sigma_g
theta[8] <- 5 # sigma_h
theta[9] <- 0 

df <- sim_df

mod <- update(kalman_model, theta)
filtered <- filter(df, mod)
-1.0 * sum(filtered$loglike)


theta1 <- theta[1:6]
theta2 <- theta[7:8]
theta3 <- theta[9]

cost_fn1 <- function(theta1) {
    theta <- c(theta1, theta2, theta3)
    mod <- update(kalman_model, theta)
    filtered <- filter(df, mod)
    -1.0 * sum(filtered$loglike)
}

cost_fn2 <- function(theta2) {
    theta <- c(theta1, theta2, theta3)
    mod <- update(kalman_model, theta)
    filtered <- filter(df, mod)
    -1.0 * sum(filtered$loglike)
}

cost_fn3 <- function(theta3) {
    theta <- c(theta1, theta2, theta3)
    mod <- update(kalman_model, theta)
    filtered <- filter(df, mod)
    -1.0 * sum(filtered$loglike)
}



res <- optim(theta1, cost_fn1, method = "BFGS",
             control = list(maxit = 10000, reltol=1E-14,
			    parscale = c(500, .1, .1, 10, 10, 1)))
theta1 <- res$par

res <- optim(theta2, cost_fn2, method = "BFGS",
             control = list(maxit = 10000, reltol=1E-14))
theta2 <- res$par

res <- optim(theta3, cost_fn3, method = "L-BFGS-B",
	     lower=-1,
	     upper=1,
             control = list(maxit = 10000, factr=1E-14))
theta3 <- res$par




# 3D graphics
library(lattice)

# k_g and k_h
fn <- function(x, y) {
  theta <- c(500, x, y, 60, 15, 20, 15, .5, 10)
  mod <- update(kalman_model, theta) 
  filtered <- filter(sim_df, mod)
  sum(filtered$loglike)
}

grid <- expand.grid(k_g = seq(.03, .35, .02), 
		    k_h = seq(.15, .45, .02))
grid$loglike <- NA

for (i in 1:nrow(grid)) {
  print(i)
  grid[i, "loglike"] <-  fn(grid[i, "k_g"], grid[i, "k_h"])
}

wireframe(loglike ~ k_g * k_h, grid, shade = TRUE, aspect = c(1, 1),
	      light.source = c(10,50,50), main = "Study 1",
	          scales = list(z.ticks=5,arrows=FALSE, col="black", font=10, tck=0.5),
	          screen = list(z = 30, x = -75, y = 0))


# sigma_g and sigma_h
fn <- function(x, y) {
  theta <- c(500, .1, .3, 60, 15, x, y, .5, 10)
  mod <- update(kalman_model, theta) 
  filtered <- filter(sim_df, mod)
  sum(filtered$loglike)
}

grid <- expand.grid(sigma_g = seq(5, 35, 2), 
		    sigma_h = seq(10, 20, 1))
grid$loglike <- NA

for (i in 1:nrow(grid)) {
  print(i)
  grid[i, "loglike"] <-  fn(grid[i, "sigma_g"], grid[i, "sigma_h"])
}

wireframe(loglike ~ sigma_g * sigma_h, grid, shade = TRUE, aspect = c(1, 1),
	      light.source = c(10,50,50), main = "Study 1",
	          scales = list(z.ticks=5,arrows=FALSE, col="black", font=10, tck=0.5),
	          screen = list(z = 130, x = -75, y = 0))









theta <- extract_and_transform_params(kalman_model)
theta[8] <- .6

optim_fn <- function(theta) {
  mod <- update(kalman_model, theta)
  filtered <- filter(sim_df, mod)
  -1.0 * sum(filtered$loglike)
}
 
starting <- theta + .1 * rnorm(length(theta))
res <- optim(starting, optim_fn, method = "BFGS",
             control = list(maxit = 10000, reltol=1E-14))
# TODO: fix state space error subscript
update(kalman_model, theta)
update(kalman_model, starting)
update(kalman_model, res$par)


N <- nrow(sim_df)
sim_df$fit_lag1 <- c(NA, sim_df$true_fitness[1:(N-1)])
sim_df$fat_lag1 <- c(NA, sim_df$true_fatigue[1:(N-1)])
sim_df$w_lag1 <- c(NA, sim_df$w[1:(N-1)])

sim_df$v1_hat <- sim_df$true_fitness - exp(-1/60) * sim_df$fit_lag1 - exp(-1/60) * sim_df$w_lag1
sim_df$v2_hat <- sim_df$true_fatigue - exp(-1/15) * sim_df$fat_lag1 - exp(-1/15) * sim_df$w_lag1

sd(sim_df$v1_hat[2:N])
sd(sim_df$v2_hat[2:N])
cor(sim_df$v1_hat[2:N], sim_df$v2_hat[2:N])


# Predicted ("filtered") performance values for every time step
plot(training_df$perf, main="Filtered predictions vs actuals")
points(ffm_fit$perf_hat, col='blue', type = 'b')

# State estimation of fitness and fatigue at each time point
plot(ffm_fit$mu[, 1], col='blue', type='b', ylim=c(0, 3000),
     main="Fitness (blue) and Fatigue (red)")
points(ffm_fit$mu[, 2], col='red', type='b')


# Another data set
training_df <- read.csv('training_df.csv')

# Modify to fix state intercept: initial conditions:
training_df[1, 'w'] <- 100
training_df[nrow(training_df), 'w'] <- 100

if (FALSE) {
  # for interactive purposes
  p_0 <- 400
  k_g <- .05
  k_h <- .15
  tau_g <- 20
  tau_h <- 5 
  xi <- sqrt(35) 
  sigma_g <- 2
  sigma_h <- 2
  sigma_gh <- 0
  prior_mean_fitness <- 0
  prior_mean_fatigue <- 0
  prior_sd_fitness <- 1 
  prior_sd_fatigue <- 1 
  prior_ff_corr <- 0
}



# Simulation 5: Turner and Min MSE (same as ML) ---------------------------------------
load_spec <- list(list(start = 3, end = 15, start_level=10, end_level=20, noise_sd = 0),
                  list(start = 30, end = 100, start_level=50, end_level=100, noise_sd = 1),
                  list(start = 120, end = 200, start_level=100, end_level=70, noise_sd = 1))

training_df <- simulate_turner(250, load_spec,
			       p_0 = 155, k_g = .05, k_h = .15, tau_g = 61, tau_h = 13,
			       sigma_e = 10,
			       alpha = 1.16, beta = .85,
			       fitness_0 = 70.9, fatigue_0 = 24.5,
			       seed=1030943)

p_0 = 155
k_g = .05
k_h = .15
tau_g = 61
tau_h = 13
sigma_e = 10
alpha = 1.16
beta = .85
fitness_0 = 70.9
fatigue_0 = 24.5

## Fitting with R's optim and RSS loss ------------------------------------------------
rss <- function(theta) {
  p_0  <- theta[1] # performance baseline
  k_g   <- theta[2] # fitness weight
  k_h   <- theta[3] # fatigue weight
  tau_g <- theta[4] # fitness decay
  tau_h <- theta[5] # fatigue decay
  alpha <- 1.16 # fitness ODE power transform
  beta <- .85 # fatigue ODE power transform
  #sigma <- exp(theta[8])  # should definitely do this everywhere!
  #fitness_0 <- theta[8] # fitness initial condition
  #fatigue_0 <- theta[9] # fatigue initial condition
  fitness_0 <- 70.9
  fatigue_0 <- 24.5

  w <- training_df$w

  fitness <- get_euler_path_turner(w, tau_g, k_g, alpha, fitness_0)
  fatigue <- get_euler_path_turner(w, tau_h, k_h, beta, fatigue_0)
  E_perf <- p_0 + fitness - fatigue

  sum((training_df$perf - E_perf) ^ 2)
  #plike <- (dnorm(training_df$perf, mean=E_perf, sd=sigma, log=TRUE)
  #          + dgamma(alpha, 3, 2, log=TRUE)
  #          + dgamma(beta, 3, 2, log=TRUE))
  #sum(-plike)
}
# NOTE: after lots of experimenting, realizing that the stability of the
# rss is worth iteration over the 2-d grid. Things just fall apart when
# jumping into the likelihood

theta_starting <- c(100, .08, .2, 30, 10)
optim_results <- optim(theta_starting, rss, method = "BFGS",
                       hessian = FALSE, control = list(maxit = 1000, reltol=1E-14))

(theta_hat <- optim_results$par)
training_df$perf_hat <- get_E_perf(w=training_df$w, p_0=theta_hat[1], k_g=theta_hat[2],
				   k_h=theta_hat[3], tau_g = theta_hat[4], tau_h = theta_hat[5],
				   tau_h2 = theta_hat[6])

plot(perf ~ t, data = training_df)
points(perf_hat ~ t, data = training_df, type = 'b', col = 'blue')


