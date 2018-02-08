# Exploring state space models
# Using the JSS article on state space models to motivate the simulations
# Statistical Software for State Space Methods
# Commandeur, Koopman, Ooms (May 2011)

# Stochastic Seasonal Model, extension of local linear trend model

# The following seasonal model can be written in state-space form:
#
# y_t    = mu_t + gamma_1,t + epsilon_t
# mu_t+1 = mu_t + xi_t
# gamma_1,t+1 = -gamma_1,t - gamma_2,t - gamma_3,t + omega_t
# gamma_2,t+1 = gamma_1,t
# gamma_3,t+1 = gamma_2,t
#
# with epsilon_t, zi_t, and omega_t iid normal with separate error components
#
# So for 4 periods with a mean of 18 and a seasonal profile of
# (+5, +15, -8, -12), the initial state for period 1 would be
# (18, 5, -12, -8), for which applying the recurrence relation again would
# yield (18, 15, 5, -12), and so forth.

library(MASS)

# State Transition Matrix
F <- matrix(c(1,  0,  0,  0,
              0, -1, -1, -1,
              0,  1,  0,  0,
              0,  0,  1,  0), byrow = TRUE, ncol = 4)

sigma_epsilon <- 2.0 # affects the measurement error
sigma_xi <- 1 # affects the local level
sigma_omega <- 1 # affects the seasonality

# Observation Model Matrix
H <- matrix(c(1, 1, 0, 0), ncol = 4)

# Covariance Matrix of the Process Noise
Q <- diag(c(sigma_xi ^ 2, sigma_omega ^ 2, 0.0, 0.0))

# Covariance Matrix of the Observation Noise
R <- matrix(sigma_epsilon ^ 2)

# Storing the state x in a matrix
x_mat <- matrix(rep(0, 4 * 12), ncol = 4)

x <- c(18, 5, -12, -8) # initial value, corresponding to 18 + 5 (period 1)
x_mat[1, ] <- x + mvrnorm(1, rep(0, 4), Q) # True initial state plus noise

for (t in 2:12) {
  w <- mvrnorm(1, rep(0, 4), Q)
  x <- F %*% x + w
x_mat[t, ] <- x
}

v <- rnorm(12, 0, sigma_epsilon)
y <- x_mat %*% t(H) + v 
plot(y ~ c(1:12), type = 'b', ylim = c(-5, 45))

write.csv(y, "C:/devl/test-series.csv", row.names = FALSE)
# Performing Kalman Filter

a_mat <- matrix(rep(0, 4 * 12), ncol = 4)

x <- c(18.0, 5, -12, -8) # initial value, corresponds to period 1
P <- 1.0 * diag(c(1, 1, 1, 1)) # initial value of covariance matrix

update <- function(x_apriori, P_apriori, z) {
  S <- R + H %*% P_apriori %*% t(H) # Innovation Covariance
  K <- P_apriori %*% t(H) %*% solve(S) # optimal Kalman Gain (SEL)
  y <- z - H %*% x_apriori 
  x_apost <- x_apriori + K %*% y
  P_apost <- P_apriori - K %*% H %*% t(P_apriori)
  return(list(x = x_apost, P = P_apost))
}

predict <- function(x_prev, P_prev) {
  x_next <- F %*% x_prev
  P_next <- F %*% P_prev %*% t(F) + Q
  return(list(x = x_next, P = P_next))
}

aposteriori <- update(x, P, y[1])
apriori_next <- predict(aposteriori$x, aposteriori$P)


# Stopped at this point


recovered_signal <- a_mat %*% t(H)

plot(signal ~ c(1:12), type = "b")
lines(recovered_signal ~ c(1:12), type = "b", col = "blue")

# Forecasting
a_mat <- matrix(rep(0, 4 * 12), ncol = 4)

for (t in 1:12) {
  a <- T %*% a
  a_mat[t, ] <- a
}

forecast <- a_mat %*% t(Z)

plot(signal ~ c(1:12), type = "b", xlim = c(0, 25))
lines(recovered_signal ~ c(1:12), type = "b", col = "blue")
lines(forecast ~ c(13:24), type = "b", col = "blue")
