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
# What this means is that, without noise:
#
# period 1's effect is gamma_1
# period 2's effect is -gamma_1 - gamma_2 - gamma_3
# period 3's effect is gamma_3
# period 4's effect is gamma_2

# So for 4 periods with profile (+5, +15, -8, -12), the gamma profile would be
# gamma_1 = 5
# gamma_2 = -12 
# gamma_3 = -8 

library(MASS)

alpha_mat <- matrix(rep(0, 4 * 12), ncol = 4)

T <- matrix(c(1,  0,  0,  0,
              0, -1, -1, -1,
              0,  1,  0,  0,
              0,  0,  1,  0), byrow = TRUE, ncol = 4)

sigma_epsilon <- 8.0 # affects the measurement error
sigma_xi <- 4 # affects the local level
sigma_omega <- 4 # affects the seasonality

H <- matrix(sigma_epsilon ^ 2)

Q <- diag(c(sigma_xi ^ 2, sigma_omega ^ 2, 0.0, 0.0))

R <- matrix(c(1, 0, 0, 0,
              0, 1, 0, 0,
              0, 0, 0, 0,
              0, 0, 0, 0), byrow = TRUE, ncol = 4)

alpha <- c(20, 5, -12, -8) # initial value

alpha_mat[1, ] <- alpha
for (t in 2:12) {
  alpha <- T %*% alpha + R %*% mvrnorm(1, 0 * alpha, Q)
alpha_mat[t, ] <- alpha
}

Z <- matrix(c(1, 1, 0, 0), ncol = 4)

noise <- sigma_epsilon * rnorm(12)
signal <- alpha_mat %*% t(Z)
y <- signal + noise 
plot(y ~ c(1:12), type = 'b', ylim = c(-5, 45))

# Performing Kalman Filter


a_mat <- matrix(rep(0, 4 * 12), ncol = 4)

a <- c(18, 5, -12, -8) # initial value
P <- 1 * diag(c(.01, .01, 0.01, 0.01))

a_mat[1, ] <- a

for (t in 2:12) {
  v <- y[t - 1] - Z %*% a # set to 0 for fcast
  F <- Z %*% P %*% t(Z) + H
  K <- T %*% P %*% t(Z) %*% solve(F) # optimal Kalman Gain, set to 0 for fcast
  L <- T - K %*% Z # used only in covariance update
  
  a <- T %*% a + K %*% v
  P <- T %*% P %*% t(L) + R %*% Q %*% t(R)
  
  a_mat[t, ] <- a
}

recovered_signal <- a_mat %*% t(Z)

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
