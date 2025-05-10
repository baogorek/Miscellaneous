library(forecast)
library(dplyr)

set.seed(123)

# Generate data
n <- 100
phi <- 0.9   # AR(1) coefficient
beta <- 0.3  # Slope of the trend
sigma <- 5   # Standard deviation of noise

# Define the regressor (linear trend)
X <- 1:n  

# Generate AR(1) errors
epsilon <- rnorm(n, mean = 0, sd = sigma)
u <- numeric(n)
u[1] <- epsilon[1]

for (t in 2:n) {
  u[t] <- phi * u[t - 1] + epsilon[t]
}

y <- beta * X + u  

fit <- Arima(y, xreg = X, order = c(1,0,0))
summary(fit)

intercept <- coef(fit)["intercept"] %>% as.numeric()
phi_hat <- coef(fit)["ar1"] %>% as.numeric()
beta_hat <- coef(fit)["xreg"] %>% as.numeric()

plot(y ~ X, xlim = c(950, 1050))
lines(beta * X ~ X, col = "green")
lines(beta_hat * X ~ X, col = "orange")

n_steps <- 20
X_future <- (n+1):(n+n_steps)
forecast_values <- forecast(fit, xreg = X_future)

X_total <- c(X, X_future)
plot(forecast_values)
lines(X_total * beta_hat ~ X_total, col="red")

eta_n <- y[100] - (intercept + 100 * beta_hat)
y_hat_np1 <- intercept + beta_hat * 101 + phi_hat * eta_n

cat("Manual y_{n+1}:", y_hat_np1, "\n")
cat("forecast() y_{n+1}:", forecast_values$mean[1], "\n")

eta_np1 <- y_hat_np1 - (intercept + 101 * beta_hat)
y_hat_np2 <- intercept + beta_hat * 102 + phi_hat * eta_np1

cat("Manual y_{n+2}:", y_hat_np2, "\n")
cat("forecast() y_{n+2}:", forecast_values$mean[2], "\n")

# Simulate 100 paths
n_paths <- 100
sim_paths <- matrix(NA, nrow = n_steps, ncol = n_paths)
set.seed(456)  # For reproducibility
for (path in 1:n_paths) {
  sim_paths[, path] <- simulate(fit, nsim = n_steps, future = TRUE, xreg = X_future)
}

X_total <- c(X, X_future)
plot(X_total, c(y, rep(NA, n_steps)), type = "l", col = "black", 
     ylim = range(c(y, sim_paths, forecast_values$mean)), 
     xlab = "Time", ylab = "y", main = "Full Series with 100 Simulated Paths")
for (path in 1:n_paths) {
  lines(X_future, sim_paths[, path], col = "gray", alpha = 0.5)
}
lines(X_future, forecast_values$mean, col = "blue", lwd = 2)
lines(X_total, coef(fit)["intercept"] + coef(fit)["xreg"] * X_total, col = "red")
lines(X_total, beta_hat * X_total, col = "orange", lty = 2)  # Just beta_hat * X
legend("topleft", legend = c("Observed", "Forecast Mean", "Simulated Paths", 
                             "Trend (mu + beta_hat * X)", "beta_hat * X"), 
       col = c("black", "blue", "gray", "red", "orange"), lty = c(1, 1, 1, 1, 2))
