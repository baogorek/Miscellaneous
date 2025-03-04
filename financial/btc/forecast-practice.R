library(forecast)

set.seed(1927)

# Generate data
n <- 1000
phi <- 0.85   # AR(1) coefficient
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

# Generate dependent variable y with trend
y <- beta * X + u  

fit <- Arima(y, xreg = X, order = c(1,0,0))

# Extract estimated parameters
phi_hat <- coef(fit)["ar1"]
beta_hat <- coef(fit)["xreg"]

plot(y ~ X, xlim = c(950, 1050))
lines(beta * X ~ X, col = "green")
lines(beta_hat * X ~ X, col = "orange")

# Print model summary
summary(fit)
n_steps <- 20

# Forecasting: Extend the trend into the future
X_future <- (n+1):(n+n_steps)

# Generate forecasts using forecast()
forecast_values <- forecast(fit, xreg = X_future)
plot(forecast_values)
