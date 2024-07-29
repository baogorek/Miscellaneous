t_delta <- .0001  # time delta in seconds
x_final <- 300  # 300 feet is 100 yards
v_0 <- 1200  # in ft / s
theta_0 <- .0026 # angle of elevation in radians
simple_drag_coefficient <- .00064  # A bundle of coefficients

# Set initial values
t <- 0  # time in seconds (s)
y <- 0  # height dimension in feet (ft)
x <- 0  # horizontal travel dimension in feet (ft)
v <- v_0
theta <- theta_0
x_velocity <- v_0 * cos(theta_0)
y_velocity <- v_0 * sin(theta_0)

results <- list()
results[[1]] <- c(t, x, y, v, theta)
i <- 1
while (x < x_final) { 
  i <- i + 1

  drag_x <- simple_drag_coefficient * x_velocity * sqrt(x_velocity ^ 2 + y_velocity ^ 2)
  drag_y <- simple_drag_coefficient * y_velocity * sqrt(x_velocity ^ 2 + y_velocity ^ 2)

  y_prime <- v * sin(theta) - 32 * t_delta - drag_y * t_delta
  x_prime <- v * cos(theta) - drag_x * t_delta

  y_candidate <- y + y_prime * t_delta
  x_candidate <- x + x_prime * t_delta

  y_velocity <- (y_candidate - y) / t_delta
  x_velocity <- (x_candidate - x) / t_delta

  v <- sqrt(x_velocity ^ 2 + y_velocity ^ 2)
  theta <- atan(y_velocity / x_velocity)

  x <- x_candidate
  y <- y_candidate
  t <- t + t_delta

  results[[i]] <- c(t, x, y, v, theta)
}

df <- as.data.frame(t(do.call(cbind, results)))
names(df) <- c("t", "x", "y", "v", "theta")
nrow(df)

# Position view
plot(y ~ x, data = df)
abline(h = 1.5 / 12)  # sight line
abline(v = 150)  # 50 yards, should be sighted in 

# Time view
plot(y ~ t, data = df)
abline(h = 1.5 / 12)  # sight line

# Velocity position view
plot(v ~ x, data = df)
abline(v = 150)  # 50 yards
abline(h = 1080)  # box fps at 50 yards 
