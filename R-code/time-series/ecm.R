library(urca)
library(vars)

# Going after estimation of the model:
# (del y_t, del x_t)T = Alpha %*% BetaT %*% (y_t-1, x_t-1)T
#    + Gamma %*% (del y_t-1, del x_t-1)T + epsilon_t
set.seed(1342)
T <- 500
burned_in <- 100:T

# Note: change say the .6 to 0 in alpha and the system explodes
# Claim: There's no way to get unidirectional causality in a cointegrated system
# Even though: the shocks only go from x to y

beta <- 2  # y_t - beta * x_t is stationary
Alpha <- matrix(c(-.3, -.4, .6, .5), ncol=2, byrow=T)  # Pulled from an example VAR(1)
Beta_T <- matrix(c(1, -beta, -1 / beta, 1), ncol=2, byrow=T)
Gamma <- matrix(c(.5, .4, 0, .8), ncol=2, byrow=T)

# Generate data
X <- matrix(NA, nrow=T, ncol=2)
X[1:2, ] <- 0
for (t in 3:T) {
  epsilon_t <- rnorm(2, mean=0, sd=1)
  x_lag1 <- X[t - 1, ]
  del_x_lag1 <- X[t - 1, ] - X[t - 2, ]
  cat('---', t, '---\n')
  cat('x(t-1) =', x_lag1, ', dx(t-1)=', del_x_lag1, '\n-----\n')

  del_x <- Alpha %*% Beta_T %*% x_lag1 + Gamma %*% del_x_lag1 + epsilon_t
  X[t, ] <- x_lag1 + del_x
}

df <- as.data.frame(X[burned_in, ])
names(df) <- c('y', 'x')

plot(df$y, type='l')
lines(df$x, col = 'blue')

summary(lm(y ~ x, data = df))

jotest <- ca.jo(df,
                type="eigen",
                ecdet="none",
                K=2,
                spec="transitory")

summary(jotest)
jotest@V  # Alpha
jotest@W # t(Beta)

# These are the same
jotest@PI  # matrix mutliplying levels
jotest@V %*% t(jotest@W)
Alpha %*% Beta_T  # Not the same! TODO: figure out why

jotest@GAMMA  # This is very close

var <- vec2var(jotest)

# Almost identical! (Maybe it's the constant in jotest)
-var$A$A2
jotest@GAMMA[, 2:3]

-(diag(2) - var$A$A1 - var$A$A2)
jotest@PI

ir_x_to_y <- irf(var, impulse="x", response="y")
plot(ir_x_to_y)
ir_y_to_x <- irf(var, impulse="y", response="x")
plot(ir_y_to_x)


library(forecast)
auto.arima(df$x)
auto.arima(df$y)

