library(urca)
library(vars)

# Going after estimation of the model:
# (del y_t, del x_t)T = Alpha %*% BetaT %*% (y_t-1, x_t-1)T
#    + Gamma %*% (del y_t-1, del x_t-1)T + epsilon_t
set.seed(1342)

T <- 5000
x_t1 <- c(100, 80)
x_t2 <- c(110, 85)

burned_in <- (.05 * T):T
# Note: change say the .6 to 0 in alpha and the system explodes
# Claim: There's no way to get unidirectional causality in a cointegrated system
# Even though: the shocks only go from x to y

beta <- 1.3  # y_t - beta * x_t is stationary
Alpha <- matrix(c(.3, .4), ncol=1)
Beta_T <- matrix(c(1, -beta), ncol=2)
Gamma <- matrix(c(.5, .4, 0, .8), ncol=2, byrow=T)

# Generate data
X <- matrix(NA, nrow=T, ncol=2)
X[1, ] <- x_t1
X[2, ] <- x_t2 
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
low <- min(c(df$y, df$x))
high <- max(c(df$y, df$x))

plot(df$y, type = 'l', ylim = c(low, high))
lines(df$x, col = 'blue')

summary(lm(y ~ x, data = df))

jotest <- ca.jo(df,
                type="eigen",
                ecdet="none",
                K=2,
                spec="transitory")

summary(jotest)

# Lag matrix multiplier recovery - Pretty Good!
jotest@PI
jotest@W %*% t(jotest@V)  # Alpha %*% Beta^T
Alpha %*% Beta_T

# Can I recover Alpha and Beta?
# Remember, ca.jo returns matrices but in texts like
# https://faculty.washington.edu/ezivot/econ584/notes/cointegration.pdf
# Alpha is n x r and Beta_T is r x n

# Matching Beta
jotest@V[, 1]  # Beta
Beta_T

# Matching Alpha
jotest@W[, 1]  # Alpha
t(Alpha)

t(jotest@V)
Beta_T
# The first column is spot on, but the second appears to have been normalized
jotest@W
Alpha

# Matching the matrix multiplying the lag deltas
Gamma
jotest@GAMMA  # This is very close


# Using a VAR representation of a VEC
# x_t = A1 * x_{t - 1} + A2 * x_{t - 2} 
var <- vec2var(jotest)
# Almost identical! (Maybe it's the constant in jotest)

# Gamma *is* -A2
-var$A$A2
jotest@GAMMA[, 2:3]
Gamma

# The relationship of A1 and A2 of the var and PI of the VECM
-(diag(2) - var$A$A1 - var$A$A2)
jotest@PI

# Recovering A1
diag(2) + Gamma + Alpha %*% Beta_T
var$A$A1

# Studying impulse response

# Here's what comes out of the vars package:
ir_x_to_y <- irf(var, impulse="x", response="y", n.ahead = 40)
plot(ir_x_to_y)

ir_x_to_x <- irf(var, impulse="x", response="x", n.ahead = 40)
plot(ir_x_to_x)

ir_y_to_x <- irf(var, impulse="y", response="x", n.ahead = 40)
plot(ir_y_to_x)

# I'm going to shock the system at period 3

T <- 50
x0 <- 100
shock <- 1
offset <- 0
# Shock x and run Generate Data block
x_t1 <- c(offset + beta * x0, x0)  # start at equilibrium
x_t2 <- c(offset + beta * x0, x0 + shock)  # shock in x only

# or Shock y and run Generate Data block
#x_t1 <- c(offset + beta * x0, x0)  # start at equilibrium
#x_t2 <- c(offset + beta * x0 + shock, x0)  # shock in x only

# Generate data
X <- matrix(NA, nrow=T, ncol=2)
X[1, ] <- x_t1
X[2, ] <- x_t2 
X[1, ] <- c(0, 0)
X[2, ] <- c(0, 1)
for (t in 3:T) {
  x_lag1 <- X[t - 1, ]
  del_x_lag1 <- X[t - 1, ] - X[t - 2, ]
  del_x <- Alpha %*% Beta_T %*% x_lag1 + Gamma %*% del_x_lag1
  X[t, ] <- x_lag1 + del_x
}

df <- as.data.frame(X)
names(df) <- c('y', 'x')
par(mfrow=c(2, 1))
plot(df$y, main="Effect on y_t of x's impulse at t=2")
plot(df$x, main="Effect on x_t of x's impulse at t=2")

library(Matrix)
A2 <- -Gamma
A1 <- diag(2) + Gamma + Alpha %*% Beta_T

# Check
var$A$A1
A1

var$A$A2
A2

# Transition matrix (State Space Representation of Var2 Representation of VECM!)
F <- matrix(rep(0, 16), nrow=4, ncol=4)
F[1:2, 1:2] <- A1
F[1:2, 3:4] <- A2
F[3:4, 1:2] <- diag(2)

u <- matrix(c(0, 1, 0, 0), ncol=1)  # unit shock in x_t

# Spectral Decomposition of F
r <- eigen(F)
P <- r$vectors
D <- diag(r$values)
P_inv <- solve(P)

# Check decomposition: Imaginary components are approx 0
round(Re(P %*% D %*% P_inv), 4)
F

# Send out to h power
irf_y_to_x_shock <- c()
irf_x_to_x_shock <- c()
for (h in 1:30) {
    F_h <- round(Re(P %*% D ^ h %*% P_inv), 4)
    F_h
    response <- F_h %*% u
    irf_y_to_x_shock <- c(irf_y_to_x_shock, response[1])
    irf_x_to_x_shock <- c(irf_x_to_x_shock, response[2])
}

par(mfrow=c(2, 1))
plot(irf_y_to_x_shock, main="Effect on y_t of x's impulse at t=2")
plot(irf_x_to_x_shock, main="Effect on x_t of x's impulse at t=2")


# We'll do the same thing without the spectral decomposition, just powers
mat_power <- function(M, pow) {
  Result <- M
  while (pow > 1) {
    Result <- Result %*% M 
    pow <- pow - 1
  }
  Result 
}
  

irf_y <- c()
irf_x <- c()
for (j in 2:30) {
    F_j <- mat_power(F, j) %*% u
    irf_y <- c(irf_y, F_j[1])
    irf_x <- c(irf_x, F_j[2])
}
plot(irf_x)
plot(irf_y)
