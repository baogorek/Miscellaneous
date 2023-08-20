# http://course.sdu.edu.cn/G2S/eWebEditor/uploadfile/20140110134920017.pdf
library(stats)
library(dplyr)

# Model 1: Regression with AR(1) errors ----------------
set.seed(123)
n <- 100 
x <- rnorm(n)  # Predictor variable
y <- 2 + 0.5 * x + arima.sim(model = list(ar = 0.7), n = n)

log_likelihood <- function(params) {
  beta <- params[1:2]  # Regression coefficients (intercept and slope)
  rho <- params[3]  # AR coefficient
  sigma2 <- params[4]  # Error variance

  # Initial likelihood term
  initial_term <- dnorm(y[1],
                        mean = beta[1] + beta[2] * x[1],
                        sd = sqrt(sigma2 / (1 - rho ^ 2)), log=TRUE)

  # Conditional likelihood terms
  neg_log_like <- -initial_term  # Initialize accumulator
  for (t in 2:n) {
    e_prev <- y[t - 1] - beta[1] - beta[2] * x[t - 1]
    e_t <- y[t] - beta[1] - beta[2] * x[t] - rho * e_prev 
    cond_term <-  dnorm(e_t,
                        mean = 0,
                        sd = sqrt(sigma2), log=TRUE)
    neg_log_like <- neg_log_like + -cond_term
  }
  neg_log_like
}

# Initial parameter values for optimization
initial_params <- c(4, .4, 0.5, .9)

# Maximize the likelihood using optim()
fit <- optim(initial_params, log_likelihood, method="L-BFGS-B",
             lower = c(0, -2, -.99, 0.1), upper = c(10, 2, .99, 100))
fit
fit$par
fit$value

model
model$loglik


# Model 2: Regression with ARMA(1, 1) errors ----------------
set.seed(123)
n <- 10
x <- rnorm(n)  # Predictor variable
y <- 2 + 0.5 * x + arima.sim(model = list(ar = 0.7, ma=-.3), n = n)

log_likelihood <- function(params) {
  beta <- params[1:2]  # Regression coefficients (intercept and slope)
  rho <- params[3]  # AR coefficient
  theta <- params[4] # MA coefficient
  sigma2 <- params[5]  # Error variance

  # Initial likelihood term
  initial_term <- dnorm(y[1] - beta[1] - beta[2] * x[1],
                        mean = 0,
                        sd = sqrt((1 + theta ^ 2) * sigma2 / (1 - rho ^ 2)),
                        log=TRUE)
  # initializing error sequence epsilon_t = rho * epsilon_{t-1} + eta_t + eta_{t-1}
  epsilon_tm1 = y[1] - beta[1] - beta[2] * x[1] # initialize AR epsilon as fn of y1
  eta_tm1 <- epsilon_tm1
  # Conditional likelihood terms
  neg_log_like <- -initial_term  # Initialize accumulator
  for (t in 2:n) {
    # express both epsilon_t and eta_t as fn of y1,...,yn
    epsilon_t <- y[t] - beta[1] - beta[2] * x[t]
    eta_t = epsilon_t - rho * epsilon_tm1 - theta * eta_tm1

    # likelihood term uses eta_t as fn of y1,...,yn
    cond_term <-  dnorm(eta_t,
                        mean = 0,
                        sd = sqrt(sigma2),
                        log=TRUE)
    neg_log_like <- neg_log_like + -cond_term
    # prep for the recursion in the next step
    epsilon_tm1 <- epsilon_t
    eta_tm1 <- eta_t
  }
  neg_log_like
}

# Initial parameter values for optimization
initial_params <- c(4, .4, 0.5, -.5, .9)
fit <- optim(initial_params, log_likelihood, method="L-BFGS-B",
             lower = c(-10, -2, -.99, -.99, 0.1), upper = c(10, 2, .99, .99, 100))
fit

model <- arima(y, order=c(1, 0, 1), xreg=x)
model




### Now let's try conditional ML using OLS regression
df <- data.frame(t = 1:n, y = y, x = x)
df$lag_x <-



# Model 2: Regression with AR(1) errors and variable lag term ----------------
set.seed(123)
n <- 100 
x <- rnorm(n)  # Predictor variable
y <- 2 + 0.5 * x + arima.sim(model = list(ar = 0.7), n = n)




log_likelihood <- function(params) {
  beta <- params[1:2]  # Regression coefficients (intercept and slope)
  rho <- params[3]  # AR coefficient
  sigma2 <- params[4]  # Error variance

  # Initial likelihood term
  initial_term <- dnorm(y[1],
                        mean = beta[1] + beta[2] * x[1],
                        sd = sqrt(sigma2 / (1 - rho ^ 2)), log=TRUE)

  # Conditional likelihood terms
  neg_log_like <- -initial_term  # Initialize accumulator
  for (t in 2:n) {
    e_prev <- y[t - 1] - beta[1] - beta[2] * x[t - 1]
    e_t <- y[t] - beta[1] - beta[2] * x[t] - rho * e_prev 
    cond_term <-  dnorm(e_t,
                        mean = 0,
                        sd = sqrt(sigma2), log=TRUE)
    neg_log_like <- neg_log_like + -cond_term
  }
  neg_log_like
}

# Initial parameter values for optimization
initial_params <- c(4, .4, 0.5, .9)

# Maximize the likelihood using optim()
fit <- optim(initial_params, log_likelihood, method="L-BFGS-B",
             lower = c(0, -2, -.99, 0.1), upper = c(10, 2, .99, 100))
fit
fit$par
fit$value

# Almost the same exact question:
# https://stats.stackexchange.com/questions/77663/arima-estimation-by-hand

set.seed(456)


## Simulate Arima
y <- arima.sim(n = 250, list(ar = 0.3, ma = 0.7), mean = 5)
plot(y)

## Optimize Log-Likelihood for ARIMA

n = length(y) ## Count the number of observations
e = rep(1, n) ## Initialize e

logl <- function(mx){
  
  g <- numeric
  mx <- matrix(mx, ncol = 4)
  
  mu <- mx[,1] ## Constant Term
  sigma <- mx[,2] 
  rho <- mx[,3] ## AR coeff
  theta <- mx[,4] ## MA coeff
  
  e[1] = 0 ## Since e1 = 0
  
  for (t in (2 : n)){
    e[t] = y[t] - mu - rho*y[t-1] - theta*e[t-1]
  }
  
  ## Maximize Log-Likelihood Function 
  g1 <-  (-((n)/2)*log(2*pi) - ((n)/2)*log(sigma^2+0.000000001) - (1/2)*(1/(sigma^2+0.000000001))*e%*%e)
  
  ##note: multiplying Log-Likelihood by "-1" in order to maximize in the optimization
  ## This is done becuase Optim function in R can only minimize, "X"ing by -1 we can maximize
  ## also "+"ing by 0.000000001 sigma^2 to avoid divisible by 0
  g <- -1 * g1
  
  return(g)
  
}

## Optimize Log-Likelihood
arimopt <- optim(par=c(10,0.6,0.3,0.5), fn=logl, gr = NULL,
                 method = c("L-BFGS-B"),control = list(), hessian = T)
arimopt



