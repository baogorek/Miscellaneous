# https://anythingbutrbitrary.blogspot.com/2012/06/random-regression-coefficients-using.html

library(lme4)
library(dplyr)

J <- 15
N <- 30
 
test_df <- data.frame(unit = sort(rep(c(1:N),J)), 
                      J = rep(c(1:J),N) , x = rnorm(n = J*N) )

#Next, we'll generate data from our above model, where βi ~ N(3, .22), αi = 1 for all i, and εij ~ N(0, .752). 

beta <- 3 + .75 * rnorm(N)
test_df$beta <- beta[test_df$unit]
test_df$y <- 1 + test_df$x * test_df$beta + 1.25 * rnorm(n = J*N)
head(test_df, 18)

alpha_hat <- c()
beta_hat <- c()
resid <- c()
for(i in 1:N){
  unit_lm <- lm(y ~ x, data = subset(test_df, unit == i) )
  resid <- c(resid, unit_lm$residuals)
  alpha_hat <- c(alpha_hat, as.numeric(coef(unit_lm)[1]))
  beta_hat <- c(beta_hat, as.numeric(coef(unit_lm)[2]))
}

re_lm <- lmer(y ~ x + (0 + x | unit), data = test_df, REML=FALSE) 
summary(re_lm)

# Starting_values
alpha_bar <- mean(alpha_hat)
beta_bar <- mean(beta_hat)
sigma_beta <- sd(beta_hat) * .75
sigma_e <- sd(resid)

b_hat <- beta_hat - beta_bar

theta <- c(b_hat, alpha_bar, beta_bar, sigma_beta, sigma_e)
get_negloglike <- function(theta) {

  b <- theta[1:N]
  alpha_bar <- theta[N + 1]
  beta_bar <- theta[N + 2]
  sigma_beta <- theta[N + 3]
  sigma_e <- theta[N + 4]

  loglike <- c()
  for (i in 1:N) {
    x_i <- test_df[test_df$unit == i, "x"]
    y_i <- test_df[test_df$unit == i, "y"]
    E_yi <- alpha_bar + x_i * (beta_bar + b[i])
    like_i_given_b_i <- dnorm(y_i, mean=E_yi, sd = sigma_e, log = TRUE)
    # NOTE: b_i appears in for multiple likelihood terms above,
    # but it is 1 param and contributes once to the likelihood
    like_b_i <- dnorm(b[i], mean=0, sd=sigma_beta, log=TRUE)
    loglike <- c(loglike, like_i_given_b_i, like_b_i)
  }
  -sum(loglike)
}

res <- optim(theta, get_negloglike,
             method = 'Nelder-Mead',
             control = list(maxit=100000, reltol=1E-17))

data.frame(
           beta_i = beta, beta_lm = beta_hat,
           like_b_i = res$par[N + 2] + res$par[1:N],
 	   beta_lmer = fixef(re_lm)[2] + ranef(re_lm)$unit$x)

cat("alpha_hat:", res$par[N + 1], "\n")
cat("beta_bar:", res$par[N + 2], "\n")
cat("sigma_beta:", res$par[N + 3], "\n")
cat("sigma_e:", res$par[N + 4], "\n")
cat("optim neglogLik:", res$value, "\n")

fixef(re_lm)
-logLik(re_lm)
