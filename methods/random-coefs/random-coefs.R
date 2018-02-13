# Example from http://anythingbutrbitrary_blogspot_com/2012/06/random-regression-coefficients-using_html
library(lme4)

J <- 15
N <- 30
 
test_df <- data.frame(unit = sort(rep(1:N, J)), 
                      j = rep(1:J, N),
                      x = rnorm(n = J * N))

beta <- 3 + .2 * rnorm(N)
test_df$beta <- beta[test_df$unit]
test_df$y <- 1 + test_df$x * test_df$beta + .75 * rnorm(n = J * N)

# If we estimated the regressions separately
beta_hat <- c()
for(i in 1:N){
  unit_lm <- lm(y ~ x, data = subset(test_df, unit == i))
  beta_hat[i] <- coef(unit_lm)[["x"]]
}

re_lm <- lmer(y ~ x + (1 + x | unit), data = test_df) 

summary(re_lm)

re_beta <- coef(re_lm)$unit[,"x"]
re_beta <- coef(re_lm)$unit[,"(Intercept)"]

ols_lm <- lm(y ~ x, data = test_df)
summary(ols_lm)


write.csv(test_df, file = "C:/devl/re_test.csv", row.names = FALSE)
