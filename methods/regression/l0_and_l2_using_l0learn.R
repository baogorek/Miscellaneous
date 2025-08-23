# L0Learn Example
# https://cran.r-project.org/web/packages/L0Learn/vignettes/L0Learn-vignette.html
library(L0Learn)
library(MASS)
library(dplyr)

# --- Data Generation ---
set.seed(12543)
options(max.print = 100)


n <- 500
b0 <- 30
b1 <- 1
b2 <- 0
b3 <- -2
b4 <- 0

b <- c(b1, b2, b3, b4)
p <- length(b)

rho <- 0.5
sigma_X <- matrix(rho, nrow = p, ncol = p)
diag(sigma_X) <- 1
sigma_e <- 1.5

# Generate correlated predictors
X <- mvrnorm(n = n, mu = rep(0, p), Sigma = sigma_X)
colnames(X) <- paste0("x", 1:p)

# Generate response (no intercept in X, L0Learn adds it automatically)
y <- as.vector(b0 + X %*% b + sigma_e * rnorm(n))

# fit a path of solutions for the L0-regularized model with at most
# maxSuppSize non-zeros using coordinate descent (CD)

fit <- L0Learn.fit(X, y, penalty="L0", maxSuppSize=20)
print(fit)

# Corresponds to 2 variables ----
coef(fit, lambda=4.88437e-02, gamma=0)

predict(fit, newx=X, lambda=0.0325142, gamma=0)

# Visualizing the regularization path -----
plot(fit, gamma=0)

# Let's see if I can put a bunch of L2 on there 
coef(fit, lambda=4.88437e-02, gamma=1000)
## Hmm, no effect!

# Well, probably because the penalty was explicitly "L0"
fit <- L0Learn.fit(
    X, y,
    penalty="L0L2",  # Ok, let's make sure we're using L2 along with L0
    # The L2-grid: 5 gamma values on a log scale ranging from gammaMin to gammaMax
    nGamma = 5,
    gammaMin = 0.0001,
    gammaMax = 10,
    # End L2-grid
    maxSuppSize=6  # Now, just the max L0 parameter
)

options(max.print = 10000)

#> print(fit)  # Not ideal for printing
# Also I'm not aware of a way that's not just copying and pasting lambda
# Well, maybe the cross validation makes that easier


#         lambda        gamma suppSize
#1   9.87286e-03 10.000000000        0
#2   9.77413e-03 10.000000000        1
#3   1.80426e-03 10.000000000        1
#4   1.44341e-03 10.000000000        2
# ...
# 401 2.07289e-01  0.000100000        0
# 402 2.05216e-01  0.000100000        1
# 403 3.78820e-02  0.000100000        2
# 404 3.67455e-02  0.000100000        2


#  Wow, a very high L2 parameter competely squishes parameters, gets the wrong non-zeros
coef(fit,lambda=1.44341e-03, gamma=10)

# Just a touch of L2 works fine
coef(fit,lambda=3.78820e-02, gamma=0.0001)

# Ok, but can this procedure handle a generalized regression situation, say n > p


