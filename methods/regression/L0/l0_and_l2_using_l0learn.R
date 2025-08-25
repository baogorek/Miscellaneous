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
# Look down below, claude code found one.
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

# Ok, but can this procedure handle a generalized regression situation, say n < p?
# Yes, it can

# Test underdetermined case (n < p)
n <- 50
p <- 200
b <- rep(0, p)
b[c(5, 20, 35, 80, 150)] <- c(2, -3, 1.5, -1, 0.8)

X <- mvrnorm(n = n, mu = rep(0, p), Sigma = diag(p))
y <- as.vector(10 + X %*% b + 0.5 * rnorm(n))

fit_underdetermined <- L0Learn.fit(X, y, penalty="L0", maxSuppSize=10)
print(fit_underdetermined)

dim(X)
length(y)

# LM doesn't fail, but you can see there are a bunch of NAs
df <- data.frame(y = y, X)
lm(y ~ ., data = df)


lambda_5 <- fit_underdetermined$lambda[[1]][which(fit_underdetermined$suppSize[[1]] == 5)[1]]
coef_5 <- coef(fit_underdetermined, lambda = lambda_5, gamma = 0)

cat("n =", n, ", p =", p, "→ underdetermined\n")
cat("True support:", which(b != 0), "\n")
cat("L0 found:", which(abs(coef_5[-1, 1]) > 1e-10), "\n")

# Remaining: keeping coefficients positive, removing the intercept
# Yeah, it looks like we can. See page 12: https://cran.r-project.org/web/packages/L0Learn/L0Learn.pdf

#intercept: If FALSE, no intercept term is included in the model.

# lows: Lower bounds for coefficients. Either a scalar for all coefficients to have the
# same bound or a vector of size p (number of columns of X) where lows[i] is the
# lower bound for coefficient i.

# highs: Upper bounds for coefficients. Either a scalar for all coefficients to have the
# same bound or a vector of size p (number of columns of X) where highs[i] is the
# upper bound for coefficient i.

# And finally, we can use our relative mean squared error function? Again, yes, but not
# out of the box. We will have to transform the data to get the effect. From Claude Code:

#  Goal: Minimize Σ[(ŷᵢ - yᵢ)² / (yᵢ + c)²] using standard squared error regression
#
#  Transformation:
#  1. Scale each observation by dividing by √(yᵢ + c):
#    - X'ᵢ = Xᵢ / √(yᵢ + c)
#    - y'ᵢ = yᵢ / √(yᵢ + c)
#  2. Run standard regression on (X', y') to get coefficients β
#  3. For predictions on new data:
#    - Use original scale: ŷ = X × β
#
#  Why it works:
#  - Standard regression minimizes: Σ(X'ᵢβ - y'ᵢ)²
#  - Substituting: Σ[(Xᵢβ - yᵢ)² / (yᵢ + c)]
#  - This is exactly the relative squared error with constant
#
#  Implementation notes:
#  - Choose c based on your data (e.g., 1, 0.01×mean(y), or smallest non-zero y)
#  - The √ in the transformation is key - using 1/(yᵢ + c) would be incorrect
#  - Each row gets its own scaling factor based on its target value
#  - The learned coefficients β apply directly to original-scale features
#


library(L0Learn)

Q <- 5000  # number of targets/samples
N <- 50000  # number of households/features - There will be this many weights
set.seed(34543)

# Metric matrix M (shape Q x N) - underdetermined system
M <- matrix(rlnorm(Q * N, meanlog=1.5, sdlog=.25), nrow = Q, ncol = N)

# Let's make a true w so we know that the target is in the space spanned by w
w_true <- rlnorm(N, meanlog=2, sdlog=1)

# Target vector y
y <- as.numeric(M %*% w_true)

# Fit with L0 and non-negative constraints
fit <- L0Learn.fit(
    M,
    y,
    penalty = "L0",
    maxSuppSize = 3000,  # Number that can be non-negative, but it will be a grid 
    # USER NOTE: toggle the following off after running through this once
    #lows = 0.0,  # Add non-negative box constraint
    highs = Inf,
    intercept = FALSE,
)

options(max.print = 1000)
print(fit)  # USER NOTE: look at the number of non-zero weights


single_lambda=4.14795e-11  # You can look at print(fit), but I choose a very small value
w_hat <- as.numeric(coef(fit, lambda=single_lambda, gamma=0))

sum(w_hat != 0)


w_hat[w_hat > 0]

summary(w_hat)

# Let's get an r-squared
y_hat <- as.numeric(predict(fit, newx=M, lambda=single_lambda, gamma=0))
cor(as.numeric(y_hat), y)



