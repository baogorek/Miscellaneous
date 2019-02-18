
n <- 200
p <- 3

beta <- c(1, -1, 1.5, 0.5) # the only parameters in this specification
cat("Theoretical Probs are", exp(beta) / sum(exp(beta)), "\n")

# Simulate data
X1 <- matrix(runif(n * p, min = -1, max = 1), ncol = p) # Alt-specific var 1
X2 <- matrix(runif(n * p, min = -1, max = 1), ncol = p) # Alt-specific var 2
Xa <- cbind(X1, X2) # Alternative-specific X matrix [alt 1 vars | alt 2 vars]
X <- createX(p, na = 2, nd = NULL, Xd = NULL, Xa = Xa, base = 1) # n*p rows

E_w_vec <- X %*% beta # n*p x 1 vector
E_w_mat <- matrix(E_w_vec, byrow = TRUE, ncol = p) # n x 3 matrix

Pr_mat <- exp(E_w_mat) / as.numeric(exp(E_w_mat) %*% rep(1, p))

# Make sure rows are probabilities
all(abs(rowSums(Pr_mat) - 1) < 1E-6)

y <- sapply(1:n, function(i) which.max(rmultinom(1, 1, Pr_mat[i, ])))

# Priors
betabar <- c(0, 0, 0, 0)
A <- (.01) * diag(4)

out <- rmnlIndepMetrop(Data  = list(y = y, X = X, p = 3),
                       Prior = list(betabar = betabar, A = A),
                       Mcmc  = list(R = 5000, beta0 = c(0, 0, 0, 0)))

summary(out$betadraw, tvalues = beta)

plot(out$betadraw)
