library(bayesm)

ind_max <- function(x) which(max(x) == x)

p <- 3
n <- 2000
na <- 2 # 2 alternative specific variables
beta <- c(-.35, .13, 0, 0) # zeroing out impact of alt-specific vars
# this does not seem to work well for small probabilities
Sigma <- matrix(c(1, .2, .2, 1), ncol = 2)

# 2 identical matricies made up of uniform random numbers
X1 <- matrix(runif(n * p, min = 0, max = 2), ncol = p)
X2 <- matrix(runif(n * p, min = 0, max = 2), ncol = p)
Xa <- cbind(X1, X2) # First three columns are var 1, second are var2
# DIFF = TRUE. p106: "common practice to subtract pth eq from first p-1 eqs
X  <- createX(p, na = na, nd = NULL, Xa = Xa, Xd = NULL, DIFF = TRUE)

# some matrix
Xbeta <- X %*% beta
epsilon_indep <- matrix(rnorm((p - 1) * n), ncol = n) # 2 * 500 random normals!
epsilon_depend <- t(chol(Sigma)) %*% epsilon_indep

w <- Xbeta + as.vector(epsilon_depend) # latent variable
w <- matrix(w, ncol = p - 1, byrow = TRUE) # Back to n x (p - 1)

max_w <- apply(w, 1, max) # 1 val for every subject
y <- apply(w, 1, ind_max) # 3 alternatives but we differenced so 2 in diff system
y <- ifelse(max_w < 0, p, y) # chose the 3rd alt where max rest has neg utility

# Evidence below that the simulation is working!
## The actual relative rates of each of these p alternatives:
print(table(y) / length(y))

## vs the model-expected probabilities 
for (j in 1:p) {
  print(exp(llmnp(beta, Sigma, X[1:2, ], y = j, r = 1E5)))
}

# Prior (using defaults)
betabar <- c(0, 0, 0, 0) 
A <- (.01) * diag(4)
nu <- (p - 1) + 3
V <- nu * diag(p - 1)

# Fit the model
out <- rmnpGibbs(Data = list(p = p, y = y, X = X),
                 Prior = list(betabar = betabar, A = A, nu = nu, V = V),
                 Mcmc = list(R = 100000, keep = 100, beta0 = c(0, 0, 0, 0)))

# An identifiable qauntity, betatilde:
betatilde <- out$betadraw  / sqrt(out$sigmadraw[, 1])

# Both look pretty bad
plot(out$betadraw[, 1]) 
plot(betatilde[, 1]) 

mean(tail(out$betadraw[, 1], 500))
mean(tail(betatilde[, 1], 500))

mean(tail(out$betadraw[, 2], 500))
mean(tail(betatilde[, 2], 500))



