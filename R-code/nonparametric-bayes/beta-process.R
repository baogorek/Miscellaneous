K <- 100000 # Set higher for more accurate approximation to BP

alpha <- 5
gamma <- 1


pi_vec <- rbeta(K, alpha * gamma / K, alpha * (1 - gamma / K))
hist(pi_vec)

atoms <- rnorm(K)

# The beta process apparently does not induce a prob meas
x_seq <- seq(-5.5, 5.5, .01)

F_t <- c()

for (t in x_seq) {

  F_t <- c(F_t, sum(pi_vec * (atoms <= t)))

}

plot(F_t ~ x_seq)
lines(pnorm(x_seq) ~ x_seq, col = "blue")

# Random binary matrix - N draws from Bernoilli process
N <- 1000

Z <- matrix(0, N, K)
for (i in 1:N) {
  Z[i, ] <- rbinom(rep(1, K), rep(1, K), pi_vec)
}


table(rowSums(Z))

# Sufficient statistics
m1 <- colSums(Z)
m0 <- N - colSums(Z)

# Because representation  in (2) is exchangeable
m1_star <- sort(m1[m1 > 0], decreasing = TRUE)
