library(MASS)
library(dplyr)
library(torch)

# Sample size
n <- 100

# Parameters of the hard concrete distribution
alpha <- 1 
beta <- 0.5
lambda <- -0.1
zeta <- 1.1

# Parameters of the regression

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

raw_data <- mvrnorm(n = n, mu = rep(0, p), Sigma = sigma_X)
df <- as.data.frame(raw_data)
colnames(df) <- paste0("x", 1:p)

df["y"] <- b0 + df$x1 * b1 + df$x2 * b2 + df$x3 * b3 + df$x4 * b4 + sigma_e * rnorm(n)
summary(lm(y ~ x1 + x2 + x3 + x4, data = df))

sample_z <- function(alpha, beta, zeta, gamma) {
    stopifnot(length(alpha) == 4)
    stopifnot(length(beta) == 1)
    # Generating the data
    u <- runif(p)  # I'm going to try to apply it to just the regression coefficients
    # Below draws from the logistic distributions with mean log(alpha) / beta
    # X ~ Logistic(mu, s) can be represented as X = mu + s * (log (U) - log(1 - U)), U ~ Uniform
    X <- log(u / (1 - u)) / beta + log(alpha) / beta
    # Side note, thei difference of two Gumbels is Logistic distributed
    # Ordinary concrete always lives wtihin 0, 1, allows for the reparameterization trick
    s <- 1 / (1 + exp(-X))
    # Hard concrete stretches range outside 0, 1, then clamps to produce point masses
    s_bar <- s * (zeta - gamma) + gamma 
    #z <- pmin(1, pmax(0, s_bar))
    z <- torch_clamp(s_bar, min = 0, max = 1)
    z
}

complexity_loss <- function(alpha, beta, zeta, gamma) {
    # I think we should really only need alpha
    torch_sum(1 / (1 + exp(log(alpha) - beta * log(-gamma / zeta))))

}

# Initialize

b0_parm <- torch_tensor(mean(df$y), requires_grad = TRUE)
b_parm <- torch_tensor(.1 * runif(p), requires_grad = TRUE)
alpha <- torch_tensor(rep(3, p), requires_grad = TRUE)  # TODO: think about initialization
beta <- 1  # I shouldn't need this to be a tensor
gamma <- -0.1
zeta <- 1.1
lambda <- 1e-3

epochs <- 1500

opt <- optim_sgd(list(b0_parm, b_parm, alpha), lr = 0.01)

for (k in 1:epochs) {
  idx <- sample.int(n) # random row order
  epoch_loss <- c()
  for (i in idx) {
    x_i <- torch_tensor(as.numeric(df[i, paste0("x", 1:p)]))
    y_i <- torch_tensor(as.numeric(df[i, "y"]))
    z <- sample_z(alpha, beta, zeta, gamma)
    b_star <- b_parm * z
    y_hat_i <- b0_parm + torch_dot(b_star, x_i)

    loss <- (y_i - y_hat_i)$pow(2) + lambda * complexity_loss(alpha, beta, zeta, gamma)

    opt$zero_grad()
    loss$backward()
    opt$step()
    epoch_loss <- c(epoch_loss, as.numeric(loss))

  }
  if (k %% 10 == 0) {
    print(k)
    cat("alpha", as.numeric(alpha), "\n")
    cat("b_parm", as.numeric(b_parm), "\n")
    cat("b_star", as.numeric(b_star), "\n")
    cat("loss", mean(epoch_loss), "\n")
  }
}

alpha_final <- as.numeric(alpha)
sigmoid_final <- 1 / (1 + exp(-log(alpha_final)))
sigmoid_final2 <- 1 / (1 + 1 / alpha_final)  # same thing

z_final <- pmin(1, pmax(0, sigmoid_final * (zeta - gamma) + gamma))
b_parm_final <- z_final * as.numeric(b_parm)

print(z_final)
print(b_parm_final)
