# LEARNING SPARSE NEURAL NETWORKS THROUGH L0 REGULARIZATION
# https://arxiv.org/pdf/1712.01312
library(MASS)
library(dplyr)
library(torch)

# Data generating process ------- 
n <- 100

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


# Onto the L0 part -----------------------

sample_z <- function(log_alpha, beta, zeta, gamma) {
    # Generating the data
    p <- as.integer(log_alpha$numel())
    # u <- runif(p)
    eps <- 1e-6
    u <- torch_rand(p)$clamp(eps, 1 - eps)
    # Below draws from the logistic distributions with mean log(alpha) / beta
    # X ~ Logistic(mu, s) can be represented as X = mu + s * (log (U) - log(1 - U)), U ~ Uniform
    # X <- log(u / (1 - u)) / beta + log(alpha) / beta
    X <- (torch_log(u) - torch_log(1 - u) + log_alpha) / beta
    # Side note, the difference of two Gumbels is Logistic distributed
    # Ordinary concrete always lives wtihin 0, 1, allows for the reparameterization trick
    # s <- 1 / (1 + exp(-X))
    s <- torch_sigmoid(X)
    # Hard concrete stretches range outside 0, 1, then clamps to produce point masses
    s_bar <- s * (zeta - gamma) + gamma 
    # z <- pmin(1, pmax(0, s_bar))
    z <- s_bar$clamp(0, 1)
    z
}

complexity_loss <- function(log_alpha, beta, zeta, gamma) {
    # I think we should really only need alpha
    c <- -beta * log(-gamma / zeta)  # scalar
    pi <- torch_sigmoid(log_alpha + c)
    pi$sum() 
}

init_log_alpha <- function(keep_prob, size, loc_sd = 0.01) {
  # Convert keep-probability (p) into the corresponding logit.
  # This is the mean mu around which we initialize log_alpha.
  # logit(p) = log(p / (1 - p))
  mu <- log(keep_prob / (1 - keep_prob))

  # Now add a tiny bit of Gaussian noise around mu.
  # This breaks symmetry so not all gates start with the exact same probability.
  # stddev = 0.01 as in the paper.
  init_vals <- rnorm(size, mean = mu, sd = loc_sd)

  # Convert to torch tensor and mark as learnable (requires_grad = TRUE)
  log_alpha <- torch_tensor(init_vals, requires_grad = TRUE)

  return(log_alpha)
}

# Notes on the model:
# The expected number of active gates is:
# Pr(Z > 0) = Sigmoid(log(alpha) - beta * log(-gamma / zeta))
# We use that in the lambda initiazliation above to set alpha given a probability


# Initialize

b0_parm <- torch_tensor(mean(df$y), requires_grad = TRUE)
b_parm <- torch_tensor(.1 * runif(p), requires_grad = TRUE)

# The only trainable L0 parameter is alpha, essentially the logit bias to keep the gate on
log_alpha <- init_log_alpha(keep_prob = 0.5, size = p)
# Architectural constants that shape distribution to approximate a discrete on/off switch after clipping
beta <- 2 / 3
gamma <- -0.1
zeta <- 1.1

# Getting a good value for lambda
sigma2_hat <- var(resid(lm(y ~ x1 + x2 + x3 + x4, data = df)))
lambda <- as.numeric(0.1 * sigma2_hat)

# Bringing the data into torch form
x_cols <- paste0("x", 1:p)
X <- torch_tensor(as.matrix(df[, x_cols, drop = FALSE]), dtype = torch_float())
y <- torch_tensor(df$y, dtype = torch_float())

epochs <- 5000

opt <- optim_adam(list(b0_parm, b_parm, log_alpha), lr = 1e-2)  # tune lr

for (k in 1:epochs) {
  # one MC sample per forward pass is exactly what the paper does
  z <- sample_z(log_alpha, beta, zeta, gamma)
  b_star <- b_parm * z

  y_hat <- b0_parm + X$matmul(b_star$unsqueeze(2))$squeeze()
  data_loss <- (y - y_hat)$pow(2)$mean()
  comp <- complexity_loss(log_alpha, beta, zeta, gamma)
  loss <- data_loss + lambda * comp

  opt$zero_grad(); loss$backward(); opt$step()

  if (k %% 50 == 0) {
    with_no_grad({
      c <- -beta * log(-gamma / zeta)
      pi <- torch_sigmoid(log_alpha + c)        # P(gate ON)
      cat(sprintf("epoch %d  loss=%.4f  comp=%.2f  pi=%s\n",
                  k, as.numeric(loss), as.numeric(comp),
                  paste(round(as.numeric(pi), 2), collapse=", ")))
    })
  }
}

with_no_grad({
  # For reference (not strictly needed for gating):
  alpha_final <- torch_exp(log_alpha)

  # Probability a gate is ON (good for reporting/pruning)
  c <- -beta * log(-gamma / zeta)
  pi_final <- torch_sigmoid(log_alpha + c)

  # Deterministic gate (u = 0.5) through hard-concrete stretch + clamp
  # (Recall this is the logistic iwthout the log(u) + log(1 - u), u ~ Uniform(0, 1) and temperature bate
  # The final gate is simply the pointwise expectation of the stretched sigmoid, clipped to [0, 1]
  # At test time: there’s no relaxation — you just take a deterministic gate from log(alph)
  # so beta (temperature) disappears. There's no softening left to do the regularize random samples
  z_final <- ((log_alpha / beta)$sigmoid() * (zeta - gamma) + gamma)$clamp(0, 1)

  # Gated coefficients as tensors (don’t coerce until printing)
  b_parm_final <- b_parm * z_final

  # --- Pretty printing (convert to R numerics only here) ---
  cat("alpha:        ", paste(round(as.numeric(alpha_final), 4), collapse = ", "), "\n")
  cat("pi (P>0):     ", paste(round(as.numeric(pi_final),   4), collapse = ", "), "\n")
  cat("z_final:      ", paste(round(as.numeric(z_final),    4), collapse = ", "), "\n")
  cat("b_parm_final: ", paste(round(as.numeric(b_parm_final),4), collapse = ", "), "\n")
})

