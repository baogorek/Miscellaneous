library(dplyr)
library(tidyr)
library(lavaan)

options(width=200)

# User input -------------------
N <- 1000
pulse_sample_sizes <- 6:40
factor_corr <- .3
k_factors <- 3  # NOTE: leave fixed at 3, sim not flexible enough currently
items_per_factor <- 4  # NOTE: leave fixed at 4, sim not flexible enough currently
loading_pattern <- c(1, .8, .6, .4)
intercept_pattern <- c(.5, -.5, 0, 0)
res_var_pattern <- c(.8, .8, .4, .4)

# Generating the data ---------
factor_cov <- matrix(factor_corr, k_factors, k_factors)
diag(factor_cov) <- rep(1, times=k_factors)
res_cov <- diag(rep(res_var_pattern, times=k_factors))

L_mat <- MASS::mvrnorm(N, mu = rep(0, times=k_factors), Sigma=factor_cov)

loadings <- rep(loading_pattern, 3)
means <- rep(intercept_pattern, 3)
manifest_vars <- rep(res_var_pattern, 3)

long_df <- data.frame()
for (i in 1:N) {
  # String out the L in a vector so the indexing corresponds to questions
  # This works because there are no cross-loadings
  L_vec <- c(
    rep(L_mat[i, 1], 4),
    rep(L_mat[i, 2], 4),
    rep(L_mat[i, 3], 4)
  )
  # variable number of responses, randomly selected each time
  k_questions <- sample(pulse_sample_sizes, size=1)
  pulse_questions <- sample(1:12, size=k_questions, replace=TRUE)
  for (j in pulse_questions) {
    manifest <- (
      means[j] +
      loadings[j] * L_vec[j] +
      rnorm(1, sd=sqrt(manifest_vars[j]))
    )
    long_df <- rbind(long_df,
      data.frame(i=i, question=j, manifest=manifest))
  }
}

# Likelihood creation and helper functions ------------------

create_block_matrix1 <- function(lambda_vec, sigma_sq_vec) {
  stopifnot(length(lambda_vec) == 4)
  stopifnot(length(sigma_sq_vec) == 4)
  matrix(c(
    # Row 1
    lambda_vec[1] ^ 2 + sigma_sq_vec[1], lambda_vec[1] * lambda_vec[2],
      lambda_vec[1] * lambda_vec[3], lambda_vec[1] * lambda_vec[4],
    # Row 2
    lambda_vec[1] * lambda_vec[2], lambda_vec[2] ^ 2 + sigma_sq_vec[2],
      lambda_vec[2] * lambda_vec[3], lambda_vec[2] * lambda_vec[4],
    # Row 3
    lambda_vec[1] * lambda_vec[3], lambda_vec[2] * lambda_vec[3],
      lambda_vec[3] ^ 2 + sigma_sq_vec[3], lambda_vec[3] * lambda_vec[4],
    # Row 4
    lambda_vec[1] * lambda_vec[4], lambda_vec[2] * lambda_vec[4],
      lambda_vec[3] * lambda_vec[4], lambda_vec[4] ^ 2 + sigma_sq_vec[4]),
    byrow=TRUE, nrow=4)
}

create_block_matrix2 <- function(lambda_vec1, lambda_vec2, latent_cov) {
  stopifnot(length(lambda_vec1) == 4)
  stopifnot(length(lambda_vec2) == 4)
  latent_cov * matrix(c(
    # Row 1
    lambda_vec1[1] * lambda_vec2[1], lambda_vec1[2] * lambda_vec2[1],
      lambda_vec1[3] * lambda_vec2[1], lambda_vec1[4] * lambda_vec2[1],
    # Row 2
    lambda_vec1[1] * lambda_vec2[2], lambda_vec1[2] * lambda_vec2[2],
      lambda_vec1[3] * lambda_vec2[2], lambda_vec1[4] * lambda_vec2[2],
    # Row 3
    lambda_vec1[1] * lambda_vec2[3], lambda_vec1[2] * lambda_vec2[3],
      lambda_vec1[3] * lambda_vec2[3], lambda_vec1[4] * lambda_vec2[3],
    # Row 4
    lambda_vec1[1] * lambda_vec2[4], lambda_vec1[2] * lambda_vec2[4],
      lambda_vec1[3] * lambda_vec2[4], lambda_vec1[4] * lambda_vec2[4]),
    byrow=TRUE, nrow=4)
}

create_covariance_matrix <- function(loadings, manifest_variances, latent_covs) {
   diag1 <- create_block_matrix1(loadings[1:4], manifest_variances[1:4])
   diag2 <- create_block_matrix1(loadings[5:8], manifest_variances[5:8])
   diag3 <- create_block_matrix1(loadings[9:12], manifest_variances[9:12])
   
   offdiag1 <- create_block_matrix2(loadings[1:4], loadings[5:8], latent_covs[1])
   offdiag2 <- create_block_matrix2(loadings[1:4], loadings[9:12], latent_covs[2])
   offdiag3 <- create_block_matrix2(loadings[5:8], loadings[9:12], latent_covs[3])

   cov_mat <- Matrix::bdiag(list(diag1, diag2, diag3))
   # Notice the order of the indexes
   cov_mat[5:8, 1:4] = offdiag1
   cov_mat[9:12, 1:4] = offdiag2
   cov_mat[9:12, 5:8] = offdiag3

   cov_mat[1:4, 5:8] = t(offdiag1)
   cov_mat[1:4, 9:12] = t(offdiag2)
   cov_mat[5:8, 9:12] = t(offdiag3)
   
   cov_mat <- as.matrix(cov_mat)

   stopifnot(all(cov_mat == t(cov_mat)))
   cov_mat
}

means_df <- long_df %>%
  group_by(i, question) %>%
  summarize(x = mean(manifest)) %>%
  pivot_wider(id_cols=i, names_from="question", names_prefix="xbar", values_from="x")

n_df <- long_df %>%
  group_by(i, question) %>%
  summarize(n=n()) %>%
  pivot_wider(id_cols=i, names_from="question", names_prefix="n", values_from="n") %>%
  replace(is.na(.), 0)

wide_df <- means_df[, c("i", paste0("xbar", 1:12))] %>%
  inner_join(n_df[, c("i", paste0("n", 1:12))], by="i")


# Lavaan is definitely doing something smart
loadings_start <- rep(c(1, .8, .6, .4), 3) + .1 * rnorm(12)
latent_covs_start <- rep(0, 3)
manifest_variances_start <- rep(c(.8, .8, .4, .4), 3) + .05 * rnorm(12)
manifest_means_start <- colMeans(wide_df[, paste0("xbar", 1:12)], na.rm=TRUE)

# Match the pattern of ptmodels as much as possible
params_start <- as.numeric(c(
  loadings_start,
  latent_covs_start,
  log(manifest_variances_start),
  manifest_means_start
))

get_neg_log_likelihood <- function(params) {
   # using wide_df of means and sample sizes from the global environment
   loadings <- params[1:12]
   latent_covs <- params[13:15]
   manifest_variances <- exp(params[16:27])
   manifest_means <- params[28:39]
   #print(paste("params <- c(", paste(params, collapse=','), ")\n"))  # DEBUG

   # The diagonals will be wrong, and we will have to make adjustments below
   Sigma_template <- create_covariance_matrix(loadings, manifest_variances, latent_covs)

   n <- nrow(wide_df)
   log_likelihoods <- c() 
   for (i in 1:n) {

     observed <- wide_df[i, paste0("n", 1:12)] > 0
     n_vec <- wide_df[i, paste0("n", 1:12)][observed]
     xbar_vec <- wide_df[i, paste0("xbar", 1:12)][observed]

     Sigma <- Sigma_template[observed, observed]
     diag(Sigma) <- loadings[observed] ^ 2 + manifest_variances[observed] / n_vec

     U <- chol(Sigma)  # An upper triangular matrix, or L^T in L * L^T
     det_U <- prod(diag(U))  # Easy determinent
     U_inv <- backsolve(U, diag(12), upper.tri=TRUE)

     x_centered_vec <- xbar_vec - manifest_means[observed]

     L_inv_x <- t(U_inv) %*% x_centered_vec  # L is U^T
     log_likelihoods <- c(log_likelihoods,
        -log(det_U) - .5 * as.numeric(t(L_inv_x) %*% L_inv_x)
     )
   }
   -sum(log_likelihoods)
}

my_optim <- optim(params_start, get_neg_log_likelihood, method="L-BFGS-B", control=list(maxit=1000))
my_optim

# Check first 5 loadings
my_optim$par[1:4]
# Check latent covariances
my_optim$par[13:15]
