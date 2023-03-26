library(dplyr)
library(tidyr)
library(lavaan)

options(width=200)

# Let's start with a structure to see if we can find it with EFA. 1 Group
N <- 1000 * 12
factor_corr <- .3
k_factors <- 3
items_per_factor <- 4
loading_pattern <- c(1, .8, .6, .4)
intercept_pattern <- c(.5, -.5, 0, 0)
res_var_pattern <- c(.8, .8, .4, .4)

# simulation

# Starting with standarized factors. Can generalize later, since factor
# dispersion could change across groups
factor_cov <- matrix(factor_corr, k_factors, k_factors)
diag(factor_cov) <- rep(1, times=k_factors)
res_cov <- diag(rep(res_var_pattern, times=k_factors))


L_mat <- MASS::mvrnorm(N, mu = rep(0, times=k_factors), Sigma=factor_cov)
p_manifest <- k_factors * items_per_factor
E_mat <- MASS::mvrnorm(N, mu = rep(0, times=p_manifest), Sigma=res_cov)

mu_vec <- rep(intercept_pattern, times=k_factors)
mu_mat <- matrix(rep(mu_vec, times=N), ncol=p_manifest, byrow=TRUE)

matrix_blocks <- rep(list(loading_pattern), 3)
lambda_mat <- t(as.matrix(Matrix::bdiag(matrix_blocks)))


X_mat <- mu_mat + L_mat %*% lambda_mat + E_mat

manifest_df <- as.data.frame(X_mat)
names(manifest_df) <- paste0("x", 1:p_manifest)

manifest_df %>% head()

# Fit a confirmatory factor analysis to recover it. For 3 factors here

# TODO: auto model string generator based on the parameters
model_string <- '
  L1 =~ NA*x1 + x1 + x2 + x3 + x4
  L2 =~ NA*x5 + x5 + x6 + x7 + x8
  L3 =~ NA*x9 + x9 + x10 + x11 + x12

  L1 ~~ L2
  L1 ~~ L3
  L2 ~~ L3

  x1 ~ 1
  x2 ~ 1
  x3 ~ 1
  x4 ~ 1
  x5 ~ 1
  x6 ~ 1
  x7 ~ 1
  x8 ~ 1
  x9 ~ 1
  x10 ~ 1
  x11 ~ 1
  x12 ~ 1
'
my_cfa <- cfa(model_string, data = manifest_df, std.lv=TRUE)
summary(my_cfa)
coef(my_cfa)

ptmodel <- parTable(my_cfa)  # Note that starting values are in here

# CFA: Missing data in the extreme - pulse surveys
manifest_pulse <- manifest_df
manifest_pulse <- data.frame(matrix(rep(NA, times=N * 12 * 12), ncol=12))
names(manifest_pulse) <- names(manifest_df)

j <- 0
pulse_row <- 1
for (i in 1:nrow(manifest_df)) {
  for (k in 1:12) {
    manifest_pulse[pulse_row, (j %% 12) + 1] <-  manifest_df[i, (j %% 12) + 1]
    j <- j + 1
    pulse_row <- pulse_row + 1
  }
}

## Check that the last two values of manifest_df are manifest_pulse
manifest_df %>% tail(2)
manifest_pulse %>% tail(2)

# Arbitrarily assign group sizes so that n_js are different

## OOPS! Start so where they're all exactly 12 to match this simulation setup
## THEN, later, you'll have to simulate the pulse survey data separately
# group_sizes <- sample(seq(12, 36), size=N, replace=TRUE)
group_sizes <- rep(12, times = N)

group_starts <- cumsum(group_sizes) + 1
m <- min(which(group_starts > nrow(manifest_pulse) - 12 + 1))
group_starts <- group_starts[1:(m-1)]

manifest_pulse$group <- NA
manifest_pulse[1, "group"] <- "group 1"

group_levels_past1 <- paste("group", 1 + 1:length(group_starts))
manifest_pulse[group_starts, "group"] <- group_levels_past1

manifest_pulse %>% head(36)
manifest_pulse %>% tail(36)  # Check that the last group has at least 12 obs

manifest_pulse <- manifest_pulse %>%
  fill(group, .direction='down') %>%
  mutate(group = factor(group, levels=c("group 1", group_levels_past1)))

# Group statistics now mimic averaging over pulse survey responses for a period

manifest_stats <- manifest_pulse %>%
  group_by(group) %>%
  summarize(n_pulse_surveys=n(),
            x1_bar=mean(x1, na.rm=TRUE),
            x2_bar=mean(x2, na.rm=TRUE),
            x3_bar=mean(x3, na.rm=TRUE),
            x4_bar=mean(x4, na.rm=TRUE),
            x5_bar=mean(x5, na.rm=TRUE),
            x6_bar=mean(x6, na.rm=TRUE),
            x7_bar=mean(x7, na.rm=TRUE),
            x8_bar=mean(x8, na.rm=TRUE),
            x9_bar=mean(x9, na.rm=TRUE),
            x10_bar=mean(x10, na.rm=TRUE),
            x11_bar=mean(x11, na.rm=TRUE),
            x12_bar=mean(x12, na.rm=TRUE),
  ) %>%
  arrange(group)

manifest_stats

# Just for this all n equal example
stopifnot(all(manifest_stats$n_pulse_surveys == 12))

# Math says covs should be lambda_i * lambda_j, regardless of n_i or n_j
cat("compare to ", lambda_mat[1, 1] * lambda_mat[1, 2], "\n")
cov(manifest_stats[, c("x1_bar", "x2_bar")])[1, 2]

cat("compare to", lambda_mat[1, 2] * lambda_mat[1, 3], "\n")
cov(manifest_stats[, c("x2_bar", "x3_bar")])[1, 2]

# Now, the challenge is to replicate lavaan with equal-size means
## In a separate simualation, I'll move to unequal-size means


## Step 1. Get the parameter starting values from the original cfa model
loadings <- ptmodel[1:12, "start"]
latent_covs <- ptmodel[13:15, "start"]
manifest_means <- ptmodel[16:27, "start"]  # Note that ests = start vals

### Might as well get the ests while you're here
loadings_est <- ptmodel[1:12, "est"]
latent_covs_est <- ptmodel[13:15, "est"]
manifest_means_est <- ptmodel[16:27, "est"]  # Note that ests = start vals

# You can see that these are the means: manifest_stats$x1_bar %>% mean()
manifest_variances <- ptmodel[28:39, "start"]
### And then latent variances and intercepts are fixed

## Functions
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

create_block_matrix1(loadings[1:4], manifest_variances[1:4])

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

create_block_matrix2(c(1, .8, .6, .4), c(1, .8, .6, .4), .3)

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

# write some tests
loading_pattern <- c(1, .8, .6, .4)
res_var_pattern <- c(.8, .8, .4, .4)

my_cov <- create_covariance_matrix(
  rep(loading_pattern, 3), rep(res_var_pattern, 3), c(.3, .3, .3)
)

# Test by visual inspection with path-tracing rules


create_premultiplying_matrix <- function(loadings, latent_covs) {
  template <- matrix(c(
    rep(loadings[1], 3),
    rep(loadings[2], 3),
    rep(loadings[3], 3),
    rep(loadings[4], 3)),
  byrow=TRUE, ncol=3)

  part1 <- template
  part1[, 2] <- part1[, 2] * latent_covs[1]
  part1[, 3] <- part1[, 3] * latent_covs[2]

  template <- matrix(c(
    rep(loadings[5], 3),
    rep(loadings[6], 3),
    rep(loadings[7], 3),
    rep(loadings[8], 3)),
  byrow=TRUE, ncol=3)

  part2 <- template
  part2[, 1] <- part2[, 1] * latent_covs[1]
  part2[, 3] <- part2[, 3] * latent_covs[3]

  template <- matrix(c(
    rep(loadings[9], 3),
    rep(loadings[10], 3),
    rep(loadings[11], 3),
    rep(loadings[12], 3)),
  byrow=TRUE, ncol=3)

  part3 <- template
  part3[, 1] <- part3[, 1] * latent_covs[2]
  part3[, 2] <- part3[, 2] * latent_covs[3]

  t(rbind(part1, part2, part3))
}


# Get the estimates
loadings_est <- ptmodel[1:12, "est"]
latent_covs_est <- ptmodel[13:15, "est"]
manifest_means_est <- ptmodel[16:27, "est"]  # Note that ests = start vals
manifest_variances_est <- ptmodel[28:39, "est"]  # Note that ests = start vals

# L_hat premultiplyting mat
my_premat <- create_premultiplying_matrix(loadings_est, latent_covs_est)
my_premat


my_cov <- create_covariance_matrix(
  loadings_est, manifest_variances_est, latent_covs_est 
)
my_cov



# Can I get the L-hats now?
## NOTE: the estimates are plugged in below
means_mat <- matrix(rep(1, times = 12 * nrow(manifest_df)), ncol=12) %*% diag(manifest_means_est)

X_mat <- manifest_df %>% select(starts_with("x")) %>% as.matrix()
X_mat_centered <- X_mat - means_mat

L_hats <- t(my_premat %*% solve(my_cov) %*% t(X_mat_centered))
L_hats %>% head()

predict(my_cfa) %>% head()


# Let's maximize the likelihood -----

## Means will be used to center, not part of the optimization equation
### Though you might need to rethink that for different sample sizes per row / column combo

loadings_start <- ptmodel[1:12, "start"]
latent_covs_start <- ptmodel[13:15, "start"]
manifest_means_start <- ptmodel[16:27, "start"]  # Note that ests = start vals
manifest_variances_start <- ptmodel[28:39, "start"]  # Note that ests = start vals

# Match the pattern of ptmodels as much as possible
params <- c(loadings_start, latent_covs_start, log(manifest_variances_start))

means_mat <- matrix(rep(1, times = 12 * nrow(manifest_df)), ncol=12) %*% diag(manifest_means_est)
X_mat <- manifest_df %>% select(starts_with("x")) %>% as.matrix()
X_mat_centered <- X_mat - means_mat


get_neg_log_likelihood <- function(params) {
   # using manifest_df, and temporarily, manifest_means from the environment
   loadings <- params[1:12]
   latent_covs <- params[13:15]

   manifest_variances <- exp(params[16:27])

   print(paste("params <- c(", paste(params, collapse=','), ")\n"))

   Sigma <- create_covariance_matrix(loadings, manifest_variances, latent_covs)

   #Sigma_inv <- solve(Sigma)

   U <- chol(Sigma)  # An upper triangular matrix, or L^T in L * L^T

   #Sigma_copy <- t(U) %*% U

   det_U <- prod(diag(U))  # Easy determinent

   U_inv <- backsolve(U, diag(12), upper.tri=TRUE)

   #Sigma_inv_copy <- U_inv %*% t(U_inv)

   #Sigma_det <- det_U ^ 2

   # DO it naive, then do it better
   n <- nrow(manifest_df)
   squares_vec <- c() 
   for (i in 1:n) {
     x_vec <- X_mat_centered[i, ]
     L_inv_x <- t(U_inv) %*% x_vec  # L is U^T
     squares_vec <- c(squares_vec,
        as.numeric(t(L_inv_x) %*% L_inv_x)
       #-.5 * log(Sigma_det) - .5 * as.numeric(t(x_vec) %*% Sigma_inv %*% x_vec)
     )
   }
   loglike <- -n * log(det_U) - .5 * sum(squares_vec)
   -loglike
}


params_start <- c(loadings_start, latent_covs_start, log(manifest_variances_start))
my_optim <- optim(params_start, get_neg_log_likelihood, method="L-BFGS-B", control=list(maxit=1000))
my_optim
my_optim$par

get_log_likelihood(my_optim$par)

# Back to the original 12-var df, on to Exploratory Factor Analysis


# Then, group responses into units and have different numbers
# of questions per unit
# Following: https://towardsdatascience.com/exploratory-factor-analysis-in-r-e31b0015f224

corrplot::corrplot(cor(manifest_df))
corrplot::corrplot(cor(manifest_df), method="number")

# The Factorability of the Data
## Kaiser-Meyer-Olkin (KMO)
# a suggested cutoff for determining the factorability of the sample data is KMO ≥ 60.
psych::KMO(r=cor(manifest_df))


# Bartlett’s Test of Sphericity
## small p values are good here, apparently
psych::cortest.bartlett(manifest_df)

# Scree plot
library(ggplot2)
my_fa <- psych::fa(manifest_df, nfactors=12,
                   rotate="none")

scree_df <- data.frame(
  factor_n=factor(1:12),
  eigenvalue=my_fa$e.values
)

# See this looks like you need 4 factors
ggplot(scree_df, aes(x = factor_n, y = eigenvalue, group = 1)) + 
  geom_point() + geom_line() +
  xlab("Number of factors") +
  ylab("Initial eigenvalue") +
  labs( title = "Scree Plot", 
        subtitle = "(Based on the unreduced correlation matrix)")

# Parallel analysis
# https://en.wikipedia.org/wiki/Parallel_analysis
# NOTE: this is SLOW. Using a subset
parallel <- psych::fa.parallel(manifest_df[1:500, ], fa="fa")

# The EFA

my_efa <- psych::fa(
  manifest_df,
  nfactors=3,
  rotate="oblimin",
  fm="ml"
)

print(my_efa)

# Adjust cut (default is .3)
psych::fa.diagram(my_efa, cut=.1)


