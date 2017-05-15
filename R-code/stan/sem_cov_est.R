n <- 100000

rho1 <- .7
sigma_a <- sqrt(2)
rho2 <- .4
sigma_b <- 1

lambda_a <- sqrt(rho1 * sigma_a ^ 2)
sigma_e <- sqrt((1 - rho1) * sigma_a ^ 2)
lambda_b <- sigma_a * sigma_b * rho2 / lambda_a
sigma_g <- sqrt(sigma_b ^ 2 - lambda_b ^ 2)

L <- rnorm(n)
y1 <- lambda_a * L + sigma_e * rnorm(n) 
y2 <- lambda_a * L + sigma_e * rnorm(n)
y3 <- lambda_b * L + sigma_g * rnorm(n)

cor(cbind(y1, y2, y3))
cov(cbind(y1, y2, y3))


bound <- function(rho1) {
  sqrt( 1 / ( 1 + (1 - rho1) / (1 + rho1) ) )
}

rho_seq <- seq(-1, 1, .01)
plot(bound(rho_seq) ~ rho_seq)

rho1 <- 0
rho2 <- -.70
Sigma_123 <- matrix(c(
  2               , sqrt(2 * 2) * rho1, sqrt(2 * 1) * rho2,
  sqrt(2 * 2) * rho1, 2               , sqrt(2 * 1) * rho2,
  sqrt(2 * 1) * rho2, sqrt(2 * 1) * rho2, 1
), ncol = 3)
eigen(Sigma_123)$values

library(rstan)
library(MASS)
library(dplyr)
library(tidyr)

n_clusters <- 1000

cluster_list <- lapply(1:n_clusters,
                       FUN = function(i) {list(type = sample(1:3, sample(2:3)),
                                               cluster = i)})
generate_cov_mat <- function(indexes) {
  Sigma_123 <- matrix(c(
    2               , sqrt(2 * 2) * .7, sqrt(2 * 1) * .4,
    sqrt(2 * 2) * .7, 2               , sqrt(2 * 1) * .4,
    sqrt(2 * 1) * .4, sqrt(2 * 1) * .4, 1
  ), ncol = 3)
  return(Sigma_123[indexes, indexes])
}

sim_df <- data.frame()
for (j in 1:n_clusters) {
  indexes <- cluster_list[[j]]$type
  p <- length(indexes)
  mu <- rep(0, times = p)
  sigma <- generate_cov_mat(indexes)
  y <- mvrnorm(1, mu = mu, Sigma = sigma)
  sim_df <- rbind(sim_df, data.frame(cluster = j, type = cluster_list[[j]]$type,
                                     y = y))
}

sim_df <- sim_df[order(sim_df$cluster, sim_df$type), ]

cluster_df <- sim_df %>% mutate(row_no = row_number()) %>% group_by(cluster) %>%
  summarise(low_index = min(row_no), high_index = max(row_no))

stanDat <- list(
  n_row = nrow(sim_df),
  cluster = sim_df$cluster,
  type = sim_df$type,
  y = sim_df$y,
  n_clusters = length(unique(sim_df$cluster)),
  subj_lower = cluster_df$low_index,
  subj_upper = cluster_df$high_index
)

stan_model_fit <- stan(file = "ragged_array_cov_est.stan",
                       model_name = "test model",
                       control = list(adapt_delta = .85),
                       data = stanDat, iter = 2000,
                       warmup = 500,
                       chains = 4, verbose = FALSE)

ext <- rstan:::extract(stan_model_fit)
hist(ext[["sigma_a"]]) # check for mass near sqrt(2) or 1.4
hist(ext[["sigma_b"]]) # check for mass near 1
hist(ext[["rho1"]]) # check for mass near .7 
hist(ext[["rho2"]]) # check for mass near .4 
