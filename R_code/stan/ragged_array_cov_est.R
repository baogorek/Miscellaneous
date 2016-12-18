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

quick_compile <- stanc("c:/devl/FantasySports/models/ragged_array_cov_est.stan")
stan_model_fit <- stan(file = "c:/devl/FantasySports/models/ragged_array_cov_est.stan",
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
