library(rstan)
library(MASS)
library(dplyr)
library(tidyr)

##############################################################################
# Simulation of 2 and 3 dimensional Gaussian vectors with sparse cov structure
##############################################################################

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

######################################################
# Working with Data in Long form
######################################################

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

# Stan approach 1: Data in long form, direct inference on cov matrix parms
stan_model_fit <- stan(file = "ragged_array_cov_est_long.stan",
                       model_name = "long form model",
                       control = list(adapt_delta = .85),
                       data = stanDat, iter = 1000,
                       warmup = 100,
                       chains = 3, verbose = FALSE)

ext <- rstan:::extract(stan_model_fit)
hist(ext[["sigma_a"]]) # check for mass near sqrt(2) or 1.4
hist(ext[["sigma_b"]]) # check for mass near 1
hist(ext[["rho1"]]) # check for mass near .7 
hist(ext[["rho2"]]) # check for mass near .4

# Stan approach 2: Data in long form, indirect inference on cov mat via SEM
stan_model_fit <- stan(file = "ragged_array_cov_est_sem.stan",
                       model_name = "SEM estimation",
                       control = list(adapt_delta = .85),
                       data = stanDat, iter = 1000,
                       warmup = 100,
                       chains = 2, verbose = FALSE)

ext <- rstan:::extract(stan_model_fit)
hist(ext[["sigma_a"]]) # check for mass near sqrt(2) or 1.4
hist(ext[["sigma_b"]]) # check for mass near 1
hist(ext[["rho1"]]) # check for mass near .7 
hist(ext[["rho2"]]) # check for mass near .4

######################################################
# Getting Data in Wide form
######################################################

sim_df_w_sizes <- sim_df %>% group_by(cluster) %>%
  mutate(n = length(y), pos = row_number(y))
sim_df_size2 <- filter(sim_df_w_sizes, n == 2)
sim_df_size3 <- filter(sim_df_w_sizes, n == 3)

y_matrix2 <- sim_df_size2 %>% select(cluster, pos, y) %>% spread(pos, y) 
type_matrix2 <- sim_df_size2 %>% select(cluster, pos, type) %>%
  spread(pos, type) 

y_matrix3 <- sim_df_size3 %>% select(cluster, pos, y) %>% spread(pos, y) 
type_matrix3 <- sim_df_size3 %>% select(cluster, pos, type) %>%
  spread(pos, type) 

stanDat <- list(
  type_size2 = as.matrix(type_matrix2)[, 2:3],
  y_size2 = as.matrix(y_matrix2)[, 2:3],
  n_size2 = nrow(y_matrix2),
  type_size3 = as.matrix(type_matrix3)[, 2:4],
  y_size3 = as.matrix(y_matrix3)[, 2:4],
  n_size3 = nrow(y_matrix3) 
)

# Stan approach 3: Data in wide form, direct inference on cov matrix parms
stan_model_fit <- stan(file = "ragged_array_cov_est_wide.stan",
                       model_name = "wide format model",
                       control = list(adapt_delta = .85),
                       data = stanDat, iter = 1000,
                       warmup = 100,
                       chains = 3, verbose = FALSE)

ext <- rstan:::extract(stan_model_fit)
hist(ext[["sigma_a"]]) # check for mass near sqrt(2) or 1.4
hist(ext[["sigma_b"]]) # check for mass near 1
hist(ext[["rho1"]]) # check for mass near .7 
hist(ext[["rho2"]]) # check for mass near .4
