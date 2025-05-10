# https://cran.r-project.org/web/packages/sandwich/vignettes/sandwich-OOP.pdf
# Freedman's: https://www.stat.berkeley.edu/~census/mlesan.pdf
# https://cran.r-project.org/web/packages/sjPlot/vignettes/tab_model_robust.html
# Autocorrelation: https://www.jstatsoft.org/article/view/v011i10
library(dplyr)
library(sandwich)
library(lavaan)


generate_dependent_binary_data <- function(N, n, logit_mu, logit_sigma,
                                           pseudo_replication=FALSE,
                                           same_x_pattern = FALSE) {
  # Generate data with dependency within cluster, modeled by random logit means--
  cluster_logit_means <- rnorm(N, mean=logit_mu, sd=logit_sigma)
  cluster_df <- data.frame(
    cluster = 1:N,
    prob = plogis(cluster_logit_means)  # cluster level Pr
  )
  if (pseudo_replication) {
    cluster_df$y <- as.numeric(runif(nrow(cluster_df)) < cluster_df$prob)
    cluster_df$x <- round(rnorm(N), 3)  # Same x throughout the cluster
  }
  
  df <- data.frame(cluster=sort(rep(seq(1, N), n)), obs=rep(seq(1, n), N)) |>
    inner_join(cluster_df, by="cluster")
  if (!pseudo_replication) { 
    df$y <- as.numeric(runif(nrow(df)) < df$prob)
    df$x <- round(rnorm(N * n), 3)  # Covariate: no relation to response
    if (same_x_pattern) {
      df$x <- rep(rep(1:n), N)  # Covariate: no relation to response
    }
  }
  return(df)
}

generate_dependent_poisson_data <- function(N, n_low, n_high, mu, sigma) {
  # Generate data with dependency within cluster, modeled by random logit means--
  cluster_poisson_means <- rnorm(N, mean=mu, sd=sigma)
  cluster_df <- data.frame(cluster = 1:N, mean=cluster_poisson_means)
 
  dfs <- lapply(1:N, function(i) data.frame(cluster=i, obs=seq(1, sample(seq(n_low, n_high), 1))))
  df <- Reduce(`rbind`, dfs) |>
    inner_join(cluster_df, by="cluster")
  
  df$y <- rpois(nrow(df), df$mean)
  df$x <- round(rnorm(nrow(df)), 3)  # Covariate: no relation to response
  return(df)
}

generate_dependent_cfa_data <- function(N, n, mu_bar, sd_of_means) {
  # https://github.com/baogorek/Miscellaneous/blob/master/methods/sem/cfa-one-factor.R
  lambda1 <- .6
  lambda2 <- .4
  lambda3 <- .5
  
  sigma_e1 <- sqrt(.1)
  sigma_e2 <- sqrt(.1)
  sigma_e3 <- sqrt(.1)

  #item_means <- rnorm(N, mean=mu_bar, sd=sd_of_means)
  cluster_df <- data.frame(cluster = 1:N)
  L <- rnorm(N, mean=0, sd=1)
  cluster_df$mean1 <- lambda1 * L
  cluster_df$mean2 <- lambda2 * L
  cluster_df$mean3 <- lambda3 * L

  df <- data.frame(cluster=sort(rep(seq(1, N), n)), obs=rep(seq(1, n), N)) |>
    inner_join(cluster_df, by="cluster")

  # Induce correlation from nested subjects having the same latent factor  val
  # Generate independent errors
  e_1 <- rnorm(nrow(df), mean=0, sd=sigma_e1)
  e_2 <- rnorm(nrow(df), mean=0, sd=sigma_e2)
  e_3 <- rnorm(nrow(df), mean=0, sd=sigma_e3)

  df$y1 <- df$mean1 + e_1
  df$y2 <- df$mean2 + e_2
  df$y3 <- df$mean3 + e_3
  
  df <- df %>% select(cluster, y1, y2, y3) 
  return(df)
}

## Lavaan data experiment

model_string <- 
' L =~ NA*y1 + y1 + y2 + y3 

  y1 ~ 1
  y2 ~ 1
  y3 ~ 1
'


M <- 5000
N <- 300
n <- 7
mu_bar <- 30
sd_of_means <- 5

results <- data.frame()
for (rep in 1:M) {
  df <- generate_dependent_cfa_data(N, n, mu_bar, sd_of_means)
  cfa_mod <- cfa(model_string, data = df, std.lv=TRUE)
  beta <- partable(cfa_mod)[3, "est"]
  se_beta <- partable(cfa_mod)[3, "se"]

  results <- rbind(results, data.frame(rep=rep, beta=beta, se_beta=se_beta))
}

# Actual vs nominal standard error of beta 
cat("Model implied:", mean(results$se_beta), "vs empirical:", sd(results$beta), "\n")


cfa_mod <- cfa(model_string, data = df, std.lv=TRUE, cluster="cluster")

# https://cran.r-project.org/web/packages/lavaan/lavaan.pdf
# It does have an estfun!
estfun(cfa_mod)

cluster = "cluster",


# Poisson case -----------------------------

M <- 5000
N <- 300
n_low <- 3
n_high <- 10
mu_bar <- 30
sd_of_means <- 5

results <- data.frame()
for (rep in 1:M) {
  df <- generate_dependent_poisson_data(N, n_low, n_high, mu_bar, sd_of_means)
  glm_mod <- glm(y ~ x, data=df, family="poisson")
  glm_summary <- summary(glm_mod)
  beta <- glm_summary$coefficients %>% as.data.frame() %>% pull(`Estimate`) %>% tail(1)
  se_beta <- glm_summary$coefficients %>% as.data.frame() %>% pull(`Std. Error`) %>% tail(1)
  p_beta <- glm_summary$coefficients %>% as.data.frame() %>% pull(`Pr(>|z|)`) %>% tail(1)

  # From the sandwich package
  vcov_sandwich <- vcovCL(glm_mod, cluster=df$cluster, type="HC0", cadjust=FALSE)
  se_beta_sandwich <- sqrt(vcov_sandwich[2, 2])

  # Now from my cluster sandwhich formula ----
  g_mat <- estfun(glm_mod)

  results <- rbind(results, data.frame(rep=rep, beta=beta, se_beta=se_beta,
                                       p_beta=p_beta,
                                       se_beta_sandwich=se_beta_sandwich))
}
# Actual vs nominal Type I error rate
mean(results$p_beta < .05)

# Actual vs nominal standard error of beta 
cat("Model implied:", mean(results$se_beta), "vs empirical:", sd(results$beta),
    "vs Sandwich package", mean(results$se_beta_sandwich),
    "\n")

# Bread
vcov_from_bread <- (1 / (N * n)) * bread(glm_mod)
vcov_from_bread  # requires scaling
stats::vcov(glm_mod)
glm_summary$cov.scaled

sandwich:::bread.glm  # A private function

# Matching the meat matrix with estfun
g_mat <- estfun(glm_mod)  # How did it get these?
(1 / (N * n)) * t(g_mat) %*% g_mat
meat(glm_mod)

sandwich:::estfun.glm  # A private function

sandwich:::bread
c <- 1 / (N * n)
B <- bread(glm_mod)  # Notice that we're not scaling the bread, so it's not quite the vcov
M <- c * meat(glm_mod)
B %*% M %*% B

vcovHC(glm_mod, type="HC0")
# 1/NROW(estfun(obj)) * (bread. %*% meat. %*% bread.)

# Let's try one with vcov
N * n * vcov(glm_mod) %*% meat(glm_mod) %*% vcov(glm_mod)


# Now let's try the clustered version

# Bread is the same. It's only the meat that is different

g_mat2 <- g_mat %>% as.data.frame()
g_mat2$cluster <- df$cluster

g_cluster_sums <- aggregate(g_mat, by=list(cluster=df$cluster), FUN=sum)
g_cluster_sums$cluster <- NULL  # same as row number
g_cluster_mat <- g_cluster_sums %>% as.matrix()

(1 / nrow(df)) * t(g_cluster_mat) %*% g_cluster_mat
meatCL(glm_mod, cluster=df$cluster, type="HC0", cadjust=FALSE)




# It's still the same c
c <- 1 / nrow(df) 
B <- bread(glm_mod)
M2 <- c * meatCL(glm_mod, cluster=df$cluster, type="HC0", cadjust=FALSE)
B %*% M2 %*% B
vcovCL(glm_mod, cluster=df$cluster, type="HC0", cadjust=FALSE)


# The bread is the unscaled VCOV (probably because of sqrt(n) (theta_hat - theta) is the form used)
(1 / nrow(df)) * bread(glm_mod)
vcov(glm_mod)

# Can I get the gest?
g_mat <- estfun(glm_mod)
# uses weights and residuals of the glm_mod, which are part of stats


