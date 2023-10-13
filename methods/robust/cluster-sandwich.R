library(dplyr)


generate_dependent_binary_data <- function(N, n, logit_mu, logit_sigma) {
  # Generate data with dependency within cluster, modeled by random logit means--
  cluster_logit_means <- rnorm(N, mean=logit_mu, sd=logit_sigma)
  cluster_df <- data.frame(cluster = 1:N, logit_mean=cluster_logit_means)
  
  df <- data.frame(cluster=sort(rep(seq(1, N), n)), obs=rep(seq(1, n), N)) |>
    inner_join(cluster_df, by="cluster")
  
  df$prob <- plogis(df$logit_mean)
  df$y <- as.numeric(df$prob < runif(nrow(df)))
  # TODO: check to make sure the above step is doing what it's doign
  
  # Look at logit_mean and prob before they're deleted
  df$logit_mean <- NULL
  df$prob <- NULL
  
  # Add a covariate that has nothing to do with the response
  df$x <- rnorm(N*n)
  return(df)
}

generate_dependent_poisson_data <- function(N, n, mu, sigma) {
  # Generate data with dependency within cluster, modeled by random logit means--
  cluster_poisson_means <- rnorm(N, mean=mu, sd=sigma)
  cluster_df <- data.frame(cluster = 1:N, mean=cluster_poisson_means)
  
  df <- data.frame(cluster=sort(rep(seq(1, N), n)), obs=rep(seq(1, n), N)) |>
    inner_join(cluster_df, by="cluster")
  
  df$y <- sapply(1:nrow(df), function(i) rpois(1, lambda=df[i, "mean"]))
  
  # Look at logit_mean and prob before they're deleted
  df$mean <- NULL
  
  # Add a covariate that has nothing to do with the response
  df$x <- rnorm(N*n)
  return(df)
}


M <- 5000
N <- 300
n <- 5 
logit_mu <- -1.5
logit_sigma <- 5

results <- data.frame()
for (rep in 1:M) {
   df <- generate_dependent_binary_data(N, n, logit_mu, logit_sigma)
   # TODO: measure number of pure clusters, 1 or 0, and the rate of 1s. See if you're doing something stupid here
   glm_summary <- summary(glm(y ~ x, data=df, family="binomial"))
   beta <- glm_summary$coefficients %>% as.data.frame() %>% pull(`Estimate`) %>% tail(1)
   se_beta <- glm_summary$coefficients %>% as.data.frame() %>% pull(`Std. Error`) %>% tail(1)
   p_beta <- glm_summary$coefficients %>% as.data.frame() %>% pull(`Pr(>|z|)`) %>% tail(1)
   results <- rbind(results, data.frame(rep=rep, beta=beta, se_beta=se_beta, p_beta=p_beta))
}

# Actual vs nominal Type I error rate
mean(results$p_beta < .05)

# Actual vs nominal standard error of beta 
cat("Model implied:", mean(results$se_beta), "vs empirical:", sd(results$beta), "\n") 

mu_bar <- 30
sd_of_means <- 15

results <- data.frame()
for (rep in 1:M) {
   df <- generate_dependent_poisson_data(N, n, mu_bar, sd_of_means)
   glm_summary <- summary(glm(y ~ x, data=df, family="poisson"))
   beta <- glm_summary$coefficients %>% as.data.frame() %>% pull(`Estimate`) %>% tail(1)
   se_beta <- glm_summary$coefficients %>% as.data.frame() %>% pull(`Std. Error`) %>% tail(1)
   p_beta <- glm_summary$coefficients %>% as.data.frame() %>% pull(`Pr(>|z|)`) %>% tail(1)
   results <- rbind(results, data.frame(rep=rep, beta=beta, se_beta=se_beta, p_beta=p_beta))
}

# Actual vs nominal Type I error rate
mean(results$p_beta < .05)

# Actual vs nominal standard error of beta 
cat("Model implied:", mean(results$se_beta), "vs empirical:", sd(results$beta), "\n") 



