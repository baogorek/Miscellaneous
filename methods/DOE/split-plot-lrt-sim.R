library(lme4)
set.seed(1343465)

N <- 2000 # whole plots. Use multiple of 2 for balanced data set
J <- 50  # subplots within w.p.s. Use multiple of 2 for balanced data set


B <- 10000 # Monte Carlo reps
p_values <- c()
for (rep in 1:B) {
  if (rep %% 100 == 0) cat("On Monte Carlo rep ", rep, "\n")
  u_i <- 5.5 * rnorm(N)
  e_i <- 2.9 * rnorm(N * J)
  
  sp_df <- expand.grid(subunit = 1:J, unit = 1:N)
  sp_df$wp_trt <- sp_df$unit <= N / 2
  sp_df$sp_trt <- sp_df$subunit <= J / 2
  
  sp_df$y <- 30 + u_i[sp_df$unit] + 1.5 * sp_df$sp_trt + e_i
  
  lmer_full_wp <- lmer(y ~ factor(wp_trt) + (1 | unit), data = sp_df,
                       REML = FALSE)
  lmer_red_wp <- lmer(y ~ (1 | unit), data = sp_df, REML = FALSE)
  
  my_anova <- anova(lmer_full_wp, lmer_red_wp)
  p_values <- c(p_values, my_anova["Pr(>Chisq)"][2, 1])
}

# With no whole plot treatment effect, the p-value distr should be uniform
hist(p_values, main = paste("Distribution of p-values for", N, "whole plots",
                            "and", J, "subplots / w.p."))


