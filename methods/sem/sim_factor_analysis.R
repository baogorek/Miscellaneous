library(dplyr)
library(lavaan)

# Let's start with a structure to see if we can find it with EFA. 1 Group
N <- 10000
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

# On to Exploratory Factor Analysis


# Then, I'm going to group responses into units and have different numbers
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

