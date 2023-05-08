library(lavaan)
library(dplyr)

set.seed(12345)

options(width=300)

# Set the parameters and sample size
lambda1 <- 1
lambda2 <- .5
lambda3a <- .7 # Lambda 3 for group a
lambda3b <- .6 # Lambda 3 for group b

lambda4 <- .5
lambda5 <- .5
lambda6 <- .5

cross_loading_lambda <- .04  # Really small; the test statistics quickly rises

sigma_e1 <- sqrt(.1)
sigma_e2 <- sqrt(.2)
sigma_e3 <- sqrt(.3)

sigma_e4 <- sqrt(.1)
sigma_e5 <- sqrt(.2)
sigma_e6 <- sqrt(.3)


N <- 10000

# Generate the data
# Note: I first tried this with 1 factor and no restrictions on indicators
# Besides the first loading being set to 1, and I had no degrees of freedom;
# That's because the model is fully saturated. For me to meaningfully run a
# test of configural invariance, there has to be meaningful restrictions
# that both groups share
L1 <- rnorm(2 * N, mean=0, sd=1)
L2 <- rnorm(2 * N, mean=0, sd=1)

e_1 <- rnorm(2 * N, mean=0, sd=sigma_e1)
e_2 <- rnorm(2 * N, mean=0, sd=sigma_e2)
e_3 <- rnorm(2 * N, mean=0, sd=sigma_e3)

e_4 <- rnorm(2 * N, mean=0, sd=sigma_e1)
e_5 <- rnorm(2 * N, mean=0, sd=sigma_e2)
e_6 <- rnorm(2 * N, mean=0, sd=sigma_e3)

## Group a, factor 1
y1a <- lambda1 * L1[1:N] + + e_1[1:N]
y2a <- lambda2 * L1[1:N] + e_2[1:N]
y3a <- lambda3a * L1[1:N] + e_3[1:N]

## Group b, factor 1
seq2 <- (N+1):(2*N)
y1b <- lambda1 * L1[seq2] + e_1[seq2]
y2b <- lambda2 * L1[seq2] + e_2[seq2]
# add a cross-loading in group b only, thus violating configural invariance
y3b <- lambda3b * L1[seq2] + cross_loading_lambda * L2[seq2] + e_3[seq2]

# Both Groups a and b, factor 2
y4 <- lambda4 * L2 + e_4
y5 <- lambda5 * L2 + e_5
y6 <- lambda6 * L2 + e_6

group <- c(rep("a", N), rep("b", N))
df <- data.frame(
  group=group, y1=c(y1a, y1b), y2=c(y2a, y2b), y3=c(y3a, y3b),
  y4=y4, y5=y5, y6=y6
)

# Now recover the parameters with lavaan

# There are meaningful constraints in this model; y3 does not
# load onto L2
model_string <- 
' L1 =~ y1 + y2 + y3 
  L2 =~ y4 + y5 + y6 
  y1 ~ 1
  y2 ~ 1
  y3 ~ 1
  y4 ~ 1
  y5 ~ 1
  y6 ~ 1
'

# Ignoring the group structure, fit a CFA
my_cfa <- cfa(model_string, data = df)
fitmeasures(my_cfa)

my_cfa_a <- cfa(model_string, data = df %>% filter(group == "a"))
fitmeasures(my_cfa_a)

my_cfa_b <- cfa(model_string, data = df %>% filter(group == "b"))
fitmeasures(my_cfa_b)

# Notice that the estimate of lambda3 is between lambda3a and lambda 3b
mgcfa <- cfa(model_string, data=df, group="group")
fitmeasures(mgcfa)
# I don't think this is meaningfully showing invariance. It's just doubling
# the parameters

# From: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6657455/pdf/fpsyg-10-01507.pdf
# The
# first issue is whether the same items can be used to measure
# the theoretical variable in each group. For example, is the item
# “I feel blue” a good indicator of depression?
# If the answer is
# yes, we are dealing with configural invariance. The loadings (the
# extent to which each item taps into the underlying construct of
# depression) are all in the same direction in the different groups
# (this is why this sometimes called form invariance), nevertheless,
# the specific factor loadings or item discrimination parameters
# may still differ across samples

# NOTE: I really think the description above is too simple. You could have
# all loadings in the same direction but have a cross-loading for one group,
# and not the other, which would violate configural (form) invariance.

# Also, this makes me think about the (jokingly discussed) "cowardice" on
# Quantitude. Having a single factor model makes it pretty hard to fail
# Configural invariance, because you're not imposing any structural contraints
# You could of course fail metric invariance, etc.

# Let's see if I can calculate the fit measures myself, on the b group
# Using: https://journals.sagepub.com/doi/10.1177/0013164418783530
null_model_string <- 
'
  y1 ~ 1
  y2 ~ 1
  y3 ~ 1
  y4 ~ 1
  y5 ~ 1
  y6 ~ 1
'
null_cfa <- cfa(null_model_string, data = df %>% filter(group == "b"))
chi_square_null <- fitMeasures(null_cfa)['chisq']
df_null <- fitMeasures(null_cfa)['df']

chi_square_cfa <- fitMeasures(my_cfa_b)['chisq']
df_cfa <- fitMeasures(my_cfa_b)['df']
tli_cfa <- fitMeasures(my_cfa_b)['tli']
cfi_cfa <- fitMeasures(my_cfa_b)['cfi']
rmsea_cfa <- fitMeasures(my_cfa_b)['rmsea']

# TFI: a relative reduction in misfit per degree of freedom.
# nonnormed in that its value can occasionally be negative or exceed 1
(chi_square_null/df_null - chi_square_cfa / df_cfa) / (chi_square_null/df_null - 1)
tli_cfa

# CFI: relative improvement in fit going from the baseline model to the postulated model
(max(chi_square_null - df_null, 0) - max(chi_square_cfa - df_cfa, 0)) / max(chi_square_null - df_null, 0)
cfi_cfa

# RMSEA is a badness-of-fit measure, yielding lower values for a better fit.
sqrt(max(chi_square_cfa - df_cfa, 0) / (df_cfa * (N - 0)))
rmsea_cfa  # Got it! 
# From https://www.researchgate.net/profile/Patricia-Brosseau-Liard/publication/263928614_An_Investigation_of_the_Sample_Performance_of_Two_Nonnormality_Corrections_for_RMSEA/links/580fb7ab08aef2ef97afe9d7/An-Investigation-of-the-Sample-Performance-of-Two-Nonnormality-Corrections-for-RMSEA.pdf

# "There is not one definition of the RMSEA"

