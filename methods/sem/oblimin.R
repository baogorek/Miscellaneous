library(dplyr)
library(psych)
library(GPArotation)
library(lavaan)
library(semPlot)

# Read in only complete cases from the example data set -----------
data(bfi)
df <- bfi[, 1:25] %>%
  filter(complete.cases(.))  # Not going to think too hard about this; it's a demo


# Oblimin rotation demonstration --------------------

## Perform factor analysis with Oblimin rotation
fa_none <- fa(df, nfactors = 3, rotate = "none")
print(fa_none$loadings, cutoff=1E-6, digits=5)

# Our loadings here are definitely defined in terms of a single vector X (px1)
# modeled as X = L F+ e, where L is p x m

fa_oblimin <- fa(df, nfactors = 3, rotate = "oblimin")
print(fa_oblimin$loadings, cutoff=1E-6, digits=5)

fa_oblimin2 <- oblimin(fa_none$loadings)
print(fa_oblimin2$loadings, cutoff=1E-6, digits=3)

fa_oblimin3 <- quartimin(fa_none$loadings)
print(fa_oblimin3$loadings, cutoff=1E-6, digits=3)

fa_biquartimin <- oblimin(fa_none$loadings, gam=.5)
print(fa_biquartimin$loadings, cutoff=1E-6, digits=5)

## calculate the Quartimin criterion and show that it is minimized by the oblimin loadings
get_quartimin_obj <- function(Lambda) {
  class(Lambda) <- "matrix"
  A <- Lambda ^ 2 
  N <- matrix(rep(1, times=ncol(Lambda)**2), ncol=ncol(Lambda))
  diag(N) <- rep(0, times=ncol(N))
  B <- A %*% N
  sum(A * B) / 4
}

get_quartimin_obj(fa_none$loadings)
get_quartimin_obj(fa_biquartimin$loadings)
get_quartimin_obj(fa_oblimin$loadings)


# CFA of the bfi complete cases, no cross-loadings  ----------
model_string <- '
  L1 =~ NA*A1 + A1 + A2 + A3 + A4 + A5
  L2 =~ NA*C1 + C1 + C2 + C3 + C4 + C5 
  L3 =~ NA*E1 + E1 + E2 + E3 + E4 + E5
  L4 =~ NA*N1 + N1 + N2 + N3 + N4 + N5
  L5 =~ NA*O1 + O1 + O2 + O3 + O4 + O5

  L1 ~~ L2
  L1 ~~ L3
  L1 ~~ L4
  L1 ~~ L5
  L2 ~~ L3
  L2 ~~ L4
  L2 ~~ L5
  L3 ~~ L4
  L3 ~~ L5

  A1 ~ 1
  A2 ~ 1
  A3 ~ 1
  A4 ~ 1
  A5 ~ 1
  C1 ~ 1
  C2 ~ 1
  C3 ~ 1
  C4 ~ 1
  C5 ~ 1
  E1 ~ 1
  E2 ~ 1
  E3 ~ 1
  E4 ~ 1
  E5 ~ 1
  N1 ~ 1
  N2 ~ 1
  N3 ~ 1
  N4 ~ 1
  N5 ~ 1
  O1 ~ 1
  O2 ~ 1
  O3 ~ 1
  O4 ~ 1
  O5 ~ 1
'
my_cfa <- cfa(model_string, data = df, std.lv=TRUE)
my_cfa
semPaths(my_cfa, what="est", layout = "tree", intercepts=FALSE, sizeMan=2.5)

# Get the loadings matrix from the model fit
loading_matrix <- lavInspect(my_cfa, "est")$lambda
oblimin_of_cfa <- oblimin(loading_matrix)

# if you sort the loadings, it will look different. See help(print.GPArotation)
print(loading_matrix, sortLoadings=FALSE)
print(oblimin_of_cfa$loadings, digits=3)


# Now, refit with a cross-product added, based on modification indicies

modificationIndices(my_cfa, sort=TRUE) %>% head()

# Highest cross-loading modification index:
# L3 =~  N4 200.789
# Before, Chi-square statistic was 4165.5

model_string2 <- '
  L1 =~ NA*A1 + A1 + A2 + A3 + A4 + A5
  L2 =~ NA*C1 + C1 + C2 + C3 + C4 + C5
  L3 =~ NA*E1 + E1 + E2 + E3 + E4 + E5 + N4
  L4 =~ NA*N1 + N1 + N2 + N3 + N4 + N5
  L5 =~ NA*O1 + O1 + O2 + O3 + O4 + O5

  L1 ~~ L2
  L1 ~~ L3
  L1 ~~ L4
  L1 ~~ L5
  L2 ~~ L3
  L2 ~~ L4
  L2 ~~ L5
  L3 ~~ L4
  L3 ~~ L5

  A1 ~ 1
  A2 ~ 1
  A3 ~ 1
  A4 ~ 1
  A5 ~ 1
  C1 ~ 1
  C2 ~ 1
  C3 ~ 1
  C4 ~ 1
  C5 ~ 1
  E1 ~ 1
  E2 ~ 1
  E3 ~ 1
  E4 ~ 1
  E5 ~ 1
  N1 ~ 1
  N2 ~ 1
  N3 ~ 1
  N4 ~ 1
  N5 ~ 1
  O1 ~ 1
  O2 ~ 1
  O3 ~ 1
  O4 ~ 1
  O5 ~ 1
'
my_cfa2 <- cfa(model_string2, data = df, std.lv=TRUE)
my_cfa2

# Chi-square difference is a little larger than what modificationIndices told me it would be
4165.5 - 3955.5

semPaths(my_cfa2, what="est", layout = "tree", intercepts=FALSE, sizeMan=2.5)

loading_matrix2 <- lavInspect(my_cfa2, "est")$lambda
oblimin_of_cfa2 <- oblimin(loading_matrix2)

print(loading_matrix2, sortLoadings=F)
print(oblimin_of_cfa2$loadings, digits=3)
