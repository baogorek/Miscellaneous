Stan case study: Covariance Estimation of ragged arrays with shared parameters
==================================================================

This simulated practice case investigates the situation where there is a
structured covariance matrix for the vector (X1, X2, X3) but the observations
form a "ragged array"; observations come in the form of either (X1, X2),
(X1, X3), (X2, X3), or (X1, X2, X3).

The variances and covariances are such that:
 - Var(X1) = Var(X2) = sigma\_a^2
 - Var(X3) = sigma\_b^2
 - Cov(X1, X2) = rho1
 - Cov(X1, X3) = rho2
 - Cov(X2, X3) = rho2 

The resulting covariance matrix is structured in that it is described by 4
rather than the 6 possible parameters, but this author could not figure out
how to specify such a structure in existing software (e.g., lme4, nlme).

The R file in this directory simulates data from such a case and uses rstan
to link the stan file. The final posteriors look okay, but as an inexperienced
both stan user and bayesian practicioner, there are a few points that concern
me:

 - My priors don't ensure positive definiteness
 - There's a warning message about my "transformation" of the data into
cluster-sized variables
 - There seems to be some bias in the posterior draws, but perhaps that's due
to my choice of priors (independent uniforms for rho1 and rho2, half normal
for sigma\_a and sigma\_b.
