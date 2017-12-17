library(rstan)
stan_version()

N <- 1000

z_mat <- rmultinom(N, 1, c(.6, .3, .1))
z <- apply(z_mat, 2, which.max)

y <- (z == 1) * rnorm(N, 1, .5) + (z == 2) * rnorm(N, 4, .5) +
     (z == 3) * rnorm(N, -1, .5)

hist(y)

stan_code <- 
"
data {
  int<lower=1> K;
  int<lower=1> N;
  real y[N];
}

parameters {
  simplex[K] theta;
  ordered[K] mu;
  vector<lower=0>[K] sigma;
}


model {
  //real log_theta[K] = log(theta); //caching
  theta ~ dirichlet(rep_vector(1, K));
  sigma ~ normal(0, .75); // for whole vector
  mu ~ normal(0, 10); // for whole ordered vector

  for (n in 1:N) {
    real lps[K]; // log_theta;
    for (k in 1:K) {
      lps[k] = log(theta[k]) + normal_lpdf(y[n] | mu[k], sigma[k]);
    }
    target += log_sum_exp(lps); 
  }
}
"
fit <- stan(model_code = stan_code, model_name = "mixture",
            data = list(K = 3, N = N, y = y), warmup = 3000,
            iter = 6000, chains = 1)

e <- extract(fit)
traceplot(fit)
mean(e$mu[, 1])
mean(e$mu[, 2])
mean(e$mu[, 3])

mean(e$theta[, 1])
mean(e$theta[, 2])
mean(e$theta[, 3])

# Do do: make pi a transformed parameter

stan_code <- 
'
data {
  int<lower=1> T; // DP truncation parameter
  int<lower=1> N;
  real y[N];
}

parameters {
  //real<lower=0> alpha; // DP concentration parameter
  vector<lower=0, upper=1>[T] v; // stick breaking parameters
  ordered[T] mu;
  vector<lower=0>[T] sigma;
}

transformed parameters {
  vector[T] pi = rep_vector(0, T);

  real log_sum_v_complement = 0;
  for (i in 1:(T - 1)) {
    pi[i] = v[i] * exp(log_sum_v_complement);
    log_sum_v_complement = log_sum_v_complement + log(1 - v[i]);
  } 
  pi[T] = 1.0 - sum(pi);
}

model {
  // G is this independent mix of half-normal and normal
  sigma ~ normal(0, .75); // for whole vector
  mu ~ normal(0, 10); // for whole ordered vector
 
  v ~ beta(1, .5); // alpha is the seond parameter 

  for (n in 1:N) {
    real lps[T]; // log_theta;
    for (k in 1:T) {
      lps[k] = log(pi[k]) + normal_lpdf(y[n] | mu[k], sigma[k]);
    }
    target += log_sum_exp(lps); 
  }
}'

fit <- stan(model_code = stan_code, model_name = "mixture",
            data = list(T = 15, N = N, y = y), warmup = 1000,
            iter = 1500, chains = 1)


e <- extract(fit)
#traceplot(fit)

hist(e$pi[, 1])
hist(e$mu[, 2])
hist(e$sigma[, 1])
hist(rowSums(e$pi[, 1:14]))

eval_mix_dens <- function(i, x) {
  
  dens <- e$pi[i, 1] * dnorm(x, e$mu[i, 1], e$sigma[i, 1])
  for (j in 2:15) {
    dens <- dens + e$pi[i, j] * dnorm(x, e$mu[i, j], e$sigma[i, j])
  }
  return(dens)
}

y_grid <- seq(-3, 6, .05)

plot(eval_mix_dens(1, y_grid) ~ y_grid, type = "l")

for (i in 2:500) {
  lines(eval_mix_dens(i, y_grid) ~ y_grid, type = "l", col = "blue")
}

i = 40
integrand <- function(x) eval_mix_dens(i, x)
integrate(integrand, lower = -6, upper = 6)



