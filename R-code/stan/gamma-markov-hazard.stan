// http://allman.rhon.itam.mx/~lnieto/index_archivos/isi2003lenb.pdf

functions {
  real gamma_poisson_lpdf(real x, real lambda, real alpha, real beta) {
    real trunc_limit_real = ceil(lambda + 6 * sqrt(lambda));
    real prob = 0;
 
    int z = 0; 
    while (z < trunc_limit_real) {
      prob = prob + exp(poisson_lpmf(z | lambda) + gamma_lpdf(x | alpha + z, beta));
      z = z + 1;
    }
    return log(prob);
  }
}

data {
  // uncensored portion
  int<lower=0> n_u;
  real<lower=0> y_u[n_u];
  matrix[n_u, 2] X_u;

 // censored portion
  int n_c;
  real<lower=0> y_c[n_c];
  matrix[n_c, 2] X_c;
}

parameters {
  real<lower=0> h[25];
  vector[2] theta;   
}

model {
  real alpha = .05;
  real beta = .55; // Consider promoting to sequence
  real c = 1;
  real hazard_t; //convenience variable

  theta[1] ~ normal(0, .3);
  theta[2] ~ normal(0, .3);

  h[1] ~ gamma(alpha, beta); // alpha_k, beta_k = .001 for all k
  for (t in 2:25) {
    h[t] ~ gamma_poisson(c * h[t - 1], alpha, beta + c); // alpha_k, beta_k = .001 for all k
  }
 
  //# uncensored
  for (i in 1:n_u) {
    
    int t = 1; // The interval (0, 1]
    while (t < ceil(y_u[i])) {
      target += -h[t] * exp(X_u[i, 1] * theta[1] + X_u[i, 2] * theta[2]);
      t = t + 1;
    }
    hazard_t = h[t] * exp(X_u[i, 1] * theta[1] + X_u[i, 2] * theta[2]);
    //target += log(1 - exp(-hazard_t * 1.0)); // event happens in this t-th interval
    target += log(hazard_t) - hazard_t * (y_u[i] - floor(y_u[i]));
  }

  # censored
  for (i in 1:n_c) {
    int t = 1; // The interval (0, 1]
    while (t < ceil(y_c[i])) {
      target += -h[t] * exp(X_c[i, 1] * theta[1] + X_c[i, 2] * theta[2]);
      t = t + 1;
    }
    hazard_t = h[t] * exp(X_c[i, 1] * theta[1] + X_c[i, 2] * theta[2]);
    target += -hazard_t * (y_c[i] - floor(y_c[i])); 
  }

}
