functions {
  real convolve_training(vector w, real tau) {
    real convolution = 0;
    int L = dims(w[])[1];
    for (i in 1:(L - 1)) {
      convolution += w[i] * exp(-(L - i) / tau);
    }
    return convolution;
  }
}

// Data blocks, can add transformed data after data
data {
  int<lower=0> T;   // Number of time points
  vector[T] p;        // performance vector at time points
  vector[T] w;        // training impulses at time T
}

// parameters
parameters {
  real<lower=0> p0;          // baseline performance
  real<lower=0> k_fitness;
  real<lower=0> k_fatigue;
  real<lower=0> tau_fitness;
  real<lower=0> tau_fatigue;
  real<lower=0> sigma;       // standard deviation of performance
}

// Model block: priors and likelihoods
model {
  vector[T] E_p;
  vector[T] fitness;
  vector[T] fatigue;

  // priors 
  p0 ~ normal(500, 100);
  k_fitness ~ normal(.08, .04);
  k_fatigue ~ normal(.30, .10);
  tau_fitness ~ normal(60, 10);
  tau_fatigue ~ normal(15, 5);
  sigma ~ normal(50, 20);

  // derived quantities
  for (t in 1:T) {
     fitness[t] = convolve_training(w[1:t], tau_fitness);
     fatigue[t] = convolve_training(w[1:t], tau_fatigue);
  }

  //print("fatigue =", fatigue[1:5]);

  for (t in 1:T) {
    E_p[t] = p0 + k_fitness * fitness[t] - k_fatigue * fatigue[t];
  }

  // likelihood
  for (t in 1:T) {
    p[t] ~ normal(E_p[t], sigma);
  }
}

// Generated quantites
// So it appears you can access data, parameters, and functions here, but not
// variables used in the models block. You'd have to recreate them
generated quantities {
  real f1 = convolve_training(w[1:1], 60);
  real f2 = convolve_training(w[1:2], 60);
  real f3 = convolve_training(w[1:3], 60);
  vector[3] w3 = w[1:3];
}
