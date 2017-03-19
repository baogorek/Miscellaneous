data {
  int  n_row;
  int  cluster[n_row];
  int  type[n_row];
  real y[n_row];
  int  n_clusters;
  int  subj_lower[n_clusters];
  int  subj_upper[n_clusters];
}

parameters {
  real lambda_a;
  real lambda_b;
  real<lower=0> sigma_e;
  real<lower=0> sigma_g; 
  real L[n_clusters];
}

transformed parameters {
  real<lower=0> sigma_a;
  real<lower=0> sigma_b;
  real<lower=-1,upper=1> rho1;
  real<lower=-1,upper=1> rho2;

  sigma_a = sqrt(lambda_a^2 + sigma_e^2); 
  rho1 = lambda_a^2 / (lambda_a^2 + sigma_e^2);

  sigma_b = sqrt(sigma_g^2 + lambda_b^2);
  rho2 = lambda_a * lambda_b / (sqrt(lambda_a^2 + sigma_e^2) *
                                 sqrt(lambda_b^2 + sigma_g^2));
}

model {
  lambda_a ~ normal(0, 10);
  lambda_b ~ normal(0, 10);
 
  sigma_e ~ cauchy(0, 3);
  sigma_g ~ cauchy(0, 3);

  for (i in 1:n_clusters) {
    L[i] ~ normal(0, 1);
    for (j in subj_lower[i]:subj_upper[i]) {
      if (type[j] == 1 || type[j] == 2) {
        y[j] ~ normal(lambda_a * L[i], sigma_e);
      }  else if (type[j] == 3) {
        y[j] ~ normal(lambda_b * L[i], sigma_g);  
      }
    }
  }
}
