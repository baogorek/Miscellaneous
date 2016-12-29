functions {
  real bound(real rho1) return sqrt( 1 / ( 1 + (1 - rho1) / (1 + rho1) ) );
}

data {
  int n_row;
  int cluster[n_row];
  int type[n_row];
  real y[n_row];
  int n_clusters;
  int subj_lower[n_clusters];
  int subj_upper[n_clusters];
}


parameters {
  real<lower=-1, upper=1> rho1;
  real<lower=-bound(rho1),upper=bound(rho1)> rho2;
  real<lower=0> sigma_a;
  real<lower=0> sigma_b;
}

transformed parameters {
  cov_matrix[3] Sigma;

  Sigma[1, 1] = sigma_a ^ 2;
  Sigma[2, 2] = sigma_a ^ 2;
  Sigma[3, 3] = sigma_b ^ 2;
  Sigma[1, 2] = sigma_a * sigma_a * rho1;
  Sigma[2, 1] = sigma_a * sigma_a * rho1;
  Sigma[1, 3] = sigma_a * sigma_b * rho2;
  Sigma[3, 1] = sigma_a * sigma_b * rho2;
  Sigma[2, 3] = sigma_a * sigma_b * rho2;
  Sigma[3, 2] = sigma_a * sigma_b * rho2;
}

model {
  int p;
  vector[2] mu_2;
  vector[3] mu_3;
  vector[2] y_vec2;
  vector[3] y_vec3;
  matrix[2, 2] Sigma_2;
  matrix[3, 3] Sigma_3;

  sigma_a ~ normal(0, 3);
  sigma_b ~ normal(0, 3);

  for (i in 1:n_clusters) {
    p = subj_upper[i] - subj_lower[i] + 1;
    if (p == 2) {
      mu_2 = rep_vector(0, 2);
      Sigma_2 = Sigma[type[subj_lower[i]:subj_upper[i]],
                      type[subj_lower[i]:subj_upper[i]]];
      y_vec2 = to_vector(y[subj_lower[i]:subj_upper[i]]);
      y_vec2 ~ multi_normal(mu_2, Sigma_2);
    } else if (p == 3) {
      mu_3 = rep_vector(0, 3);
      Sigma_3 = Sigma[type[subj_lower[i]:subj_upper[i]],
                      type[subj_lower[i]:subj_upper[i]]];

      y_vec3 = to_vector(y[subj_lower[i]:subj_upper[i]]);
      y_vec3 ~ multi_normal(mu_3, Sigma_3);
    }
  }
}
