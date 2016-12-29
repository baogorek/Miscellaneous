functions {
  real bound(real rho1) return sqrt( 1 / ( 1 + (1 - rho1) / (1 + rho1) ) );
}

data {
  int n_size2;
  int type_size2[n_size2, 2];
  matrix[n_size2, 2] y_size2;
  int n_size3;
  int type_size3[n_size3, 3];
  matrix[n_size3, 3] y_size3;
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

  vector[2] mu_2;
  vector[3] mu_3;
  matrix[2, 2] Sigma_2;
  matrix[3, 3] Sigma_3;

  sigma_a ~ cauchy(0, 3);
  sigma_b ~ cauchy(0, 3);

  for (i in 1:n_size2) {
    mu_2 = rep_vector(0, 2);
    Sigma_2 = Sigma[type_size2[i], type_size2[i]];
    y_size2[i, ]' ~ multi_normal(mu_2, Sigma_2);
  } 

  for (i in 1:n_size3) {
    mu_3 = rep_vector(0, 3);
    Sigma_3 = Sigma[type_size3[i], type_size3[i]];
    y_size3[i, ]' ~ multi_normal(mu_3, Sigma_3);
  } 

}
