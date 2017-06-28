# TODO: move to stan numerical integration example file
# Testing Stan integration
stan_str <- "
functions {
  real[] x_sq(real t, real[] y, real[] theta, real[] x_r, int[] x_i) {
    real z[1];
    z[1] = t * t;
    return z;
  }
}

data {
  real t0;
  real y0[1];
}

transformed data {
  real x_r[0];
  int x_i[0];
  real ts[2];
  ts = {1, 2};
}

parameters {
  real z;
}

model {
  real y[1];
  real theta[1];
  real ans[2, 1]; // first dim is number of eval points, second is output dim

  theta = {0};

  y = x_sq(t0, y0, theta, x_r, x_i);
  print(y);

  ans = integrate_ode_rk45(x_sq, y0, t0, ts, theta, x_r, x_i);
  print(ans);

  z ~ normal(0, 1);
}
"

stan_model_fit <- stan(model_code = stan_str,
                       data = list(t0 = 0, y0 = c(0)),
                       iter = 5,
                       warmup = 0,
                       chains = 1)

