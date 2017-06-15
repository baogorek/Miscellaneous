library(rstan)

model <- "
parameters {
  real n_0_2; // N(0, 2)
  real<lower=0> ln1;
  real<lower=0> ln2;
  real<lower=0> g1;
  real<lower=0> iv1;
  simplex[3] theta;
  vector<lower=0, upper=1>[2] v; 
}
model {
  n_0_2 ~ normal(0, 2);
  ln1 ~ lognormal(-.95, .25);
  ln2 ~ lognormal(-.95, .4);
  g1 ~ gamma(2, 1/8.0);
  iv1 ~ inv_gamma(4, 9);
  // theta ~ dirichlet([1, 1, 1]');
  theta ~ dirichlet(rep_vector(1, 3));
  v ~ beta(1, .5);
}
"

# if y ~ lognormal(mu, sigma) then log(y) as a normal(mu, sigma) dist
fit <- stan(model_code = model, 
           control = list(adapt_delta = .80),
           iter = 5000, warmup = 10, chains = 1)

ext <- extract(fit)

for (name in setdiff(names(ext), c("lp__", "theta"))) {
  hist(ext[[name]], main = name)
  line <- readline()
}

hist(ext[["theta"]][, 2])

# Fast lognormal visualizer!
hist(exp(rnorm(10000, -.95, .25)))

# inverse-gamma(a,b)

a = 4
b = 9

x = 1.0 / rgamma(5000, a, b)
hist(x, breaks = 10)


model2 <- "
parameters {
  real z;
}
model {
  z ~ normal(0, 5);
}

generated quantities {
  vector[20] y;
  for (i in 1:20)  // creates a 20 dimension matrix for every iteration
    y[i] = normal_rng(0, 2);
}
"

fit2 <- stan(model_code = model2, iter = 100, warmup = 10, chains = 1)
ext2 <- extract(fit2)
