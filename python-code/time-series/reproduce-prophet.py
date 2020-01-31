import pandas as pd
import numpy as np
import pystan

history = train_df.copy()
df = history

# setup_dataframe()
# moving to initialize_scales at line 312

start = df['ds'].min()
t_scale = df['ds'].max() - start

df['t'] = (df['ds'] - start) / t_scale

df['y_scaled'] = df['y'] / df['y'].abs().max() 
df['chemicals'] = (df.chemicals - np.mean(df.chemicals)) / np.std(df.chemicals)
df['coal'] = (df.coal - np.mean(df.coal)) / np.std(df.coal)
df['vehicles'] = (df.vehicles - np.mean(df.vehicles)) / np.std(df.vehicles)

# set auto seasonalities()

end = df['ds'].max()
dt = df['ds'].diff()
min_dt = dt.min()

# make all seasonality_features
feature_matrices = []

seconds_since_epoch = (df['ds'] - pd.datetime(1970, 1, 1)).dt.total_seconds()
t = seconds_since_epoch.values / (3600 * 24)

features = np.column_stack([trig_fn((2. * (h + 1) * np.pi * t / 365.25))
                           for h in range(5)
                           for trig_fn in (np.sin, np.cos)])

columns = [f'yearly_delim_{i + 1}' for i in range(features.shape[1])]

# NOTE: if I don't specify values than the time indices don't overlap
features['chemicals'] = df['chemicals'].values
features['coal'] = df['coal'].values
features['vehicles'] = df['vehicles'].values

# Setting changepoints
hist_size = int(np.floor(df.shape[0] * .8))
cp_indexes = np.linspace(0, hist_size - 1, 20).round().astype(np.int)
changepoints =  df.iloc[cp_indexes]['ds'].tail(-1)
changepoints_t = np.array((changepoints - start) / t_scale)

# Creating the additive regression components matrix
components = pd.DataFrame({
    'col': np.arange(features.shape[1]),
    'component': [
        x.split('_delim_')[0] for x in features.columns
    ],
})

component_cols = pd.crosstab(components['col'], components['component'])

# I'm going to skip regressor_column_matrix which seems like a lookup of sorts

feature_prior_scales = np.array([10.] * 10 + [8.] * 3)
changepoint_prior_scale = .05


STAN_CODE = """
functions {
  matrix get_changepoint_matrix(vector t, vector t_change, int T, int S) {
    // Assumes t and t_change are sorted.
    matrix[T, S] A;
    row_vector[S] a_row;
    int cp_idx;

    // Start with an empty matrix.
    A = rep_matrix(0, T, S);
    a_row = rep_row_vector(0, S);
    cp_idx = 1;

    // Fill in each row of A.
    for (i in 1:T) {
      while ((cp_idx <= S) && (t[i] >= t_change[cp_idx])) {
        a_row[cp_idx] = 1;
        cp_idx = cp_idx + 1;
      }
      A[i] = a_row;
    }
    return A;
  }

  vector linear_trend(
    real k,
    real m,
    vector delta,
    vector t,
    matrix A,
    vector t_change
  ) {
    return (k + A * delta) .* t + (m + A * (-t_change .* delta));
  }
}

data {
  int T;                // Number of time periods
  int<lower=1> K;       // Number of regressors
  vector[T] t;          // Time
  vector[T] y;          // Time series
  int S;                // Number of changepoints
  vector[S] t_change;   // Times of trend changepoints
  matrix[T,K] X;        // Regressors
  vector[K] sigmas;     // Scale on seasonality prior
  real<lower=0> tau;    // Scale on changepoints prior
}

transformed data {
  matrix[T, S] A;
  A = get_changepoint_matrix(t, t_change, T, S);
}

parameters {
  real k;                   // Base trend growth rate
  real m;                   // Trend offset
  vector[S] delta;          // Trend rate adjustments
  real<lower=0> sigma_obs;  // Observation noise
  vector[K] beta;           // Regressor coefficients
}

model {
  //priors
  k ~ normal(0, 5);
  m ~ normal(0, 5);
  delta ~ double_exponential(0, tau);
  sigma_obs ~ normal(0, 0.5);
  beta ~ normal(0, sigmas);

    y ~ normal(linear_trend(k, m, delta, t, A, t_change) + X * beta,
               sigma_obs);
}
"""

data {
  int T;                // Number of time periods
  int<lower=1> K;       // Number of regressors
  vector[T] t;          // Time
  vector[T] y;          // Time series
  int S;                // Number of changepoints
  vector[S] t_change;   // Times of trend changepoints
  matrix[T,K] X;        // Regressors
  vector[K] sigmas;     // Scale on seasonality prior
  real<lower=0> tau;    // Scale on changepoints prior
}

stan_dat = {'T': df.shape[0],
            'K': features.shape[1],
            'S': len(changepoints_t),
            'y': df['y_scaled'].values,
            't': df['t'].values,
            't_change': changepoints_t,
            'X': np.array(features),
            'sigmas': feature_prior_scales,
            'tau': changepoint_prior_scale
}

stan_init = {
    'k': np.mean(df.y),
    'm': 0.,
    'delta': np.zeros(len(changepoints_t)),
    'beta': np.zeros(features.shape[1]),
    'sigma_obs': 1.}


model = pystan.StanModel(model_code=STAN_CODE)
stan_fit_map = model.optimizing(data = stan_dat, init = stan_init)
stan_fit_mcmc = model.sampling(data = stan_dat)

stan_fit['beta']
