import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

import statsmodels as sm
from statsmodels.tsa.statespace.mlemodel import MLEModel

train_df = pd.read_csv("c:/devl/data/train_df.csv")
n = train_df.shape[0]


class FitnessFatigue(MLEModel):
    start_params = [20, 5, .05, -.15, 400, 35]
    param_names = ['tau_1', 'tau_2', 'k1', 'k2', 'int', 'sigma2_e']

    def __init__(self, endog, input_lag1):
        super().__init__(endog, k_states = 2)
        

        self.input_lag1 = input_lag1

        #self.initialize_approximate_diffuse()

        self.initialize_known(np.array([0, 0]), 2 * np.eye(2))
        self.loglikelihood_burn = 1 # due to the lag?

    def exp_decay(self, tau):
        return np.exp(-1.0 / tau)

    def update(self, params, **kwargs):

        params = super().update(params, **kwargs) # for param transformations

        # Preparing quantities
        decay_vec =       np.reshape([self.exp_decay(params[0]),
                                      self.exp_decay(params[1])], (2, 1))
        input_lag1_vecT = np.reshape(self.input_lag1, (1, -1))

        ## state part
        self['transition']          = np.diag(np.squeeze(decay_vec))
        self['state_intercept']     = np.matmul(decay_vec, input_lag1_vecT)

        ## measurement part
        self['design', 0, 0]        = params[2]
        self['design', 0, 1]        = params[3]

        ## observation part
        self['obs_intercept', 0, 0] = params[4]
        self['obs_cov',       0, 0] = params[5]


# TODO: put this in the data frame
w_lag1 = train_df["w"].shift(periods = 1, fill_value = 0).values

my_FitnessFatigue = FitnessFatigue(train_df.perf.values, w_lag1)
mle_results = my_FitnessFatigue.fit(method = 'bfgs', maxiter = 1000)

mle_results.summary()

# Fitness filtered or smoothed?
plt.plot(mle_results.filtered_state[0, :])
plt.plot(mle_results.smoothed_state[0, :])

# Fatigue filtered or smoothed?
plt.plot(mle_results.filtered_state[1, :])

plt.plot(mle_results.filtered_state_cov[0, 0, :])


mle_results.plot_diagnostics()

# Why do I need this object when I can get the info from mle_results?
my_filter = mle_results.filter_results # underlying model & KF output
my_filter.shapes

