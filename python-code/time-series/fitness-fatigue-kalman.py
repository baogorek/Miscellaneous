import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

import statsmodels as sm
from statsmodels.tsa.statespace.mlemodel import MLEModel


train_df = pd.read_csv("c:/devl/data/train_df.csv")

class FitnessFatigue(MLEModel):
    start_params = [30, 8, .07, .3, 400, 1.2] # Get same ones from R TODO
    param_names = ['tau_1', 'tau_2', 'k1', 'k2', 'int', 'sigma2_e']

    def __init__(self, endog, intensities):
        super().__init__(endog, k_states = 2)

        self.intensities = intensities
        self.initialize_approximate_diffuse()
        self.loglikelihood_burn = 1

    def update(self, params, **kwargs):

        ## state part
        T = np.exp(-np.diag([params[0], params[1]]))

        self.transition = T
    
        self.state_intercept = np.matmul(T, self.intensities) # i.e., Bu
        ## measurement part

        # design is a row vector
        self['design', 0, 0] = params[2]
        self['design', 0, 1] = params[3]

        self['obs_intercept'] = [params[4]]
        self['obs_cov', 0, 0] = params[5]


u = np.reshape(np.r_[[train_df.w, train_df.w]], (2, train_df.shape[0]))
my_FitnessFatigue = FitnessFatigue(train_df.perf, u)
mle_results = my_FitnessFatigue.fit(method = 'bfgs')
mle_results.summary()

