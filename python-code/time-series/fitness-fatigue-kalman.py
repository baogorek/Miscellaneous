import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

import statsmodels as sm
from statsmodels.tsa.statespace.mlemodel import MLEModel


train_df = pd.read_csv("c:/devl/data/train_df.csv")

class FitnessFatigue(MLEModel):
    start_params = [60, 13, .07, -.27, 496, 49] # Get same ones from R TODO
    param_names = ['tau_1', 'tau_2', 'k1', 'k2', 'int', 'sigma2_e']

    def __init__(self, endog, intensities):
        super().__init__(endog, k_states = 2)
        
        self.initialize_known(np.array([0, 0]), np.eye(2))

        self.intensities = intensities
        #self.loglikelihood_burn = 1

    def update(self, params, **kwargs):

        ## state part
        elements = np.exp(np.array([-1.0 / params[0], -1.0 / params[1]]))
        self['transition'] = np.diag(elements)
        
        self['state_intercept'] = np.matmul(np.reshape(elements, (2, 1)),
                                            self.intensities)
        ## measurement part
        self['design', 0, 0] = params[2]
        self['design', 0, 1] = params[3]

        self['obs_intercept', 0, 0] = params[4]
        self['obs_cov', 0, 0] = params[5]

u = np.reshape(np.append(0, train_df.w.values[0:-1]), (1, train_df.shape[0]))
my_FitnessFatigue = FitnessFatigue(train_df.perf.values, u)
mle_results = my_FitnessFatigue.fit(method = 'nm', maxiter = 1000)

# You can really peak inside using the filter object
my_filter = mle_results.filter_results # underlying model & KF output
my_filter.shapes

#plt.plot(my_filter.forecasts[0]) # One step ahead forecasts
#plt.show()

# Let's see if we can match the predictions:
my_filter.forecasts[0][0]

pr = my_filter.predict(0, 5, 0).results # starts dynamic predictions at 0

pr.forecasts
# Note the equivalence
pr.filtered_state
pr.predicted_state

mle_results.summary()

