import math
import numpy as np
import pandas as pd

import statsmodels as sm
from statsmodels.tsa.statespace.mlemodel import MLEModel
from scipy.stats import norm


class FitnessFatigue(MLEModel):

    start_params = [500, .1, .3, 60, 15, 10, 20, 15, .5]
    param_names = ['p_0', 'k_g', 'k_h', 'tau_g', 'tau_h', 
                   'xi', 'sigma_g', 'sigma_h', 'rho_gh']

    def __init__(self, y, w):
        super().__init__(y, 2, w)
        self.initialize_known(np.array([40, 20]), np.array([[400, 0], [0, 400]]))

    def update(self, params, **kwargs):
        params = super().update(params, **kwargs)

        self['obs_intercept', 0, 0] = params[0]
        # state space model ------
        self['transition'] =  np.array([[np.exp(-1.0 / params[3]), 0],
                                        [0, np.exp(-1.0 / params[4])]])
        self['state_intercept'] = np.array([[np.exp(-1.0 / params[3])],
                                            [np.exp(-1.0 / params[4])]]) * self.exog
        ## The matrix that's premultiplied by state error: 0 by default!
        self['selection', 0, 0] = 1
        self['selection', 1, 1] = 1

        ## And, finally, Q 
        self['state_cov', 0, 0] = params[6] ** 2
        self['state_cov', 1, 1] = params[7] ** 2
        self['state_cov', 0, 1] = params[8] * params[6] * params[7]
        self['state_cov', 1, 0] = params[8] * params[6] * params[7]

        # measurement model
        self['design', 0, 0] = params[1]
        self['design', 0, 1] = -1.0 * params[2]

        self['obs_cov', 0, 0] = params[5] ** 2


#training_df = pd.read_csv('training_df.csv')
#training_df['w'].iloc[0] = 100
#training_df['w'].iloc[-1] = 100

# Using the initial values
train_block = np.concatenate([np.repeat(50, 35), np.repeat(0, 45)])
w = np.repeat([train_block], 41, axis=0).flatten()
N = w.shape[0]
#y = training_df.perf.values

ffm = FitnessFatigue(np.repeat(np.nan, N), w)
y_sim = ffm.simulate(ffm.start_params, N,
                     initial_state = np.array([40, 20]))

ffm_sim = FitnessFatigue(y_sim, w)
theta = ffm_sim.start_params


theta[5] = 10 # xi
theta[6] = 20 #sigma_g
theta[7] = 15 # sigma_h
theta[8] = .5 # rho

# theta[0] = 300 # Reminder that smaller is better
np.sum(-1 * ffm_sim.loglikeobs(theta)) # You reset start params when you do this

out_df = pd.DataFrame({'y': y_sim, 'w' : w})
out_df.to_csv('python_sim.csv', index=False)

fit = ffm_sim.fit(method = 'bfgs', start_params=theta, maxiter = 10000)
fit.summary()

p_0 = ffm['obs_intercept']  # p_0
A = ffm['transition']  # T_mat
C = ffm['design']  # C_mat
Q = ffm['state_cov']  # Q_mat
xi_sq = ffm['obs_cov']  # R_mat, or xi
B = ffm['state_intercept']
p_0
A
C
Q
xi_sq
B


