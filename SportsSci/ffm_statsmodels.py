import math
import numpy as np
import pandas as pd

import statsmodels as sm
from statsmodels.tsa.statespace.mlemodel import MLEModel
from scipy.stats import norm

training_df = pd.read_csv('training_df.csv')

class FitnessFatigue(MLEModel):

    start_params = [20, 5, .05, -.15, 400, 35]
    param_names = ['tau_1', 'tau_2', 'k1', 'k2', 'int', 'sigma2_e']

    def __init__(self, perf, training):
        super().__init__(perf, k_states = 2)
        self.training_lag1 = training.values
        #(training
        #    .shift(periods = 1, fill_value = 0).values)

        #self.initialize_approximate_diffuse()
        self.initialize_known(np.array([0, 0]), 1.0 * np.eye(2))

    def exp_decay(self, tau):
        return np.exp(-1.0 / tau)

    def update(self, params, **kwargs):
        params = super().update(params, **kwargs)
        # Preparing quantities
        decay_vec = np.reshape([self.exp_decay(params[0]),
                                self.exp_decay(params[1])], (2, 1))
        input_lag1_vecT = np.reshape(self.training_lag1, (1, -1))
        # state space model
        self['transition'] = np.diag(np.squeeze(decay_vec))
        self['state_intercept'] = np.matmul(decay_vec,
                                            input_lag1_vecT)
        # measurement model
        self['design', 0, 0] = params[2]
        self['design', 0, 1] = params[3]
        self['obs_intercept', 0, 0] = params[4]
        self['obs_cov', 0, 0] = params[5]


# Modifying the training to have impulse first and last day
training_df['w'].iloc[0] = 100
training_df['w'].iloc[-1] = 100

# Using the initial values
ffm = FitnessFatigue(training_df.perf, training_df.w)

p0 = ffm['obs_intercept']  # p_0
A = ffm['transition']  # T_mat
H = ffm['design']  # H_mat
Q = ffm['state_cov']  # Q_mat
R = ffm['obs_cov']  # R_mat

sint = ffm['state_intercept']
ffm['obs_intercept', 0, 0]
ffm['obs_cov', 0, 0]
y = training_df.perf

HA = np.matmul(H, A)

# Filtering quantities
filtered = ffm.filter(ffm.start_params)
loglike = ffm.loglikeobs(ffm.start_params)
loglike5 = loglike[0:5]
mu_0 = filtered.filtered_state[0:, 0]
Sigma_0 = filtered.filtered_state_cov[0:, 0:, 0]

resid = filtered.resid
fitted = filtered.fittedvalues
sintercept = ffm['state_intercept']
mu = filtered.filtered_state[0:, 0:5]
Sigma = filtered.filtered_state_cov[0:, 0:, 0]





def logdensity(x, mu, sigma):
    return -.5 * math.log(2 * math.pi * sigma**2) - (x - mu) ** 2 / (2 * sigma ** 2)


# Initial likelihood
??

# mu_0
E_alpha_1 = np.matmul(np.matmul(H, A), mu_0)
V_alpha_1 = (np.matmul(np.matmul(HA, Sigma_0), np.transpose(HA)) +
             np.matmul(np.matmul(H, Q), np.transpose(H)) + R)
logdensity(y[1], 400 + E_alpha_1, np.sqrt(V_alpha_1))

# Elusive time point t=4

mu_3 = filtered.filtered_state[0:, 2]
Sigma_3 = filtered.filtered_state_cov[0:, 0:, 2]

E_alpha_4 = np.matmul(np.matmul(H, A), mu_3)
V_alpha_4 = (np.matmul(np.matmul(HA, Sigma_3), np.transpose(HA)) +
             np.matmul(np.matmul(H, Q), np.transpose(H)) + R)
logdensity(y[3], 400 + .103799, np.sqrt(35.0086))

# New elusive time point t=5

mu_4 = filtered.filtered_state[0:, 3]
Sigma_4 = filtered.filtered_state_cov[0:, 0:, 3]

w = 15.0
E_alpha_5 = np.matmul(np.matmul(H, A), mu_4) + np.matmul(H, np.array([w * np.exp(-1/20), w * np.exp(1/5)]))
V_alpha_5 = (np.matmul(np.matmul(HA, Sigma_4), np.transpose(HA)) +
             np.matmul(np.matmul(H, Q), np.transpose(H)) + R)
np.log(norm.pdf(y[4], 400 + .10515565, np.sqrt(35.00628)))
loglike[0:5]


# Goal: figure out how filtering works manually with the state intercept.
# Get the likelihood.

# Fitting via Maximum Likelihood
mle_results = ffm.fit(method = 'bfgs', maxiter = 1000)
mle_results.summary()

# Log Likelihood
mle_results.llf

# AIC
-2 * (mle_results.llf - len(ffm.start_params))


# Plotting fitness and fatigue
fig = plt.figure(figsize = (12, 8))
plt.rcParams.update({'font.size': 18})
plt.plot(mle_results.filtered_state[0, :], color='green',
         label='fitness')
plt.plot(mle_results.filtered_state[1, :], color='red',
         label='fatigue')
plt.title("Filtered estimates of state vector (Fitness and " +
          "Fatigue) over time")
plt.xlabel('Time')
plt.ylabel('Filtered state estimate')
plt.legend()
plt.show()


