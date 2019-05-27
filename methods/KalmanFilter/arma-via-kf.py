import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

import statsmodels as sm
from statsmodels.tsa.statespace.mlemodel import MLEModel

from statsmodels.tsa.arima_process import ArmaProcess
from statsmodels.tsa.arima_model import ARMA

from statsmodels.tsa.statespace.tools import (constrain_stationary_univariate,
                                              unconstrain_stationary_univariate)

import matplotlib.pyplot as plt

# Don't forget the coefficients on the 0-lag
ar1ma2 = ArmaProcess(ar=np.array([1, -.9]), ma=np.array([1, .2, -.1]))
y = ar1ma2.generate_sample(1000)
plt.plot(y)
plt.show()

model = ARMA(y, (1, 2))
model_fit = model.fit(trend='nc')
model_fit.summary()
model_fit.llf

class AR1MA2_easy(MLEModel):
    start_params = [.5, 0.1, 0.2, .7]
    param_names = ['ar1', 'ma1', 'ma2', 'sigma2']

    def __init__(self, endog):
        super().__init__(endog, k_states=3, k_posdef=1)

        # The Harvey Representation
        self['transition', 0, 1] = 1.0
        self['transition', 1, 2] = 1.0

        self['design', 0, 0]   = 1.0

        self['selection', 0, 0] = 1.0 

        #self.initialize_approximate_diffuse()
        self.initialize_known(np.array([0, 0, 0]), np.eye(3))
        #self.initialize_stationary() # replicates the likelihood of ARMA fit

    # Note: the transform and untransform steps are crucial for ML to work
    def transform_params(self, params):
        phi = constrain_stationary_univariate(params[0:1])
        theta = constrain_stationary_univariate(params[1:3])
        sigma2 = params[3] ** 2
        return np.r_[phi, theta, sigma2]

    def untransform_params(self, params):
        phi = unconstrain_stationary_univariate(params[0:1])
        theta = unconstrain_stationary_univariate(params[1:3])
        sigma2 = params[3] ** 0.5
        return np.r_[phi, theta, sigma2]

    def update(self, params, **kwargs):

        params = super().update(params, **kwargs) # for param transformations

        self['transition', 0, 0] = params[0]

        self['selection', 1, 0] = params[1]
        self['selection', 2, 0] = params[2]
        self['state_cov', 0, 0] = params[3]

kf_model_easy = AR1MA2_easy(y)
# Without fitting the model, here is a way to get the likelihood w starting val
kf_model_easy.loglikeobs(kf_model_easy.start_params)[0:5]

kf_model_easy_fit = kf_model_easy.fit()
kf_model_easy_fit.summary()

class AR1MA2_hard(MLEModel):
    start_params = [.5, 0.1, 0.2, .7]
    param_names = ['ar1', 'ma1', 'ma2', 'sigma2']

    def __init__(self, endog):
        super().__init__(endog, k_states = 3)

        # The Harvey Representation
        self['transition', 0, 1] = 1.0
        self['transition', 1, 2] = 1.0

        self['design', 0, 0]   = 1.0

        self['selection', 0, 0] = 1.0 
        self['selection', 1, 1] = 1.0 
        self['selection', 2, 2] = 1.0 

        #self.initialize_approximate_diffuse()
        self.initialize_known(np.array([0, 0, 0]), np.eye(3))
        #self.initialize_stationary() # replicates the likelihood of ARMA fit

    # Note: the transform and untransform steps are crucial for ML to work
    def transform_params(self, params):
        phi = constrain_stationary_univariate(params[0:1])
        theta = constrain_stationary_univariate(params[1:3])
        sigma2 = params[3] ** 2
        return np.r_[phi, theta, sigma2]

    def untransform_params(self, params):
        phi = unconstrain_stationary_univariate(params[0:1])
        theta = unconstrain_stationary_univariate(params[1:3])
        sigma2 = params[3] ** 0.5
        return np.r_[phi, theta, sigma2]

    def update(self, params, **kwargs):

        params = super().update(params, **kwargs) # for param transformations

        self['transition', 0, 0] = params[0]

        self['state_cov', 0, 0] = params[3] * 1.0
        self['state_cov', 1, 0] = params[3] * params[1]
        self['state_cov', 2, 0] = params[3] * params[2]
        self['state_cov', 0, 1] = params[3] * params[1]
        self['state_cov', 1, 1] = params[3] * params[1] ** 2
        self['state_cov', 2, 1] = params[3] * params[1] * params[2]
        self['state_cov', 0, 2] = params[3] * params[2]
        self['state_cov', 1, 2] = params[3] * params[1] * params[2]
        self['state_cov', 2, 2] = params[3] * params[2] ** 2

kf_model_hard = AR1MA2_hard(y)
kf_model_hard.loglikeobs(kf_model_hard.start_params)[0:5]
filtered = kf_model_hard.filter(kf_model_hard.start_params)

A = kf_model_hard['transition', 0:, 0:]
H = kf_model_hard['design', 0:, 0:]
Q = kf_model_hard['state_cov', 0:, 0:]
R = kf_model_hard['obs_cov', 0:, 0:]

HA = np.matmul(H, A)

np.log(norm.pdf(y[0], 0, 1))

mu_0 = filtered.filtered_state[0:, 0]
Sigma_0 = filtered.filtered_state_cov[0:, 0:, 0]

E_alpha_1 = np.matmul(np.matmul(H, A), mu_0)
V_alpha_1 = (np.matmul(np.matmul(HA, Sigma_0), np.transpose(HA)) + 
             np.matmul(np.matmul(H, Q), np.transpose(H)) + R)

np.log(norm.pdf(y[1], E_alpha_1, np.sqrt(V_alpha_1)))

mu_1 = filtered.filtered_state[0:, 1]
Sigma_1 = filtered.filtered_state_cov[0:, 0:, 1]

E_alpha_2 = np.matmul(np.matmul(H, A), mu_1)
V_alpha_2 = (np.matmul(np.matmul(HA, Sigma_1), np.transpose(HA)) + 
             np.matmul(np.matmul(H, Q), np.transpose(H)) + R)

np.log(norm.pdf(y[2], E_alpha_2, np.sqrt(V_alpha_2)))




kf_model_hard_fit = kf_model_hard.fit()
kf_model_hard_fit.summary()

