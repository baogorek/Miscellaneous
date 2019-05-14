import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

import statsmodels as sm
from statsmodels.tsa.statespace.mlemodel import MLEModel

from statsmodels.tsa.arima_process import ArmaProcess
from statsmodels.tsa.arima_model import ARMA

from statsmodels.tsa.statespace.tools import (constrain_stationary_univariate,
unconstrain_stationary_univariate)


# Don't forget the coefficients on the 0-lag
ar1ma2 = ArmaProcess(ar=np.array([1, -.7]), ma=np.array([1, .2, -.1]))
y = ar1ma2.generate_sample(10000)

model = ARMA(y, (1, 2))
model_fit = model.fit(trend='nc')
model_fit.summary()
model_fit.llf

class AR1MA2(MLEModel):
    start_params = [.5, -.01, .03, .7]
    param_names = ['ar1', 'ma1', 'ma2', 'sigma2']

    def __init__(self, endog):
        super().__init__(endog, k_states = 3, k_posdef = 3)

        #self.initialize_approximate_diffuse()
        #self.initialize_known(np.array([0, 0, 0]), np.eye(3))
        self.initialize_stationary()

    def transform_params(self, params):
        #phi = constrain_stationary_univariate(params[0])
        #theta = constrain_stationary_univariate(params[1:3])
        sigma2 = params[3] ** 2
        return np.r_[params[0:3], sigma2]

    def untransform_params(self, params):
        #phi = unconstrain_stationary_univariate(params[0])
        #theta = unconstrain_stationary_univariate(params[1:3])
        sigma2 = params[3] ** 0.5
        return np.r_[params[0:3], sigma2]

    def update(self, params, **kwargs):

        params = super().update(params, **kwargs) # for param transformations

        ## state part
        self['transition']          = np.array([[params[0], 1, 0],
                                                [0,         0, 1],
                                                [0,         0, 0]])
 
        self['design', 0, 0]        = 1 


        self['state_cov',       0, 0] = params[3]

        self['selection',       0, 0] = 1.0 
        self['selection',       0, 0] = -1.0 * params[1] 
        self['selection',       0, 0] = -1.0 * params[2]

        #self['state_cov',       0, 0] = 1 * params[3]
        #self['state_cov',       1, 0] = params[1] * params[3]
        #self['state_cov',       2, 0] = params[2] * params[3]
        #self['state_cov',       0, 1] = params[1] * params[3]
        #self['state_cov',       1, 1] = params[1] ** 2 * params[3]
        #self['state_cov',       2, 1] = params[1] * params[2] * params[3]
        #self['state_cov',       0, 2] = params[2] * params[3]
        #self['state_cov',       1, 2] = params[1] * params[2] * params[3]
        #self['state_cov',       2, 2] = params[2] ** 2 * params[3]


kf_model = AR1MA2(y)
kf_model_fit = kf_model.fit()
kf_model_fit.summary()
