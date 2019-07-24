from statsmodels.tsa.statespace.mlemodel import MLEModel
from statsmodels.tsa.arima_process import ArmaProcess

from statsmodels.tsa.arima_model import ARMA

import numpy as np

np.random.seed(20190624)
ma_invertible = ArmaProcess(ar=np.array([1]), ma=np.array([1, .5]))
ma_noninvertible = ArmaProcess(ar=np.array([1]), ma=np.array([1, 2.5]))

y_invertible = ma_invertible.generate_sample(nsample=1000, scale=1.2)
y_noninvertible = ma_noninvertible.generate_sample(nsample=1000, scale=1.2)

model = ARMA(y_invertible, (0, 1))
model_fit = model.fit(trend='nc')
model_fit.summary()
model_fit.predict()[0:10]
model.predict(params=np.array([.5322, 1.2]))[0:10]


y_with_outlier = y_noninvertible.copy()
y_with_outlier[995] = 1200.0

model2 = ARMA(y_with_outlier, (0, 1))
model2_fit = model2.fit(trend='nc')
model2_fit.summary()
model2.predict(params=np.array([.5]))[990:]


y_invertible[0:10]

class MAmodel(MLEModel):
    start_params = [.4, 1.1]
    param_names = ['theta', 'sigma2']
    def __init__(self, endog):
        super().__init__(endog, k_states=2)
        self['transition', 0, 1] = 1.0
        self['design', 0, 0]   = 1.0
        self['selection', 0, 0] = 1.0

        self.initialize_stationary()

    def update(self, params, **kwargs):

        self['selection', 1, 0] = params[0]
        self['state_cov', 0, 0] = params[1] 

model1 = MAmodel(y_invertible)
model1_fit = model1.fit(method='bfgs', miniter=1000)

model1_fit.summary()
y_invertible[0:10]
model1_fit.smoothed_state[0:10, :]
y_with_outlier = y_noninvertible
y_with_outlier[5] = 1200.0

model2 = MAmodel(y_with_outlier)

filtered3 = model2.filter([2.5, 1.2])
filtered3.filtered_state[0, :]


