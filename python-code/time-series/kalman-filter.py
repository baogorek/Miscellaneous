# Using python 2.7

import simdkalman
import filterpy
import numpy as np
import pandas as pd
from matplotlib import pyplot as plt
import statsmodels.api as sm

sigma_epsilon = 2.0 # affects the measurement error
sigma_xi = 1.0 # affects the local level
sigma_omega = 1.0 # affects the seasonality

test_data = pd.read_csv("C:/devl/test-series.csv")
series = test_data.V1.values

kf = simdkalman.KalmanFilter(
  # H matrix
  state_transition = [[1.,  0,  0,  0],
                      [0, -1, -1, -1],
                      [0,  1,  0,  0],
                      [0,  0,  1,  0]],
  # Q matrix
  process_noise = np.diag([sigma_xi ** 2, sigma_omega ** 2, 0.0, 0.0]),
  observation_model = np.array([[1., 1, 0, 0]]),
  observation_noise = sigma_epsilon ** 2)

computation = kf.compute(series, 0, filtered = True, smoothed = False,
                         initial_value = [18, 5, -12, -8],
                         initial_covariance = 1 * np.eye(4))

simdkalman_result = computation.filtered.states.mean

# Taking it one step further and smoothing
smoothed = kf.smooth(series,
                     initial_value = [18, 5, -12, -8],
                     initial_covariance = .01 * np.eye(4))

print smoothed.states.mean

predicted = kf.predict(series, 12)
print predicted.observations.mean

plt.plot(predicted.observations.mean)

# Now let's try with filterpy
from filterpy.kalman import KalmanFilter
f = KalmanFilter(dim_x = 4, dim_z = 1)

# Transition matrix
f.F = np.array([[1.,  0,  0,  0],
                [0, -1, -1, -1],
                [0,  1,  0,  0],
                [0,  0,  1,  0]])

# Measurement function
f.H = np.array([[1., 1., 0., 0.]])

# Measurement noise
f.R = np.array([[sigma_epsilon **2]])

# Measurement noise covariance
f.Q = np.diag([sigma_xi ** 2, sigma_omega ** 2, 0.0, 0.0])

# Run the recursive flow
# initial state, period 1
f.x = np.array([18.0, 5, -12, -8])
# initial covariance has a nice identity default
f.P = np.diag([1, 1, 1, 1]) * 1.

# Here's how to match the simdkalman, which updates the initial state first!

# Echo initial states
f.x
f.P
f.K

f.update(series[0]) # has to be a measurement from the right period!
f.x # a posteriori estimate of x
f.P # a posteriori estimate of P 
f.K # Now there is a K

# Go to the next time point
f.predict()
f.x # apriori x, x has been updated
f.P # apriori P, has been updated
f.K # still all 0s

f.update(series[1]) # has to be a measurement from the right period!
f.x # a posteriori estimate of x
f.P # a posteriori estimate of P 
f.K # Now there is a K

# Try it as batch
f.x = np.array([18.0, 5, -12, -8])
# initial covariance has a nice identity default
f.P = np.diag([1, 1, 1, 1]) * 1.

z = f.batch_filter(series, update_first = True)
z[0]
print simdkalman_result 

# Now for statsmodels

class CustomStateSpace(sm.tsa.statespace.MLEModel):
    def __init__(self, endog,
                 initial_state = [18, 5, -12, 8],
                 initial_state_cov = np.eye(4),
                 initial_params = [1, 1, 1],
                 estimate_params = True):
        # Initialize state space model
        super(CustomStateSpace, self).__init__(endog, k_states = 4,
        # k_posdef is a parameter of the Representation class
                                              k_posdef = 2,
                                              initialization = 'known',
                             initial_state = initial_state,
                             initial_state_cov = initial_state_cov)
        self.initial_params = initial_params
        self.estimate_params = estimate_params
        # fixed components of the state space representation
        # Z matrix, called "design" here
        self['design'] = [1, 1, 0, 0]
        self['transition'] = [[1,  0,  0,  0],
                              [0, -1, -1, -1],
                              [0,  1,  0,  0],
                              [0,  0,  1,  0]]

        # R matrix that's multiplied by the state equation's error,
        # Relates to k_posdef, default selection matrix is all zeros
        self['selection', 0, 0] = 1
        self['selection', 1, 1] = 1
        if not estimate_params:
            ## State covariance function, to hard code parameters
            self['state_cov', 0, 0] = initial_params[0] 
            self['state_cov', 1, 1] = initial_params[1] 
            ### Measurement error covariance functions
            self['obs_cov', 0, 0] = initial_params[2] 

    # How parameters enter the model. Work with the args from the
    # Representation class
    def update(self, params, transformed = True, **kwargs):
        params = super(CustomStateSpace, self).update(params, transformed,
                                                      **kwargs)
        if self.estimate_params:
            self['state_cov', 0, 0] = params[0]
            self['state_cov', 1, 1] = params[1]
            self['obs_cov', 0, 0] = params[2]
        else:
            pass

    @property # This is a "decorator" or Python's get-set construct
    def start_params(self):
        return self.initial_params 

my_css = CustomStateSpace(series, [18.0, 5, -12, -8], np.diag([1, 1, 1, 1]),
                          [1, 1, 4], estimate_params = False)

res = my_css.fit(maxiter = 1)
res.summary()
np.transpose(res.filtered_state)

# Back to filterpy
f.x = np.array([18.0, 5, -12, -8])
f.P = np.diag([1, 1, 1, 1]) * 1.
z = f.batch_filter(series, update_first = True)
z[0]

# Now let's try estimating some parameters
my_css = CustomStateSpace(series, [18, 5, -12, -8], np.diag([1, 1, 1, 1]),
                          [1, 1, 4], estimate_params = True)

res = my_css.fit(maxiter = 1000)
res.summary()
np.transpose(res.filtered_state)
res.params

plt.plot(res.predict())
plt.plot(series)
plt.show()

my_css = CustomStateSpace(series, [18, 5, -12, -8], np.diag([1, 1, 1, 1]),
                          res.params, estimate_params = False)

res = my_css.fit(maxiter = 10000)
np.transpose(res.filtered_state)


# Considering external regressors -------------------------------

# Note: the MLEModel class has an exog property for exogenous
# regressors, but the Representation class does not. Since there
# are ways to incorporate exogenous regressors in state space models,
# classes like SARIMAX just turn that exogenous input into a design
# matrix

class CustomStateSpaceExog(sm.tsa.statespace.MLEModel):
    def __init__(self, endog, design,
                 initial_state = [18, 5, -12, -8, 0],
                 initial_state_cov = np.eye(5),
                 initial_params = [1, 1, 1],
                 estimate_params = True):
        # The Super class is MLEModel, not Representation 
        super(CustomStateSpaceExog, self).__init__(endog,
                                               k_states = 5,
                                               k_posdef = 2,
                                               initialization = 'known',
                             initial_state = initial_state,
                             initial_state_cov = initial_state_cov)
        self.initial_params = initial_params
        self.estimate_params = estimate_params
        self['design'] = design 
        self['transition'] = [[1,  0,  0,  0, 0],
                              [0, -1, -1, -1, 0],
                              [0,  1,  0,  0, 0],
                              [0,  0,  1,  0, 0],
                              [0,  0,  0,  0, 1]]

        # R matrix that's multiplied by the state equation's error,
        self['selection', 0, 0] = 1
        self['selection', 1, 1] = 1
        if not estimate_params:
            ## State covariance function, to hard code parameters
            self['state_cov', 0, 0] = initial_params[0] 
            self['state_cov', 1, 1] = initial_params[1] 
            ### Measurement error covariance functions
            self['obs_cov', 0, 0] = initial_params[2] 

    def update(self, params, transformed = True, **kwargs):
        params = super(CustomStateSpaceExog, self).update(params, transformed,
                                                          **kwargs)
        if self.estimate_params:
            self['state_cov', 0, 0] = params[0]
            self['state_cov', 1, 1] = params[1]
            self['obs_cov', 0, 0] = params[2]
        else:
            pass

    @property
    def start_params(self):
        return self.initial_params 

# Goal 1: Match estimates from non-exogenous case above
x = np.array([[1., 1, 0, 0, 0]]) 
reg_design = np.transpose(np.repeat(x, 12, axis = 0))
# uncomment line below to wreak havok
#reg_design[4, :] = np.repeat(1, 12) 
reg_design = np.reshape(reg_design, (1, 5, 12))

my_css_exog = CustomStateSpaceExog(series, reg_design,
                          [18, 5, -12, -8, 10],
                          np.diag([1, 1, 1, 1, 1]),
                          [1, 1, 4],
                          estimate_params = True)

res_exog = my_css_exog.fit(maxiter = 10000)
res_exog.summary()
res_exog.filtered_state


T = 1240 
x_series = np.round(np.random.normal(0, .5, T), 3)
y_series = 6 + x_series * 1.2 + np.random.normal(0, 1.5, T)
plt.scatter(x_series, y_series)
plt.show()

x = np.array([[1., 1, 0, 0, 0]]) 
reg_design = np.transpose(np.repeat(x, T, axis = 0))
reg_design[4, :] = x_series
reg_design = np.reshape(reg_design, (1, 5, T))

my_css = CustomStateSpaceExog(y_series, reg_design,
                          [3, 1, -1, -3, .8],
                          np.diag([1, 1, 1, 1, 1]),
                          [4, 4, 8.0],
                          estimate_params = True)

res = my_css.fit(method = 'nm', maxiter = 1000)
res.summary()
res.filtered_state

plt.plot(res.filtered_state[4, :])
plt.show()



