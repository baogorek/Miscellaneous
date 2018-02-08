import numpy as np
import statsmodels.api as sm

# TODO: replicate pure Kalman filtering. Then ensure you can estimate

class CustomStateSpace(sm.tsa.statespace.MLEModel):
    def __init__(self, endog,
                 initial_state = [20, 1, 0, 0, 0, 0, 0],
                 initial_state_cov = np.eye(7),
                 initial_params = [1, 1, 1]):
        # Initialize state space model
        super(CustomStateSpace, self).__init__(endog, k_states = 7,
        # k_posdef is a parameter of the Representation class
                                              k_posdef = 2,
                                              initialization = 'known',
                             initial_state = initial_state,
                             initial_state_cov = initial_state_cov)

        # fixed components of the state space representation
        # Z matrix, called "design" here
        self['design'] = [1, 1, 0, 0, 0, 0, 0]
        self['transition'] = [[1, 0,  0,  0, 0,  0,   0],
                           [0, -1, -1, -1, -1, -1, -1],
                           [0,  1,  0,  0,  0,  0,  0],
                           [0,  0,  1,  0,  0,  0,  0],
                           [0,  0,  0,  1,  0,  0,  0],
                           [0,  0,  0,  0,  1,  0,  0],
                           [0,  0,  0,  0,  0,  1,  0]]

        # R matrix that's multiplied by the state equation's error,
        # Relates to k_posdef, default selection matrix is all zeros
        self['selection', 0, 0] = 1 
        self['selection', 1, 1] = 1 

    # How parameters enter the model. Work with the args from the
    # Representation class
    def update(self, params, transformed = True, **kwargs):
        params = super(CustomStateSpace, self).update(params, transformed,
                                                      **kwargs)
        # State covariance function
        self['state_cov', 0, 0] = params[0]
        self['state_cov', 1, 1] = params[1]
        # Measurement error covariance functions 
        self['obs_cov', 0, 0] = params[2]

    @property # This is a "decorator" or Python's get-set construct
    def start_params(self):
        return initial_params 


class AR2(sm.tsa.statespace.MLEModel):
    def __init__(self, endog):
        # Initialize the state space model
        super(AR2, self).__init__(endog, k_states=2, k_posdef=1,
                                  initialization='stationary')

        # Setup the fixed components of the state space representation
        self['design'] = [1, 0]
        self['transition'] = [[0, 0],
                                  [1, 0]]
        self['selection', 0, 0] = 1

    # Describe how parameters enter the model
    def update(self, params, transformed=True, **kwargs):
        params = super(AR2, self).update(params, transformed, **kwargs)

        self['transition', 0, :] = params[:2]
        self['state_cov', 0, 0] = params[2]

    # Specify start parameters and parameter names
    @property
    def start_params(self):
        return [0,0,1]  # these are very simple

