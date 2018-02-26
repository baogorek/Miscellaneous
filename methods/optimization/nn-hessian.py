import numpy as np
import pandas as pd
import keras
from keras.models import Model
from keras.layers import Dense, Input
from numpy.random import normal

import statsmodels.api as sm
import statsmodels.formula.api as smf

N = 1000
test_df = pd.DataFrame()
test_df["z"] = normal(size=N)
test_df["x"] = normal(size=N)
test_df["y"] = 3.5 + 1.2 * test_df.x - .5 * test_df.z + normal(size=N)

X = np.array(test_df[["z", "x"]])
y = np.array(test_df["y"])

X = sm.add_constant(X)

reg_model = sm.GLM(y, X)
reg_results = reg_model.fit()
reg_results.summary()
y_hat = reg_results.predict(X)
np.var(y - y_hat)

H = reg_model.hessian(reg_results.params)
# The following two are equal
observed_information = -1 * np.linalg.inv(H)
reg_results.cov_params()

np.sqrt(np.diag(observed_information))

# Keras
input_x = Input(shape = (2,))

lin_fn = Dense(1, use_bias = True)(input_x)

slr_model = Model(input = input_x, output = lin_fn)
slr_model.compile(loss = 'mean_squared_error', optimizer = "sgd")
slr_model.fit(X, y, epochs = 1000, batch_size = 450)

slr_model.get_weights()
