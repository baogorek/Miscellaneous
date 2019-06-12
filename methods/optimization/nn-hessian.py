from os import environ
environ['KERAS_BACKEND']='tensorflow'

import numpy as np
import pandas as pd
import keras
from keras.models import Model
from keras.layers import Dense, Input
from keras import backend as K
from numpy.random import normal

import statsmodels.api as sm
import statsmodels.formula.api as smf

import tensorflow as tf
import tempfile

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
observed_information = -1 * H

# inverse of observed information is the covariance matrix
np.linalg.inv(observed_information)
reg_results.cov_params()

# Keras
input_x = Input(shape = (3,))

lin_fn = Dense(1, use_bias = True)(input_x)

slr_model = Model(input = input_x, output = lin_fn)
slr_model.compile(loss = 'mean_squared_error', optimizer = "sgd")
h = slr_model.fit(X, y, epochs = 1500, batch_size = 450)

output_tensor = slr_model.output
list_of_input_tensors = slr_model.trainable_weights
gradients = K.gradients(output_tensor, list_of_input_tensors)

session = tf.InteractiveSession()
session.run(tf.initialize_all_variables())
evaluated_gradients = session.run(gradients, feed_dict = {slr_model.input:X})


# Pure tensorflow fitting

z = tf.feature_column.numeric_column('z')
x = tf.feature_column.numeric_column('x')

model_dir = tempfile.mkdtemp()
model = tf.estimator.Estimator(
        model_dir = model_dir, feature_columns = x + z)
