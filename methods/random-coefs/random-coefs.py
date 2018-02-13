import pandas as pd
import numpy as np
import statsmodels.api as sm
import statsmodels.formula.api as smf

%env KERAS_BACKEND=theano
import theano
import keras
import keras.backend as K
from keras.layers import Input, Dense, concatenate, Add
from keras.models import Model
from keras.regularizers import l1, l2

from sklearn.preprocessing import OneHotEncoder

test_df = pd.read_csv("C:/devl/re_test.csv")

# Stage 1: typical random effects models using statsmodels
test_df["Intercept"] = 1

vc = {'x': '0 + x'}

md = smf.mixedlm("y ~ 1 + x", test_df, groups = test_df["unit"],
                 vc_formula = vc,
                 re_formula = "~ 1") # random intercept
        
mdf = md.fit()
mdf.summary()
mdf.scale

reffs = mdf.random_effects
smf_b_i = [reffs[i][1] for i in range(1, 31)]
smf_beta_i = mdf.params['x'] + smf_b_i

md2 = sm.MixedLM(test_df["y"], test_df[["Intercept", "x"]],
                groups = test_df["unit"],
                exog_re = test_df[["Intercept", "x"]])

mdf2 = md2.fit()
mdf2.summary()

# Stage2: Trying to get it to work with keras

## First, a simple linear regression
input_x = Input(shape = (1,)) # Batches of 1-d vectors (scalars)

lin_fn = Dense(1, use_bias = True)(input_x)

slr_model = Model(input = input_x, output = lin_fn)
slr_model.compile(loss = 'mean_squared_error', optimizer = "sgd")

slr_model.fit(test_df["x"], test_df["y"], epochs = 1000, batch_size = 100)

slr_model.get_weights()

## Second, trying to simulate the random effects modeling

enc = OneHotEncoder()
enc.fit(test_df['unit'].values.reshape(-1, 1))
unit_onehot = enc.transform(test_df['unit'].values.reshape(-1, 1))

unit_onehot = unit_onehot.toarray() # random intercepts design
unit_x = np.dot(np.diag(test_df['x']), unit_onehot) # random coefs design

n_units = unit_onehot.shape[1]
lambda_int = .07
lambda_x = .04


input_slr = Input(shape = (1,)) # alpha_bar (bias) and beta_bar (weight)
input_units_int = Input(shape = (n_units,)) 
input_units_x = Input(shape = (n_units,)) 

slr_dense = Dense(1, use_bias = True)(input_slr)
units_int_dense = Dense(1, use_bias = False,
                        kernel_regularizer = l2(lambda_int))(input_units_int)
units_x_dense = Dense(1, use_bias = False,
                      kernel_regularizer = l2(lambda_x))(input_units_x)

output_layer = Add()([slr_dense, units_int_dense, units_x_dense])

re_model = Model(inputs = [input_slr, input_units_int, input_units_x],
                 outputs = output_layer)

re_model.compile(loss = 'mean_squared_error', optimizer = "sgd")

re_model.fit([test_df["x"], unit_onehot, unit_x], test_df["y"],
             epochs = 4000, batch_size = 450)

wts = re_model.get_weights()
beta_bar = wts[0]
alpha_bar = wts[1]
u_i = wts[2]
b_i = wts[3]

beta_i = (beta_bar + b_i).reshape(-1)
unit_i = (alpha_bar + u_i).reshape(-1)

# Very poor man's variance components

np.var(beta_i)
np.var(unit_i)

# For cross validation, here is what I can use:
from sklearn.model_selection import StratifiedKFold
X = np.array([[1, 2], [3, 4], [1, 2], [3, 4]])
y = np.array([0, 0, 1, 1])
skf = StratifiedKFold(n_splits=2)
skf.get_n_splits(X, y)

print(skf)  

for train_index, test_index in skf.split(X, y):
   print("TRAIN:", train_index, "TEST:", test_index)
   X_train, X_test = X[train_index], X[test_index]
   y_train, y_test = y[train_index], y[test_index]

# There is also a Repeated version
