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

from keras.callbacks import ModelCheckpoint, EarlyStopping

from sklearn.preprocessing import OneHotEncoder
from sklearn.model_selection import StratifiedKFold

%run utils.py

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

enc = OneHotEncoder()
enc.fit(test_df['unit'].values.reshape(-1, 1))
unit_onehot = enc.transform(test_df['unit'].values.reshape(-1, 1))

unit_onehot = unit_onehot.toarray() # random intercepts design
unit_x = np.dot(np.diag(test_df['x']), unit_onehot) # random coefs design

n_units = unit_onehot.shape[1]

# Test the model out
re_model = create_model(n_units, .006, .1, .001, 0, .9) 
X_all = [test_df["x"], unit_onehot, unit_x]
y_all = test_df["y"]
h = re_model.fit(X_all, y_all, epochs = 5500, batch_size = 450)
y_pred = re_model.predict(X_all).flatten()

wts = re_model.get_weights()
beta_bar = wts[0]
alpha_bar = wts[1]
u_i = wts[2]
b_i = wts[3]

beta_i = (beta_bar + b_i).reshape(-1)
unit_i = (alpha_bar + u_i).reshape(-1)

# Poor man's variance components
np.std(beta_i)
np.std(unit_i)
np.std(y_all - y_pred)

# Trying a conjugate gradient approach

region = np.array([[0, 1000], [0, 1000]])
f_step = np.array([1, 1]) # Will be different for every combo of vars!
line_step = 1

results_df = pd.DataFrame()

point = np.array([100, 25])
data = (test_df, unit_onehot, unit_x)
direction = get_direction(point, f_step, data, region)

#points_tested = np.array([])
line_step = line_step * .75
stop_flag = False
while stop_flag == False:

  design = enforce_boundary_2d(point, direction, line_step, region)
  #np.array(point + direction * line_step)
  for i in range(2, 5):
      design = np.vstack([design, enforce_boundary_2d(point, direction,
                                                      line_step * i, region)])

      #design, point + direction * line_step * i])
  
  cv_results = get_cv_results(design, (test_df, unit_onehot, unit_x))
  results_df = pd.concat([results_df, pd.DataFrame(np.hstack([design,
                          np.array(cv_results).reshape(-1, 1)]))])
  if cv_results[-1] > np.min(cv_results): # something better earlier
      stop_flag = True
      point = design[np.argmin(cv_results), :]
      print("Found minimum. Point has changed to %s" % str(point))
  else:
      point = design[-1]
      print("Continuing Line Search. point has changed to %s" % str(point))

# TODO: Implement f_step decay
# TODO; some kind of visual to show where point is moving
f_step = f_step * .75
new_direction = get_direction(point, f_step, data, region)

direction = new_direction + direction
direction = direction / np.linalg.norm(direction)

