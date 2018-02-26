import numpy as np
import pandas as pd
import statsmodels.formula.api as smf
from pyDOE import ff2n
from sklearn.model_selection import StratifiedKFold

from keras.layers import Input, Dense, concatenate, Add
from keras.models import Model
from keras.regularizers import l1, l2
from keras.optimizers import SGD
from keras.callbacks import ModelCheckpoint, EarlyStopping

# For cross validation, here is what I can use:
callbacks = [
  EarlyStopping(monitor='val_loss',
                                min_delta=0.000001, patience=60,
                                verbose=1, mode='auto') #,
  #ModelCheckpoint('model_checkpoint.h5', monitor='val_loss',
  #                 verbose=1, save_best_only=True,
  #                 mode='min')

]


def factor_to_value(x, min_val, max_val):
    d = max_val - min_val
    x = x * (d / 2.0)
    x = x + min_val + (d / 2.0)
    return x

def create_model(n_units, lambda_int, lambda_x, lr, decay, momentum):

  my_sgd = SGD(lr = lr, decay = decay, momentum = momentum, nesterov = True)

  input_slr = Input(shape = (1, )) # alpha_bar (bias) and beta_bar (weight)
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

  re_model.compile(loss = 'mean_squared_error', optimizer = my_sgd)

  #re_model.save_weights('re_model_weights.h5')

  return re_model


def get_cv_results(design, data, cv_splits=10):
  test_df, unit_onehot, unit_x = data
  cv_results = []
  for i in range(design.shape[0]):
    lambda_int, lambda_x = design[i, :]
    val_losses = []
    for rep in range(3): # Almost like bootstrap. Reshuffling
      
      cv_val_losses = []
      skf = StratifiedKFold(n_splits=10, shuffle=True)
      for train_index, test_index in skf.split(unit_x, test_df['unit']):
         re_model = create_model(unit_onehot.shape[1], lambda_int, lambda_x,
                                 .01, .0001, .92)

         X_train = [test_df["x"][train_index], unit_onehot[train_index],
                    unit_x[train_index]]
         X_test = [test_df["x"][test_index], unit_onehot[test_index],
                    unit_x[test_index]]

         y_train, y_test = test_df["y"][train_index], test_df["y"][test_index]
         h = re_model.fit(X_train, y_train,
                          epochs = 15000, batch_size = 450,
                          validation_data = (X_test, y_test),
                          callbacks = callbacks, verbose = 0)
         cv_val_losses.append(np.min(h.history['val_loss']))

      val_losses.append(np.mean(cv_val_losses))
    cv_results.append(np.mean(val_losses)) 
  return cv_results

def enforce_boundary_1d(position, step, boundaries):
  proposal = position + step 
  while proposal < boundaries[0] or proposal > boundaries[1]:
      step = step / 2.0
      proposal = position + step 
      print("oob. decreasing step size")
  return proposal

def enforce_boundary_2d(position, direction, step, boundaries):
  proposal = position + direction * step
  out_of_bounds1 = (boundaries[0][0] > proposal[0] or
                    proposal[0] > boundaries[0][1])
  out_of_bounds2 = (boundaries[1][0] > proposal[1] or
                    proposal[1] > boundaries[1][1])
  while out_of_bounds1 or out_of_bounds2:
    step = step / 2.0
    proposal = position + direction * step
    out_of_bounds1 = (boundaries[0][0] > proposal[0] or
                      proposal[0] > boundaries[0][1])
    out_of_bounds2 = (boundaries[1][0] > proposal[1] or
                      proposal[1] > boundaries[1][1])
    print("oob. decreasing step size")
  
  return proposal

#position = np.array([1,1])
#direction = np.array([1, 1])
#step = .5
#boundaries = np.array([[0, 2], [0, 2]])

def get_direction(point, f_step, data, boundaries):
  design = ff2n(2)
  design[:, 0] = factor_to_value(design[:, 0],
       enforce_boundary_1d(point[0], - f_step[0], boundaries[0]),
       enforce_boundary_1d(point[0], f_step[0], boundaries[0]))
  design[:, 1] = factor_to_value(design[:, 1],
        enforce_boundary_1d(point[1], -f_step[1], boundaries[1]),
        enforce_boundary_1d(point[1],f_step[1], boundaries[1]))
  print(design)
  cv_results = get_cv_results(design, data)
  res_df = np.transpose(np.vstack([design[ :, 0], design[:, 1],
                                  np.array(cv_results)]))
  res_df = pd.DataFrame(res_df)
  res_df = res_df.rename(columns = {0:"x1", 1:"x2", 2:"y"})


  model = smf.ols(formula = "y ~ x1 + x2", data = res_df)
  res = model.fit()
  res.summary()

  direction = np.array([res.params["x1"], res.params["x2"]])
  return (direction / np.linalg.norm(direction))

