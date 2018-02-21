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
                                min_delta=0.00001, patience=20,
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


def get_cv_results(design, data):
  test_df, unit_onehot, unit_x = data
  skf = StratifiedKFold(n_splits=3, shuffle=True)
  cv_results = []
  for i in range(design.shape[0]):
    lambda_int, lambda_x = design[i, :]
    val_losses = []
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
                        callbacks = callbacks)

       val_losses.append(np.min(h.history['val_loss']))

    cv_results.append(np.mean(val_losses))
  return cv_results


def get_direction(point, f_step, data):
  design = ff2n(2)
  design[:, 0] = factor_to_value(design[:, 0], point[0] - f_step[0],
                                 point[0] + f_step[0])
  design[:, 1] = factor_to_value(design[:, 1], point[1] - f_step[1],
                                 point[1] + f_step[1])

  cv_results = get_cv_results(design, data)
  res_df = np.transpose(np.vstack([design[ :, 0], design[:, 1],
                                  np.array(cv_results)]))
  res_df = pd.DataFrame(res_df)
  res_df = res_df.rename(columns = {0:"x1", 1:"x2", 2:"y"})


  model = smf.ols(formula = "y ~ x1 + x2", data = res_df)
  res = model.fit()
  res.summary()

  direction = np.array([res.params["x1"], res.params["x2"]])
  return direction

