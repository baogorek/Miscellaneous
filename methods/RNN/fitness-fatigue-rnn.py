import pandas as pd
import numpy as np
from matplotlib import pyplot as plt

import statsmodels.api as sm
import statsmodels.formula.api as smf
from statsmodels.tsa.arima_model import ARIMA
from statsmodels.tsa.statespace.sarimax import SARIMAX

from statsmodels.tsa.arima_model import ARIMA
import tensorflow as tf
from sklearn.preprocessing import StandardScaler
from tensorflow.keras.layers import GRU, LSTM, Dense, Activation, Input, BatchNormalization
from tensorflow.keras.models import Sequential
from tensorflow.keras.regularizers import l2
# Resources:
# https://keras.io/getting-started/sequential-model-guide/
# https://keras.io/getting-started/faq/#how-can-i-use-stateful-rnns

train_df = pd.read_csv('train_df.csv')
train_df['perf_lag1'] = train_df.perf.shift(1)
train_df['perf_lag2'] = train_df.perf.shift(2)
train_df['w_lag1'] = train_df.w.shift(1)
train_df['w_lag2'] = train_df.w.shift(2)
train_df = train_df.iloc[2:, :] # don't leave that NA from the lagging in there

val_df = pd.read_csv('val_df.csv')
val_df['perf_lag1'] = val_df.perf.shift(1)
val_df['perf_lag2'] = val_df.perf.shift(2)
val_df['w_lag1'] = val_df.w.shift(1)
val_df['w_lag2'] = val_df.w.shift(2)
val_df = val_df.iloc[2:, :] # ditto


varnames = ['perf_lag1', 'perf_lag2', 'w_lag1', 'w_lag2']

# Lagged-Variable Regression
## OLS Regression
mod = smf.ols(formula='perf ~ perf_lag1 + perf_lag2 + w_lag1 + w_lag2',
              data=train_df)
res = mod.fit()
res.rsquared
res.summary() # Same as in the Medium article
res.fittedvalues
np.corrcoef(res.fittedvalues, train_df.perf)
np.mean((res.fittedvalues - train_df.perf) ** 2.) # 71.15

## ARMAX Regression (Regression with MA(2) errors)
mod = ARIMA(train_df.perf, (0, 0, 2), train_df[varnames])
res = mod.fit()
res.summary()
np.corrcoef(res.fittedvalues, train_df.perf)

np.mean((res.fittedvalues - train_df.perf) ** 2.) # 51.99
# .967
## SARIMAX module

mod = SARIMAX(train_df.perf, train_df[varnames], (0, 0, 2), trend='c')
res = mod.fit()
res.summary()
np.corrcoef(res.fittedvalues, train_df.perf)


scaler = StandardScaler()
train_inputs = scaler.fit_transform(train_df[varnames].values)
val_inputs = scaler.transform(val_df[varnames].values)

## Rerun the Regression to make sure scaling didn't hurt
ols_exog = sm.add_constant(train_inputs)
mod = sm.OLS(train_df.perf, ols_exog)
res = mod.fit()
res.rsquared
res.summary() # Same as in the Medium article
res.fittedvalues
np.corrcoef(res.fittedvalues, train_df.perf)


# Experiment: LSTM with all necessary inputs
X_train = np.reshape(train_inputs, (train_df.shape[0], 1, 4))
y_train = train_df['perf'].values

X_val = np.reshape(val_inputs, (val_df.shape[0], 1, 4)) 
y_val = val_df['perf'].values

rnn = Sequential()
rnn.add(LSTM(200, stateful=True, input_shape=(1, 4), batch_size = 257))
#rnn.add(Dense(100))
rnn.add(Dense(1, activation='linear'))#, kernel_regularizer=l2(.5)))
rnn.compile(optimizer='adam', loss='mse', lr=.0001) #lr=0.0001, decay=1e-6)

for epoch in range(500):
    rnn.reset_states()
    rnn.fit(X_train, y_train, epochs=1, shuffle=False)
    #rnn.fit(X_train, y_train)

    rnn.reset_states()
    yhat_train = rnn.predict(X_train).flatten()
    mse_train = np.mean(np.square(yhat_train - y_train))

    rnn.reset_states()
    yhat_val = rnn.predict(X_val).flatten()
    mse_val = np.mean(np.square(yhat_val - y_val))
    print(f'epoch {epoch}, train MSE: {mse_train:.2f}, val MSE: {mse_val:.2f}')

yhat_train

# Dense: train MAE: 6.91, val MAE: 6.88, .9546

np.corrcoef(yhat_train, y_train)
np.corrcoef(yhat_val, y_val)

# Redoing with a focus on the length 2 sequences
train_df = pd.read_csv('train_df.csv')
vars_df = train_df[['w', 'perf']]


scaler = StandardScaler()
train_inputs = scaler.fit_transform(vars_df.values)

array_list = list()
for j in range(2, train_df.shape[0]):
    array_list.append([train_inputs[j - 1, :].tolist(),
                       train_inputs[j - 2, :].tolist()])


X_train = np.array(array_list)
y_train = train_df.perf.values[2:]

rnn = Sequential()
rnn.add(LSTM(200, input_shape=(2, 2), batch_size = 20))
#rnn.add(BatchNormalization())
rnn.add(Dense(1, activation='linear'))#, kernel_regularizer=l2(.5)))
rnn.compile(optimizer='adam', loss='mse', lr=0.0001) #lr=0.0001, decay=1e-6)

for epoch in range(500):
    #rnn.reset_states()
    rnn.fit(X_train, y_train, epochs=1)

    yhat_train = rnn.predict(X_train).flatten()
    mae_train = np.mean(np.abs(yhat_train - y_train))

    #rnn.reset_states()
    #yhat_val = rnn.predict(X_val).flatten()
    #mae_val = np.mean(np.abs(yhat_val - y_val))
    #print(f'epoch {epoch}, train MAE: {mae_train:.2f}, val MAE: {mae_val:.2f}')

yhat_train

np.corrcoef(yhat_train, y_train)
# Topping out at .957




val_inputs = scaler.transform(val_df[varnames].values)


train_df = train_df.iloc[2:, :] # don't leave that NA from the lagging in there

val_df = pd.read_csv('val_df.csv')
val_df['perf_lag1'] = val_df.perf.shift(1)
val_df['perf_lag2'] = val_df.perf.shift(2)
val_df['w_lag1'] = val_df.w.shift(1)
val_df['w_lag2'] = val_df.w.shift(2)
val_df = val_df.iloc[2:, :] # ditto


varnames = ['perf_lag1', 'perf_lag2', 'w_lag1', 'w_lag2']


