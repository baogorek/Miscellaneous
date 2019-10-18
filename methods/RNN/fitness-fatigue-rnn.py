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

## ARMAX Regression (Regression with MA(2) errors)
mod = ARIMA(train_df.perf, (0, 0, 2), train_df[varnames])
res = mod.fit()
res.summary()
np.corrcoef(res.fittedvalues, train_df.perf)

## SARIMAX module
mod = SARIMAX(train_df.perf, train_df[varnames], (0, 0, 2), trend='c')
res = mod.fit()
res.summary()
np.corrcoef(res.fittedvalues, train_df.perf)


scaler = StandardScaler()
train_inputs = scaler.fit_transform(train_df[varnames].values)
val_inputs = scaler.transform(val_df[varnames].values)

# Practicing to get the time sequence as the second dimension
np.reshape(np.array([[13, 14, 3, 4], [8, 9, 1, 2]]), (2, 2, 2), order='F')[1, :, 1]


X_train = np.reshape(train_inputs, (train_df.shape[0], 2, 2), order='F') 
y_train = train_df['perf'].values

X_val = np.reshape(val_inputs, (val_df.shape[0], 2, 2)) 
y_val = val_df['perf'].values

rnn = Sequential()
rnn.add(LSTM(10, stateful=True, input_shape=(2, 2), batch_size = 1,
            kernel_regularizer=l2(.5)))
#rnn.add(Dense(10, activation='relu'))#, kernel_regularizer=l2(.5)))
rnn.add(BatchNormalization())
rnn.add(Dense(1, activation='linear'))#, kernel_regularizer=l2(.5)))
rnn.compile(optimizer='adam', loss='mse', lr=0.0001, decay=1e-6)

for epoch in range(50):
    rnn.reset_states()
    rnn.fit(X_train, y_train, epochs=1,  batch_size=1, shuffle=False)

    rnn.reset_states()
    yhat_train = rnn.predict(X_train).flatten()
    mae_train = np.mean(np.abs(yhat_train - y_train))

    rnn.reset_states()
    yhat_val = rnn.predict(X_val).flatten()
    mae_val = np.mean(np.abs(yhat_val - y_val))
    print(f'epoch {epoch}, train MAE: {mae_train:.2f}, val MAE: {mae_val:.2f}')

yhat_train

plt.plot(yhat_train * 250)
plt.plot(yhat_val * 250)

yhat_train

np.corrcoef(yhat_train, y_train)
np.corrcoef(yhat_val, y_val)
np.corrcoef(yhat_val, y_val)
