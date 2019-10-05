import pandas as pd
import numpy as np
from matplotlib import pyplot as plt
import tensorflow as tf
from sklearn.preprocessing import StandardScaler
from tensorflow.keras.layers import GRU, LSTM, Dense, Activation, Input
from tensorflow.keras.models import Sequential
from tensorflow.keras.regularizers import l2
# Resources:
# https://keras.io/getting-started/sequential-model-guide/
# https://keras.io/getting-started/faq/#how-can-i-use-stateful-rnns

train_df = pd.read_csv('train_df.csv')
train_df['perf_lag1'] = train_df.perf.shift(1)

val_df = pd.read_csv('val_df.csv')
val_df['perf_lag1'] = val_df.perf.shift(1)


train_df = train_df.iloc[1:, :] # don't leave that NA from the lagging in there
val_df = val_df.iloc[1:, :] # don't leave that NA from the lagging in there

scaler = StandardScaler()
train_inputs = scaler.fit_transform(train_df[['perf_lag1', 'w']].values)
# TODO: Understand the scaler
val_inputs = scaler.fit_transform(val_df[['perf_lag1', 'w']].values)

X_train = np.reshape(train_inputs, (258, 1, 2)) 
y_train = np.reshape(train_df['perf'].values, (258, )) / 250.

X_val = np.reshape(val_inputs, (258, 1, 2)) 
y_val = np.reshape(val_df['perf'].values, (258, )) / 250.

rnn = Sequential()
rnn.add(GRU(100, stateful=True, input_shape=(1, 2), batch_size = 1))
            #kernel_regularizer=l2(.5)))
rnn.add(Dense(1, activation='linear', kernel_regularizer=l2(.5)))
rnn.compile(optimizer='adam', loss='mse')

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

plt.plot(yhat_train * 250)
plt.plot(yhat_val * 250)

np.corrcoef(yhat_train, y_train)
np.corrcoef(yhat_val, y_val)
np.corrcoef(yhat_val, y_val)
