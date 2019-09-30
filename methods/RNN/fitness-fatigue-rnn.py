import pandas as pd
import numpy as np

import tensorflow as tf
from tensorflow.keras.layers import GRU, LSTM, Dense, Activation, Input
from tensorflow.keras.models import Sequential
from tensorflow.keras.regularizers import l2
# Resources:
# https://keras.io/getting-started/sequential-model-guide/
# https://keras.io/getting-started/faq/#how-can-i-use-stateful-rnns

train_df = pd.read_csv('train_df.csv')
train_df['perf_lag1'] = train_df.perf.shift(1)

train_df = train_df.iloc[1:, :] # don't leave that NA from the lagging in there

X = np.reshape(train_df[['perf_lag1', 'w']].values, (258, 1, 2)) 
y = np.reshape(train_df['perf'].values, (258, )) 

rnn = Sequential()
#rnn.add(GRU(10, stateful=True, batch_input_shape=(1, 1, 2)))
rnn.add(GRU(10, stateful=True, input_shape=(1, 2), batch_size = 1,
            kernel_regularizer=l2(.5)))
rnn.add(Dense(1, activation='linear', kernel_regularizer=l2(.5)))
rnn.compile(optimizer='adam', loss='mse')

for epochs in range(1000):
    rnn.fit(X, y, epochs=1,  batch_size=1, shuffle=False)
    rnn.reset_states()

