import tensorflow as tf
import pandas as pd
import numpy as np

from tensorflow.keras.optimizers import SGD

sleepstudy_df = pd.read_csv("/mnt/c/devl/data/sleepstudy.csv")
sleepstudy_df['one'] = 1.
ones = sleepstudy_df.one 
y = sleepstudy_df.Reaction


x_inputs = tf.keras.Input(shape=(1,))

concat_layer = tf.keras.layers.concatenate([one_inputs, x_inputs])

out_layer = tf.keras.layers.Dense(1, use_bias=False)(concat_layer)

model = tf.keras.Model(inputs = [one_inputs, x_inputs], outputs=out_layer, name = 'slr')

my_sgd = SGD(lr = .001, decay = .01, momentum = .9)
model.compile(optimizer=my_sgd, loss = 'mse', metrics=['mse'])

model.fit(x=[ones, sleepstudy_df.Days], y=y, epochs=1000)

model.get_weights()
