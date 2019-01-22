# Modified version of accepted answer from Cross Validated user @Maxim:
# https://stats.stackexchange.com/questions/330176/what-is-the-output-of-a-tf-nn-dynamic-rnn
import tensorflow as tf
import numpy as np

T = 2 # number of time steps
n_inputs = 3 # inputs at each time step
n_hidden = 5 # dimension of LSTM hidden layer

X = tf.placeholder(dtype=tf.float32, shape=[None, T, n_inputs])

basic_cell = tf.nn.rnn_cell.BasicLSTMCell(num_units=n_hidden)
outputs, states = tf.nn.dynamic_rnn(basic_cell, X, dtype=tf.float32)

# The input batch
X_batch = np.array([
  # t = 0      t = 1
  [[0, 1, 2], [9, 8, 7]], # instance 0
  [[3, 4, 5], [0, 0, 0]], # instance 1
  [[6, 7, 8], [6, 5, 4]], # instance 2
  [[9, 0, 1], [3, 2, 1]], # instance 3
])

sess = tf.Session()
sess.run(tf.global_variables_initializer())

outputs_val, states_val = sess.run([outputs, states], feed_dict={X: X_batch})

# @Maxim explains that state is for convenience, holds the last RNN state
# while outputs_val contains all states
print(outputs_val)
print(states_val)

print(outputs_val.shape)
# Note that the shape of the outputs is the same as X except the last dimension
# is hidden nodes instead of input features
