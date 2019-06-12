
import numpy as np
import statsmodels as sm

from statsmodels.tsa.arima_process import ArmaProcess
from matplotlib import pyplot as plt
#np.random.seed(123)

# For both AR and MA parameters, one must include the "zero lag" values
ar_params = np.array([1, -0.95]) # based on notation, this is phi = .95
ma_params = np.array([1])

my_arma = ArmaProcess(ar_params, ma_params)
x = my_arma.generate_sample(nsample = 200)
x_test = my_arma.generate_sample(nsample = 100)

plt.plot(x)
plt.show()

def moving_average(a, n=3) :
    # Stack Overflow questions/14313510, has a left end effect
    ret = np.cumsum(a, dtype=float)
    ret[n:] = ret[n:] - ret[:-n]
    ma = ret[n - 1:] / n
    front_pad = np.cumsum(a)[:(n - 1)] / np.arange(1, n)
    padded_ma = np.pad(ma, ((n - 1), 0), 'constant',
                       constant_values = (tuple(front_pad), ()))
    return padded_ma

#x_ma = moving_average(x, 3)
#x_ma_test = moving_average(x, 3)
#pad_0 = x[0]
#pad_1 = (x[0] + x[1]) / 2.0
#
#ys = np.pad(x_ma, (2, 0), 'constant', constant_values = ((pad_0, pad_1), ()))

ys = moving_average(x)
ys_test = moving_average(x_test)
Xs = np.reshape(x, (1, 200, 1))
Xs_test = np.reshape(x_test, (1, 100, 1))

# challenge: predict on Xs_shape

plt.plot(x)
plt.plot(ys)
plt.show()

plt.scatter(ys, x)
plt.show()

import tensorflow as tf

T = 200 # number of time steps per batch - Need to think about
n_inputs = 1 # rolling set of inputs 10 units back?
n_hidden = 10 # dim of LSTM hidden layer, much better with 10 nodes than 5, 3

# Hard-coding a single batch of size T, which isn't best practice
X = tf.placeholder(dtype = tf.float32, shape = [1, T, n_inputs]) #dynamic_rnn requires batch size
y = tf.placeholder(dtype = tf.float32, shape = [T])

basic_cell = tf.nn.rnn_cell.BasicLSTMCell(num_units=n_hidden)
hidden, _ = tf.nn.dynamic_rnn(basic_cell, X, dtype=tf.float32)

w1 = tf.Variable(tf.random_normal([1, n_hidden, 1]), dtype = tf.float32) # Weights hidden to output
output = tf.matmul(hidden, w1) # start without a bias

mse = tf.keras.losses.mean_squared_error(y, tf.squeeze(output))
loss = tf.reduce_mean(mse, name = "loss")


sess = tf.Session()
sess.run(tf.global_variables_initializer())

# Initial loss
sess.run([loss], feed_dict = {X: Xs, y: ys})

y_hat = sess.run([output], feed_dict = {X: Xs})
plt.plot(np.squeeze(y_hat))
plt.plot(ys)
plt.show()

train_op = tf.train.GradientDescentOptimizer(0.05).minimize(loss)
for i in range(200):
    loss_val, something = sess.run([loss, train_op], feed_dict = {X: Xs, y: ys})
    print("Iteration %d: loss = %g" % (i, loss_val))

train_op = tf.train.GradientDescentOptimizer(0.025).minimize(loss)
for i in range(200):
    loss_val, something = sess.run([loss, train_op], feed_dict = {X: Xs, y: ys})
    print("Iteration %d: loss = %g" % (i, loss_val))

# Final loss
sess.run([loss], feed_dict = {X: Xs, y: ys})

y_hat = sess.run([output], feed_dict = {X: Xs})
plt.plot(np.squeeze(y_hat))
plt.plot(ys)
plt.show()

# Predicting on X_test

Xs_test_padded = np.append(Xs_test, np.reshape(np.zeros(100), (1, 100, 1)),
                           axis = 1)

assert(Xs_test_padded.shape == Xs.shape)
y_hat_test = sess.run([output], feed_dict = {X: Xs_test_padded})

plt.plot(np.squeeze(y_hat_test))
plt.plot(ys_test)
plt.show()





# References
# https://stats.stackexchange.com/questions/330176/what-is-the-output-of-a-tf-nn-dynamic-rnn
# On whether people usually need a dense layer: 
# https://stats.stackexchange.com/questions/242754/lstm-cell-output-activation-for-series
# Explains the shape of the weights:
# https://stackoverflow.com/questions/51963182/understanding-tensorflow-basiclstmcell-kernel-and-bias-shape
# Batch sizes and LSTMs:
# https://machinelearningmastery.com/use-different-batch-sizes-training-predicting-python-keras/

# Good article about differing time sequence lengths:
# https://danijar.com/variable-sequence-lengths-in-tensorflow/

# Examining the shape
np.sum([np.prod(v.shape) for v in tf.trainable_variables()])

tvars = tf.trainable_variables()
tvars_vals = sess.run(tvars)

tvars_vals[0].shape # 'kernel' of lstm: [W V] where h_t = W*h_t-1 + V*x_t
sess.run('rnn/basic_lstm_cell/kernel:0')

#TODO: fit LSTM on part of sample and then let it predict out of sample on
# batch of different time interval


