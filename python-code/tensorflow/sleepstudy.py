import tensorflow as tf
import pandas as pd
import numpy as np
import patsy

sleepstudy_df = pd.read_csv("/mnt/c/devl/data/sleepstudy.csv")
sleepstudy_df['Subject'] = pd.Categorical(sleepstudy_df['Subject'], ordered=False) 
sleepstudy_df['one'] = 1.
ones = sleepstudy_df.one 
y = sleepstudy_df.Reaction

# I'd like to do a regression with Gradient tape

X_np = patsy.dmatrix('1 + Days', data=sleepstudy_df) #, return_type='dataframe')
X_tf = tf.constant(X_np, dtype=tf.float32)
y_tf = tf.constant(y.values.reshape((180, 1)), dtype=tf.float32)
beta = tf.Variable(tf.zeros([2., 1.]), dtype=tf.float32)
beta.assign([[200], [8]])

@tf.function
def get_loss(predicted_y, desired_y):
    return tf.reduce_mean(tf.square(predicted_y - desired_y))

@tf.function
def predict(X_input, beta):
    return tf.matmul(X_input, beta)

for epoch in range(100):

    with tf.GradientTape() as tape:
        y_pred = predict(X_tf, beta) 
        loss = get_loss(y_pred, y_tf)
    
    grads = tape.gradient(loss, beta)
    
    beta.assign_sub(.01 * grads)

    print('loss is: {}'.format(loss.numpy()))
    print('beta0: {}, beta1: {}'.format(beta.numpy()[0], beta.numpy()[1]))


# THIS IS THE ONE!! 
https://www.tensorflow.org/beta/guide/variables

class LinReg(tf.Module):
    def __init__(self):
        self.beta = tf.Variable(tf.zeros([2., 1.]), dtype=tf.float32)

linreg = LinReg()
sgd_optimizer = tf.optimizers.SGD(.01)

for epoch in range(100):

    with tf.GradientTape() as tape:
        y_pred = predict(X_tf, linreg.trainable_variables) 
        loss = get_loss(y_pred, y_tf)
    
    grads = tape.gradient(loss, linreg.trainable_variables)
    sgd_optimizer.apply_gradients(zip(grads, linreg.trainable_variables))

    print('loss is: {}'.format(loss.numpy()))
    print('beta0: {}, beta1: {}'.format(beta.numpy()[0], beta.numpy()[1]))


# Custom Training: basics
https://www.tensorflow.org/beta/tutorials/eager/custom_training
# tf.keras is a neural networks API, strongly recommended for nets
import tensorflow as tf
# Tensors in tf are immutable stateless objects
x = tf.zeros([10, 10]) # 10 x 10 tensor
# But Python is stateful. Using python state:
x += 2
print(x)

# Tensorflow has its own stateful operations built in,  like tf variables.
# A tf Variable is an object which stores a value , implicitly reads it
v = tf.Variable(1.0)
v.assign(3.0)

v.assign(tf.square(v))

# synthesize regression data
TRUE_W = 3.0
TRUE_b = 2.0

NUM_EXAMPLES = 100

inputs  = tf.random.normal(shape=[NUM_EXAMPLES])
noise   = tf.random.normal(shape=[NUM_EXAMPLES])
outputs = inputs * TRUE_W + TRUE_b + noise

def loss(predicted_y, desired_y):
    return tf.reduce_mean(tf.square(predicted_y - desired_y))

W = tf.Variable(5.0)
b = tf.Variable(0.0)

pred = W * inputs + b
print(loss(pred, outputs))


for epoch in range(1000):
    with tf.GradientTape() as tape:
        current_loss = loss(W * inputs + b, outputs)
    dW, db = tape.gradient(current_loss, [W, b])
    W.assign_sub(.01 * dW)
    b.assign_sub(.01 * db)
    print('W: {}, b: {}'.format(W.numpy(), b.numpy()))


@tf.function
def custom_loss(obs, pred):
    return 2 * tf.metrics.mse(obs, pred)


# Simple Linear Regression
one_inputs = tf.keras.Input(shape=(1,))
x_inputs = tf.keras.Input(shape=(1,))

concat_layer = tf.keras.layers.concatenate([one_inputs, x_inputs])

out_layer = tf.keras.layers.Dense(1, use_bias=False)(concat_layer)

model = tf.keras.Model(inputs=[one_inputs, x_inputs], outputs=out_layer,
                       name = 'slr')

my_sgd = SGD(lr = .01, decay = .01, momentum = .9)
model.compile(optimizer=my_sgd, loss = custom_loss, metrics=['mse'])

model.fit(x=[ones, sleepstudy_df.Days], y=y, epochs=1000)

model.get_weights()

# Now do a Regression with one beta for every subject
my_one_hot = tf.one_hot(sleepstudy_df.Subject, depth=18)

Z = patsy.dmatrix('0 + Subject', data=sleepstudy_df, return_type='dataframe')
Z_slope = patsy.dmatrix('0 + Subject:Days', data=sleepstudy_df,
                        return_type='dataframe')
Z_slope = patsy.dmatrix('0 + Subject + Subject:Days', data=sleepstudy_df,
                        return_type='dataframe')
