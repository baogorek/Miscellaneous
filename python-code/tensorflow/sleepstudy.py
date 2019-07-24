import tensorflow as tf
import pandas as pd
import numpy as np
import patsy

sleepstudy_df = pd.read_csv("/mnt/c/devl/data/sleepstudy.csv")
sleepstudy_df['SubjectId'] = pd.Categorical(sleepstudy_df['Subject']).codes
sleepstudy_df['SubjectId'] = sleepstudy_df.SubjectId.apply(lambda x: str(x))

X = patsy.dmatrix('1 + Days', data=sleepstudy_df)

Z_int = patsy.dmatrix('0 + SubjectId', data=sleepstudy_df)
Z_slope = patsy.dmatrix('0 + SubjectId:Days', data=sleepstudy_df)
Z = np.concatenate((Z_int, Z_slope), axis=1)

# Define your coefficients by subclassing from tf.Module
class SleepReg(tf.Module):
    def __init__(self):
        self.fixed_effects = tf.Variable(tf.zeros([2, 1], dtype=tf.float64))
        self.random_effects = tf.Variable(tf.zeros([36, 1], dtype=tf.float64))
sleep_reg = SleepReg()
sgd_optimizer = tf.optimizers.SGD(learning_rate=.01, momentum=.98)

@tf.function
def predict(X, Z, beta_bar, b):
    return tf.matmul(X, beta_bar) + tf.matmul(Z, b)

@tf.function
def get_loss(observed, predicted):
        return tf.reduce_mean(tf.square(observed - predicted))

sgd_optimizer = tf.optimizers.SGD(learning_rate=.01, momentum=.98)

X = tf.constant(X)
Z = tf.constant(Z)
y = tf.constant(sleepstudy_df.Reaction.values.reshape((180, 1)))

for epoch in range(1000):
    with tf.GradientTape() as gradient_tape:
        y_pred = predict(X, Z, sleep_reg.trainable_variables[0],
                         sleep_reg.trainable_variables[1])
        loss = get_loss(y, y_pred)

    gradient = gradient_tape.gradient(loss, (sleep_reg.trainable_variables[0],
                                             sleep_reg.trainable_variables[1]))
    sgd_optimizer.apply_gradients(zip(gradient,
                                      sleep_reg.trainable_variables))
    print(sleep_reg.trainable_variables[0])


