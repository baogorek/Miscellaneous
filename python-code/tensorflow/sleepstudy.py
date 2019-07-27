import tensorflow as tf
import pandas as p
import numpy as np
import patsy

sleepstudy_df = pd.read_csv("/mnt/c/devl/data/sleepstudy.csv")
sleepstudy_df['SubjectId'] = pd.Categorical(sleepstudy_df['Subject']).codes
sleepstudy_df['SubjectId'] = sleepstudy_df.SubjectId.apply(lambda x: str(x))

X = patsy.dmatrix('1 + Days', data=sleepstudy_df)

Z_int = patsy.dmatrix('0 + SubjectId', data=sleepstudy_df)
Z_slope = patsy.dmatrix('0 + SubjectId:Days', data=sleepstudy_df)
Z = np.concatenate((Z_int, Z_slope), axis=1)

N = Z_int.shape[1] # number of subjects

P = np.zeros((2 * N, 2 * N)) # Column Permutation matrix
j = 0
for i in range(N):
    P[i, 2 * j] = 1.0 
    P[N + i, 2 * j + 1] = 1.0
    j += 1
Z2 = np.matmul(Z, P) # Textbook Z matrix

# Define your coefficients by subclassing from tf.Module
class SleepReg(tf.Module):
    def __init__(self):
        self.beta = tf.Variable(tf.zeros([2, 1], dtype=tf.float64))
        self.b = tf.Variable(tf.zeros([36, 1], dtype=tf.float64))
sleep_reg = SleepReg()
sgd_optimizer = tf.optimizers.SGD(learning_rate=.01, momentum=.98)

@tf.function
def predict(X, Z, beta, b):
    return tf.matmul(X, beta) + tf.matmul(Z, b)

# Sigma from lme4
#            (Intercept)      Days
#(Intercept)   46.574562 -1.451096
#Days          -1.451096  2.389463
b = np.reshape(np.random.normal(size=(N * 2)), (N * 2, 1))
Sigma = np.array([[46.574562, -1.451096], [-1.451096, 2.389463]])
Sigma_inv = np.linalg.inv(Sigma)

V = np.kron(np.identity(N), Sigma_inv)
bTV = np.matmul(np.transpose(b), V)
bTVb = np.matmul(bTV, b)

b = tf.constant(b)
V = tf.constant(V) # No need to do the kronecker product in tensorflow

@tf.function
def get_loss2(b, V):
    bTV = tf.matmul(tf.transpose(b), V)
    bTVb = tf.matmul(bTV, b)
    return tf.squeeze(bTVb)

@tf.function
def get_loss(y, y_hat):
        return tf.reduce_mean(tf.square(y - y_hat))

sgd_optimizer = tf.optimizers.SGD(learning_rate=.01, momentum=.98)

X = tf.constant(X)
Z = tf.constant(Z2) # TODO: fix Z2
y = tf.constant(sleepstudy_df.Reaction.values.reshape((180, 1)))

for epoch in range(1000):
    with tf.GradientTape() as gradient_tape:
        y_pred = predict(X, Z, sleep_reg.beta, sleep_reg.b) 
        loss = get_loss(y, y_pred) + get_loss2(sleep_reg.b, V)

    gradient = gradient_tape.gradient(loss, (sleep_reg.trainable_variables[0],
                                             sleep_reg.trainable_variables[1]))
    sgd_optimizer.apply_gradients(zip(gradient,
                                      sleep_reg.trainable_variables))
    print(sleep_reg.beta)
re = np.squeeze(sleep_reg.b.numpy())
mean_re = [re[i] for i in range(2 * N) if np.mod(i, 2) == 0]
b_re = [re[i] for i in range(2 * N) if np.mod(i, 2) == 1]

np.std(mean_re)
np.std(b_re)
# TODO: find a way to estimate this using bivariate vectors
