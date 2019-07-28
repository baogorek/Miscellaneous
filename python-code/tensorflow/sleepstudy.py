import pandas as pd
import numpy as np
import patsy

sleepstudy_df = pd.read_csv("/mnt/c/devl/data/sleepstudy.csv")
sleepstudy_df['SubjectId'] = pd.Categorical(sleepstudy_df['Subject']).codes
sleepstudy_df['SubjectId'] = sleepstudy_df.SubjectId.apply(lambda x: str(x))

X = patsy.dmatrix('1 + Days', data=sleepstudy_df)

Z_int = patsy.dmatrix('0 + SubjectId', data=sleepstudy_df)
Z_slope = patsy.dmatrix('0 + SubjectId:Days', data=sleepstudy_df)
Z_version1 = np.concatenate((Z_int, Z_slope), axis=1)

N = Z_int.shape[1] # number of subjects

P = np.zeros((2 * N, 2 * N)) # Column Permutation matrix
j = 0
for i in range(N):
    P[i, 2 * j] = 1.0 
    P[N + i, 2 * j + 1] = 1.0
    j += 1
Z = np.matmul(Z_version1, P) # Textbook Z matrix

# Sigma from lme4
#  Groups   Name        Std.Dev. Corr
#  Subject  (Intercept) 24.7404
#            Days         5.9221  0.066
#            Residual             25.5918

# Need global variables b, V, and sigma_epsilon
# First putting in the final values from lme4 so there's no need to iterate
b = np.reshape(np.random.normal(size=(N * 2)), (N * 2, 1))
Sigma = np.array([[24.7404 ** 2, .066 * 5.9221 * 24.7404],
                  [.066 * 24.7404 * 5.9221, 5.9221 ** 2]])
Sigma_inv = np.linalg.inv(Sigma)

sigma2_epsilon = 25.5918 ** 2 

V = np.kron(np.identity(N), Sigma_inv)

# Define the Hat matrix H such that y_hat = Hy
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4217225/#R17
Xt = np.transpose(X)
Zt = np.transpose(Z)
XtX = np.matmul(Xt, X)
XtZ = np.matmul(Xt, Z)
ZtX = np.transpose(XtZ)
ZtZ_plus_Sigma = np.matmul(Zt, Z) + V

XandZ = np.hstack((X, Z))
XtoverZt = np.vstack((Xt, Zt))
D = np.vstack((np.hstack((XtX, XtZ)), np.hstack((ZtX, ZtZ_plus_Sigma))))
H = np.matmul(np.matmul(XandZ, D), XtoverZt)
np.trace(H)  # TODO: Problem!
# Tensorflow time
import tensorflow as tf

# Define your coefficients by subclassing from tf.Module
class SleepReg(tf.Module):
    def __init__(self):
        self.beta = tf.Variable(tf.zeros([2, 1], dtype=tf.float64))
        self.b = tf.Variable(tf.zeros([36, 1], dtype=tf.float64))


@tf.function
def predict(X, Z, beta, b):
    return tf.matmul(X, beta) + tf.matmul(Z, b)


@tf.function
def get_sse(y, y_hat):
    return tf.reduce_sum(tf.square(y - y_hat))


@tf.function
def get_neglogprior(b, V):
    bTV = tf.matmul(tf.transpose(b), V)
    bTVb = tf.matmul(bTV, b)
    return tf.squeeze(bTVb)


b = tf.constant(b)
V = tf.constant(V) # No need to do the kronecker product in tensorflow

X = tf.constant(X)
Z = tf.constant(Z)
y = tf.constant(sleepstudy_df.Reaction.values.reshape((180, 1)))

sgd_optimizer = tf.optimizers.SGD(learning_rate=.05, momentum=.98)
#sgd_optimizer = tf.optimizers.Adam()

# Trick: run SGD with high momentum and then Adam
sleep_reg = SleepReg() # resets parameters to starting values

for epoch in range(1500):
    with tf.GradientTape() as gradient_tape:
        y_pred = predict(X, Z, sleep_reg.beta, sleep_reg.b) 
        loss = (get_sse(y, y_pred) / sigma2_epsilon
                + get_neglogprior(sleep_reg.b, V))

    gradient = gradient_tape.gradient(loss, (sleep_reg.trainable_variables[0],
                                             sleep_reg.trainable_variables[1]))
    sgd_optimizer.apply_gradients(zip(gradient,
                                      sleep_reg.trainable_variables))
    print(sleep_reg.beta)

b_hat = np.squeeze(sleep_reg.b.numpy())
evens = [i for i in range(2 * N) if np.mod(i, 2) == 0]
odds = [i for i in range(2 * N) if np.mod(i, 2) == 1]
mean_re = [b_hat[i] for i in evens] 
b_re = [b_hat[i] for i in odds]
re_vec = [np.array([b_hat[i], b_hat[j]]) for i, j in zip(evens, odds)] 
np.cov(re_vec, rowvar = False)
np.cov(mean_re, b_re)

# TODO: find a way to estimate this using bivariate vectors
