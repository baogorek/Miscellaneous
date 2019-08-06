import pandas as pd
import numpy as np
import patsy

import tensorflow as tf

class SleepReg(tf.Module):
    def __init__(self, sleepdata_path):
        """Initializing tensorflow variables and other necessary matrices"""
        # The two TensorFlow variables show up in the trainable_variables prop
        self.beta = tf.Variable(tf.zeros([2, 1], dtype=tf.float64))
        self.b = tf.Variable(tf.zeros([36, 1], dtype=tf.float64))

        self.set_optimizer() # Last Tensorflow-specific optimization

        self.data = self.get_sleepstudy_data(sleepdata_path)
        self.N_subjects = 18

        self.y = self.data.Reaction.values.reshape((180, 1))
        self.X = self.get_X_matrix()
        self.Z = self.get_Z_matrix()
 
        # Initializing Sigma_b and sigmasq_epsilon to lme4's output for demo
        self.Sigma_b = np.array([[24.7404 ** 2, .066 * 5.9221 * 24.7404],
                                [.066 * 24.7404 * 5.9221, 5.9221 ** 2]])
        self.sigmasq_epsilon = 25.5918 ** 2 
       
        self.V = self.get_V_matrix()
        self.H = self.get_H_matrix()
        self.df = self.get_approximate_df()

    def reset_variances(self, Sigma_b, sigmasq_epsilon):
        self.Sigma_b = Sigma_b
        self.sigmasq_epsilon = sigmasq_epsilon

        # Update matrices that depend on these variances 
        self.V = self.get_V_matrix()
        self.H = self.get_H_matrix()
        self.df = self.get_approximate_df()

    def get_sleepstudy_data(self, sleepdata_path):
        sleep_df = pd.read_csv(sleepdata_path)
        sleep_df['SubjectId'] = pd.Categorical(sleep_df['Subject']).codes
        sleep_df['SubjectId'] = sleep_df.SubjectId.apply(lambda x: str(x))
        return sleep_df

    def get_X_matrix(self):
        return patsy.dmatrix('1 + Days', data=self.data)

    def get_Z_matrix(self):
        """Gets textbook (block diagonal) random effects Z matrix."""
        Z_int = patsy.dmatrix('0 + SubjectId', data=self.data)
        Z_slope = patsy.dmatrix('0 + SubjectId:Days', data=self.data)
        Z_version1 = np.concatenate((Z_int, Z_slope), axis=1)
        
        # Prepare a column permutation matrix
        P = np.zeros((2 * self.N_subjects, 2 * self.N_subjects))
        j = 0
        for i in range(self.N_subjects):
            P[i, 2 * j] = 1.0 
            P[self.N_subjects + i, 2 * j + 1] = 1.0
            j += 1
        Z = np.matmul(Z_version1, P) # Textbook Z matrix
        return Z

    def get_V_matrix(self):
        """ """
        Sigma_b_inv = np.linalg.inv(self.Sigma_b)
        V = np.kron(np.identity(self.N_subjects), Sigma_b_inv)
        return V

    def get_H_matrix(self):
        """Define the Hat matrix H such that y_hat = Hy."""
        Xt = np.transpose(self.X)
        Zt = np.transpose(self.Z)
        XtX_s = np.matmul(Xt, self.X) / self.sigmasq_epsilon
        XtZ_s = np.matmul(Xt, self.Z) / self.sigmasq_epsilon
        ZtX_s = np.matmul(Zt, self.X) / self.sigmasq_epsilon
        ZtZ_s_plus_V = np.matmul(Zt, self.Z) / self.sigmasq_epsilon + self.V
        
        X_and_Z = np.hstack((self.X, self.Z))
        Xt_over_Zt_s = np.vstack((Xt, Zt)) / self.sigmasq_epsilon
        D = np.vstack((np.hstack((XtX_s, XtZ_s)),
                       np.hstack((ZtX_s, ZtZ_s_plus_V))))
        D_inv = np.linalg.inv(D)
        H = np.matmul(np.matmul(X_and_Z, D_inv), Xt_over_Zt_s)
        return H

    def get_approximate_df(self):
        return np.trace(self.H)

    def get_fitted_values(self):
        return np.matmul(self.H, self.y)

    def get_random_effects(self):
        b_hat = np.squeeze(self.b.numpy())
        rand_effs = np.reshape(b_hat, (self.N_subjects, 2))
        return pd.DataFrame(rand_effs, columns = ['mu', 'b'])

    def estimate_Sigma_b(self):
        rand_effs = self.get_random_effects()
        return np.cov(rand_effs, rowvar=False)

    def estimate_sigmasq_epsilon(self):
        y_hat = self.get_fitted_values()
        df = self.get_approximate_df()
        resid = self.y - y_hat
        est = np.matmul(resid.T, resid) / (resid.shape[0] - df)
        return est.ravel()

    @tf.function
    def _get_expectation(self, X, Z, beta, b):
        return tf.matmul(X, beta) + tf.matmul(Z, b)
    
    @tf.function
    def _get_sse(self, y, y_hat):
        return tf.reduce_sum(tf.square(y - y_hat))
    
    @tf.function
    def _get_neg_log_prior(self, b, V):
        bTV = tf.matmul(tf.transpose(b), V)
        bTVb = tf.matmul(bTV, b)
        return tf.squeeze(bTVb)

    def _get_neg_log_prior2(self, b, sigmasq_int, sigmasq_slope):
        """ Get sum of squares penalty from b as a tensorflow variable"""
        re_mat = np.reshape(tf.squeeze(b), (self.N_subjects, 2))

        int_penalty = tf.reduce_sum(tf.square(re_mat[:, 0])) / sigmasq_int
        slope_penalty = tf.reduce_sum(tf.square(re_mat[:, 1])) / sigmasq_slope

        return int_penalty + slope_penalty 

    def set_optimizer(self, adam=False):
        """Choose optimizer for the model training task."""
        self.optimizer = (tf.optimizers.Adam() if adam else
                          tf.optimizers.SGD(learning_rate=.025, momentum=.98))
        
    def train(self, epochs=1500, display_beta=True):
        """Trains model using a TensorFlow training loop""" 
        X = tf.constant(self.X)
        Z = tf.constant(self.Z)
        y = tf.constant(self.y)
        V = tf.constant(self.V)

        for epoch in range(epochs):
            with tf.GradientTape() as gradient_tape:
                y_pred = self._get_expectation(X, Z, self.beta, self.b) 
                #loss = (self._get_sse(y, y_pred) / self.sigmasq_epsilon
                #        + self._get_neg_log_prior(self.b, V))
                loss = (self._get_sse(y, y_pred) / self.sigmasq_epsilon
                        + self._get_neg_log_prior2(self.b, self.Sigma_b[0, 0],
                                                   self.Sigma_b[1, 1]))


            gradient = gradient_tape.gradient(loss, (
                (self.trainable_variables[0], self.trainable_variables[1])
            ))
            self.optimizer.apply_gradients(zip(gradient,
                                               self.trainable_variables))

            if display_beta: print(self.beta)


#sleep_reg = SleepReg("/mnt/c/devl/data/sleepstudy.csv")

#sleepstudy_df = pd.read_csv("/mnt/c/devl/data/sleepstudy.csv")
#sleepstudy_df['SubjectId'] = pd.Categorical(sleepstudy_df['Subject']).codes
#sleepstudy_df['SubjectId'] = sleepstudy_df.SubjectId.apply(lambda x: str(x))
#
#X = patsy.dmatrix('1 + Days', data=sleepstudy_df)

#Z_int = patsy.dmatrix('0 + SubjectId', data=sleepstudy_df)
#Z_slope = patsy.dmatrix('0 + SubjectId:Days', data=sleepstudy_df)
#Z_version1 = np.concatenate((Z_int, Z_slope), axis=1)
#
#N = Z_int.shape[1] # number of subjects
#
#P = np.zeros((2 * N, 2 * N)) # Column Permutation matrix
#j = 0
#for i in range(N):
#    P[i, 2 * j] = 1.0 
#    P[N + i, 2 * j + 1] = 1.0
#    j += 1
#Z = np.matmul(Z_version1, P) # Textbook Z matrix

# Sigma from lme4
#  Groups   Name        Std.Dev. Corr
#  Subject  (Intercept) 24.7404
#            Days         5.9221  0.066
#            Residual             25.5918

# Need global variables b, V, and sigma_epsilon
# First putting in the final values from lme4 so there's no need to iterate
#b = np.reshape(np.random.normal(size=(N * 2)), (N * 2, 1))
#Sigma = np.array([[24.7404 ** 2, .066 * 5.9221 * 24.7404],
#                  [.066 * 24.7404 * 5.9221, 5.9221 ** 2]])
#Sigma_inv = np.linalg.inv(Sigma)
#
#sigma2_epsilon = 25.5918 ** 2 
#
#V = np.kron(np.identity(N), Sigma_inv)

# Define the Hat matrix H such that y_hat = Hy
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4217225/#R17
#Xt = np.transpose(X)
#Zt = np.transpose(Z)
#XtX_s = np.matmul(Xt, X) / sigma2_epsilon
#XtZ_s = np.matmul(Xt, Z) / sigma2_epsilon
#ZtX_s = np.matmul(Zt, X) / sigma2_epsilon
#ZtZ_s_plus_Sigma = np.matmul(Zt, Z) / sigma2_epsilon + V
#
#X_and_Z = np.hstack((X, Z))
#Xt_over_Zt_s = np.vstack((Xt, Zt)) / sigma2_epsilon
#D = np.vstack((np.hstack((XtX_s, XtZ_s)),
#               np.hstack((ZtX_s, ZtZ_s_plus_Sigma))))
#D_inv = np.linalg.inv(D)
#H = np.matmul(np.matmul(X_and_Z, D_inv), Xt_over_Zt_s)
#np.trace(H) 
# Tensorflow time

# Define your coefficients by subclassing from tf.Module
#class SleepReg(tf.Module):
#    def __init__(self):
#        self.beta = tf.Variable(tf.zeros([2, 1], dtype=tf.float64))
#        self.b = tf.Variable(tf.zeros([36, 1], dtype=tf.float64))


#@tf.function
#def predict(X, Z, beta, b):
#    return tf.matmul(X, beta) + tf.matmul(Z, b)
#
#
#@tf.function
#def get_sse(y, y_hat):
#    return tf.reduce_sum(tf.square(y - y_hat))
#
#
#@tf.function
#def get_neglogprior(b, V):
#    bTV = tf.matmul(tf.transpose(b), V)
#    bTVb = tf.matmul(bTV, b)
#    return tf.squeeze(bTVb)
#
#
##b = tf.constant(b)
#V = tf.constant(V) # No need to do the kronecker product in tensorflow
#
#X = tf.constant(X)
#Z = tf.constant(Z)
#y = tf.constant(sleepstudy_df.Reaction.values.reshape((180, 1)))
#
#sgd_optimizer = tf.optimizers.SGD(learning_rate=.05, momentum=.98)
##sgd_optimizer = tf.optimizers.Adam()
#
## Trick: run SGD with high momentum and then Adam
#sleep_reg = SleepReg() # resets parameters to starting values
#
#for epoch in range(1500):
#    with tf.GradientTape() as gradient_tape:
#        y_pred = predict(X, Z, sleep_reg.beta, sleep_reg.b) 
#        loss = (get_sse(y, y_pred) / sigmasq_epsilon
#                + get_neglogprior(sleep_reg.b, V))
#
#    gradient = gradient_tape.gradient(loss, (sleep_reg.trainable_variables[0],
#                                             sleep_reg.trainable_variables[1]))
#    sgd_optimizer.apply_gradients(zip(gradient,
#                                      sleep_reg.trainable_variables))
#    print(sleep_reg.beta)
#
#b_hat = np.squeeze(sleep_reg.b.numpy())
#evens = [i for i in range(2 * N) if np.mod(i, 2) == 0]
#odds = [i for i in range(2 * N) if np.mod(i, 2) == 1]
#mean_re = [b_hat[i] for i in evens] 
#b_re = [b_hat[i] for i in odds]
#re_vec = [np.array([b_hat[i], b_hat[j]]) for i, j in zip(evens, odds)] 
#np.cov(re_vec, rowvar = False)
#np.cov(mean_re, b_re)
