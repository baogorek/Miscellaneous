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

    def zero_coefficients(self):
        self.beta = tf.Variable(tf.zeros([2, 1], dtype=tf.float64))
        self.b = tf.Variable(tf.zeros([36, 1], dtype=tf.float64))

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

    def _get_mackay_penalty(self, b, sigmasq_int, sigmasq_slope):
        """ Get sum of squares penalty from b as a tensorflow variable"""
        re_mat = np.reshape(tf.squeeze(b), (self.N_subjects, 2))

        int_penalty = tf.reduce_sum(tf.square(re_mat[:, 0])) / sigmasq_int
        slope_penalty = tf.reduce_sum(tf.square(re_mat[:, 1])) / sigmasq_slope

        return int_penalty + slope_penalty 

    def set_optimizer(self, adam=False):
        """Choose optimizer for the model training task."""
        self.optimizer = (tf.optimizers.Adam() if adam else
                          tf.optimizers.SGD(learning_rate=.025, momentum=.98))
        
    def train(self, epochs=1500, display_beta=True, mackay=False):
        """Trains model using a TensorFlow training loop""" 
        X = tf.constant(self.X)
        Z = tf.constant(self.Z)
        y = tf.constant(self.y)
        V = tf.constant(self.V)

        for epoch in range(epochs):
            with tf.GradientTape() as gradient_tape:
                y_pred = self._get_expectation(X, Z, self.beta, self.b) 
                if mackay:
                    loss = (self._get_sse(y, y_pred) / self.sigmasq_epsilon
                            + self._get_mackay_penalty(self.b,
                                                       self.Sigma_b[0, 0],
                                                       self.Sigma_b[1, 1]))
                else:
                    loss = (self._get_sse(y, y_pred) / self.sigmasq_epsilon
                            + self._get_neg_log_prior(self.b, V))

            gradient = gradient_tape.gradient(loss, (
                (self.trainable_variables[0], self.trainable_variables[1])
            ))
            self.optimizer.apply_gradients(zip(gradient,
                                               self.trainable_variables))

            if display_beta: print(self.beta)


