load_ext autoreload
autoreload 2

# Part 1. Demo of lmer matching
from sleepstudy import SleepReg
import numpy as np

sleep_reg = SleepReg("/mnt/c/devl/data/sleepstudy.csv")

# Replicate lme4's result
off_diag = 24.7404 * 5.9221 * 0.066
lmer_vcov = np.array([[24.7404 ** 2, off_diag],
                      [off_diag, 5.9221 ** 2]])

sleep_reg.reset_variances(lmer_vcov, 25.5918 ** 2)

sleep_reg.train()
sleep_reg.set_optimizer(adam=True)
sleep_reg.train(epochs=300)

# Part 2. Demo of lmer matching
sleep_reg.zero_coefficients()
sleep_reg.reset_variances(np.array([[410, 10], [10, 22]]),
                         .25 * np.var(sleep_reg.y))
sleep_reg.set_optimizer(adam=False)

for i in range(100):
    sleep_reg.train(display_beta=False)
    
    sigmasq_epsilon = sleep_reg.estimate_sigmasq_epsilon()
       
    V = sleep_reg.get_rnd_effs_variance()
    V_diag = np.diag(np.diag(V)) # comment to watch procedure fail

    sleep_reg.reset_variances(V_diag, sigmasq_epsilon)

    print(V_diag)
    print(sigmasq_epsilon)

#[[302.9045408    0.        ]
# [  0.          31.08902388]]
#[670.8546961]

re = sleep_reg.get_random_effects()
sleep_reg.get_rnd_effs_variance()
sleep_reg.estimate_sigmasq_epsilon()
