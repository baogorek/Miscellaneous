load_ext autoreload
autoreload 2

# Part 1. Demo of lmer matching
from sleepstudy import SleepReg
import numpy as np

sleep_reg = SleepReg("/mnt/c/devl/data/sleepstudy.csv")

sleep_reg.train(mackay=True)

off_diag = 24.7404 * 5.9221 * 0.066
sleep_reg.reset_variances(np.array([[24.7404 ** 2, off_diag],
                                    [off_diag, 5.9221 ** 2]]),
                          25.5918 ** 2)

sleep_reg.train(mackay=True)

sleep_reg.reset_variances(np.array([[410, 10], [10, 22]]),
                         .25 * np.var(sleep_reg.y))

off_diag = 0.0 
sleep_reg.reset_variances(np.array([[24.7404 ** 2, off_diag],
                                    [off_diag, 5.9221 ** 2]]),
                          25.5918 ** 2)

off_diag = 0.0 
sleep_reg.reset_variances(np.array([[44.7404 ** 2, off_diag],
                                    [off_diag, 9.9221 ** 2]]),
                          35.5918 ** 2)

for i in range(100):
    sleep_reg.train(display_beta=False, mackay=False)
    
    Sigma_b = sleep_reg.estimate_Sigma_b()
    sigmasq_epsilon = sleep_reg.estimate_sigmasq_epsilon()
   
    Sigma_b[0, 1] = 0.0
    Sigma_b[1, 0] = 0.0
    sleep_reg.reset_variances(Sigma_b, sigmasq_epsilon)

    print(sigmasq_epsilon)
    print(Sigma_b)

#[670.8524944]
#[[302.92110182   0.        ]
#[  0.          31.08864589]]
#sleep_reg.set_optimizer(adam=True)
#sleep_reg.train()

re = sleep_reg.get_random_effects()
sleep_reg.estimate_Sigma_b()
s = sleep_reg.estimate_sigmasq_epsilon()


