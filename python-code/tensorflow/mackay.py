from sleepstudy import SleepReg
import numpy as np

sleep_reg = SleepReg("/mnt/c/devl/data/sleepstudy.csv")
sleep_reg.reset_variances(np.array([[410, 10], [10, 22]]),
                         .25 * np.var(sleep_reg.y))

for i in range(5):
    sleep_reg.train(display_beta=False)
    
    Sigma_b = sleep_reg.estimate_Sigma_b()
    sigmasq_epsilon = sleep_reg.estimate_sigmasq_epsilon()
    
    sleep_reg.reset_variances(Sigma_b, sigmasq_epsilon)
    print(sigmasq_epsilon)
    print(Sigma_b)

#sleep_reg.set_optimizer(adam=True)
#sleep_reg.train()

re = sleep_reg.get_random_effects()
sleep_reg.estimate_Sigma_b()
s = sleep_reg.estimate_sigmasq_epsilon()


