from sleepstudy import SleepReg
import numpy as np

sleep_reg = SleepReg("/mnt/c/devl/data/sleepstudy.csv")
sleep_reg.train()
sleep_reg.set_optimizer(adam=True)
sleep_reg.train()

re = sleep_reg.get_random_effects()
sleep_reg.estimate_Sigma_b()
s = sleep_reg.estimate_sigmasq_epsilon()

