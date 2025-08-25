# LEARNING SPARSE NEURAL NETWORKS THROUGH L0 REGULARIZATION
# https://arxiv.org/pdf/1712.01312
# 
# CHANGE But we do leave the temperature in the final gate,
# which is essential for good performance here

import numpy as np
import pandas as pd
import torch
from scipy.stats import multivariate_normal
from sklearn.linear_model import LinearRegression

torch.manual_seed(12543)
np.random.seed(12543)

# Data generating process -------
n = 500

b0 = 30
b1 = 1
b2 = 0
b3 = -2
b4 = 0

b = np.array([b1, b2, b3, b4])
p = len(b)

rho = 0.5

sigma_X = np.full((p, p), rho)
np.fill_diagonal(sigma_X, 1)
sigma_e = 1.5

raw_data = multivariate_normal.rvs(mean=np.zeros(p), cov=sigma_X, size=n)
df = pd.DataFrame(raw_data, columns=[f'x{i+1}' for i in range(p)])

df['y'] = b0 + df['x1'] * b1 + df['x2'] * b2 + df['x3'] * b3 + df['x4'] * b4 + sigma_e * np.random.randn(n)

# Quick check with linear regression
X_np = df[['x1', 'x2', 'x3', 'x4']].values
y_np = df['y'].values
lr = LinearRegression().fit(X_np, y_np)
print(f"Linear regression coefficients: {lr.coef_}")
print(f"Linear regression intercept: {lr.intercept_}")

# Onto the L0 part -----------------------

def sample_z(log_alpha, beta, zeta, gamma):
    # Generating the data
    p = log_alpha.numel()
    eps = 1e-6
    u = torch.rand(p).clamp(eps, 1 - eps)
    # Below draws from the logistic distributions with mean log(alpha) / beta
    # X ~ Logistic(mu, s) can be represented as X = mu + s * (log (U) - log(1 - U)), U ~ Uniform
    X = (torch.log(u) - torch.log(1 - u) + log_alpha) / beta
    # Side note, the difference of two Gumbels is Logistic distributed
    # Ordinary concrete always lives within 0, 1, allows for the reparameterization trick
    s = torch.sigmoid(X)
    # Hard concrete stretches range outside 0, 1, then clamps to produce point masses
    s_bar = s * (zeta - gamma) + gamma
    z = s_bar.clamp(0, 1)
    return z

def complexity_loss(log_alpha, beta, zeta, gamma):
    # I think we should really only need alpha
    c = -beta * torch.log(torch.tensor(-gamma / zeta))  # scalar
    pi = torch.sigmoid(log_alpha + c)
    return pi.sum()

def init_log_alpha(keep_prob, size, loc_sd=0.01):
    # Convert keep-probability (p) into the corresponding logit.
    # This is the mean mu around which we initialize log_alpha.
    # logit(p) = log(p / (1 - p))
    mu = np.log(keep_prob / (1 - keep_prob))
    
    # Now add a tiny bit of Gaussian noise around mu.
    # This breaks symmetry so not all gates start with the exact same probability.
    # stddev = 0.01 as in the paper.
    init_vals = np.random.normal(loc=mu, scale=loc_sd, size=size)
    
    # Convert to torch tensor and mark as learnable (requires_grad = TRUE)
    log_alpha = torch.tensor(init_vals, dtype=torch.float32, requires_grad=True)
    
    return log_alpha

# Notes on the model:
# The expected number of active gates is:
# Pr(Z > 0) = Sigmoid(log(alpha) - beta * log(-gamma / zeta))
# We use that in the lambda initialization above to set alpha given a probability

# Initialize

b0_parm = torch.tensor(df['y'].mean(), dtype=torch.float32, requires_grad=True)
b_parm = torch.tensor(0.1 * np.random.rand(p), dtype=torch.float32, requires_grad=True)

# The only trainable L0 parameter is alpha, essentially the logit bias to keep the gate on
log_alpha = init_log_alpha(keep_prob=0.5, size=p)
# Architectural constants that shape distribution to approximate a discrete on/off switch after clipping
beta = 2 / 3
gamma = -0.1
zeta = 1.1

# Getting a good value for lambda
residuals = y_np - lr.predict(X_np)
sigma2_hat = np.var(residuals)
lambda_reg = 0.1 * sigma2_hat  # 0.15 will work, 0.05 will work

# Bringing the data into torch form
x_cols = [f'x{i+1}' for i in range(p)]
X = torch.tensor(df[x_cols].values, dtype=torch.float32)
y = torch.tensor(df['y'].values, dtype=torch.float32)

epochs = 5000

opt = torch.optim.Adam([b0_parm, b_parm, log_alpha], lr=1e-2)  # tune lr

for k in range(1, epochs + 1):
    # one MC sample per forward pass is exactly what the paper does
    z = sample_z(log_alpha, beta, zeta, gamma)
    b_star = b_parm * z
    
    y_hat = b0_parm + X @ b_star
    data_loss = (y - y_hat).pow(2).mean()
    comp = complexity_loss(log_alpha, beta, zeta, gamma)
    loss = data_loss + lambda_reg * comp
    
    opt.zero_grad()
    loss.backward()
    opt.step()
    
    if k % 50 == 0:
        with torch.no_grad():
            c = -beta * torch.log(torch.tensor(-gamma / zeta))
            pi = torch.sigmoid(log_alpha + c)  # P(gate ON)
            pi_str = ', '.join([f'{p:.2f}' for p in pi.numpy()])
            print(f"epoch {k:4d}  loss={loss.item():.4f}  comp={comp.item():.2f}  pi={pi_str}")

with torch.no_grad():
    # For reference (not strictly needed for gating):
    alpha_final = torch.exp(log_alpha)
    
    # Probability a gate is ON (good for reporting/pruning)
    c = -beta * torch.log(torch.tensor(-gamma / zeta))
    pi_final = torch.sigmoid(log_alpha + c)
    
    # Deterministic gate (u = 0.5) through hard-concrete stretch + clamp
    # (Recall this is the logistic without the log(u) + log(1 - u), u ~ Uniform(0, 1) and temperature beta
    # The final gate is simply the pointwise expectation of the stretched sigmoid, clipped to [0, 1]
    # At test time: there's no relaxation — you just take a deterministic gate from log(alpha)
    # so beta (temperature) disappears. There's no softening left to do the regularize random samples
    z_final = ((log_alpha / beta).sigmoid() * (zeta - gamma) + gamma).clamp(0, 1)
    
    # Gated coefficients as tensors (don't coerce until printing)
    b_parm_final = b_parm * z_final
    
    # --- Pretty printing (convert to R numerics only here) ---
    print("alpha:        ", ', '.join([f'{a:.4f}' for a in alpha_final.numpy()]))
    print("pi (P>0):     ", ', '.join([f'{p:.4f}' for p in pi_final.numpy()]))
    print("z_final:      ", ', '.join([f'{z:.4f}' for z in z_final.numpy()]))
    print("b_parm_final: ", ', '.join([f'{b:.4f}' for b in b_parm_final.numpy()]))
