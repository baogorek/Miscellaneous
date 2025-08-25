"""
Fit L0 regression model using the code at  
https://github.com/AMLab-Amsterdam/L0_regularization
To an actual sparse regression situation
"""

import sys
from pathlib import Path

import pandas as pd
import numpy as np
import torch
import torch.nn as nn
import torch.nn.functional as F
import statsmodels.api as sm
from scipy.stats import multivariate_normal

# https://github.com/AMLab-Amsterdam/L0_regularization is not an installable package
sys.path.append('/home/baogorek/devl/L0_regularization')
from l0_layers import L0Dense


torch.manual_seed(42)
np.random.seed(12543)  # Match seed from l0_louizos_improved_gate.py

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

X = df[['x1', 'x2', 'x3', 'x4']].values
y = df['y'].values

X_tensor = torch.FloatTensor(X)
y_tensor = torch.FloatTensor(y)

# Estimate variance for lambda scaling using statsmodels
X_with_const = sm.add_constant(X)
ols_model = sm.OLS(y, X_with_const).fit()
print(ols_model.summary())
sigma2_hat = ols_model.mse_resid
    

# 0.28 will push it to 1, 0.27 pushes it to 3
l0_lambda = 0.27 * sigma2_hat  # Increase penalty to achieve better sparsity
learning_rate=0.01
epochs=5000  # Match R implementation

model = L0Dense(
    in_features=4,
    out_features=1,
    bias=True,
    weight_decay=0.0,
    droprate_init=0.5,
    temperature=0.6666666666666666,
)
 
optimizer = torch.optim.Adam(model.parameters(), lr=learning_rate)

for epoch in range(epochs):
    predictions = model.forward(X_tensor).squeeze()

    data_loss = F.mse_loss(predictions, y_tensor)
    penalty = model.regularization()  # Note: it is negative, for some reason
    
    loss = data_loss - l0_lambda * penalty 

    optimizer.zero_grad()
    loss.backward()
    optimizer.step()
    
        
# analyze results ---------

model.eval()

torch.set_grad_enabled(False)


predictions = model.forward(X_tensor).squeeze()

theta_tilde = model.weights.squeeze().numpy()
z = model.sample_z(1, sample=False).squeeze().numpy()

weights = theta_tilde * z
print(f"Final weights: {weights}")

bias = model.bias.data.item()
print(f"Final bias: {bias}")

# Let's match a prediction ------
bias + np.dot(X[0, :], weights)
predictions[0]

bias + np.dot(X[15, :], weights)
predictions[15]

