"""
Fit L0 regression model using the code at  
https://github.com/AMLab-Amsterdam/L0_regularization
To an actual sparse regression situation
(relies on l0_louizos_improved_gate.R to have run)
"""

import sys
from pathlib import Path

import pandas as pd
import numpy as np
import torch
import torch.nn as nn
import torch.nn.functional as F
import statsmodels.api as sm

# https://github.com/AMLab-Amsterdam/L0_regularization is not an installable package
sys.path.append('/home/baogorek/devl/L0_regularization')
from l0_layers import L0Dense


torch.manual_seed(42)

# The csv below is generated in l0_louizos_improved_gate.R 
df = pd.read_csv('/home/baogorek/devl/regr.csv')

X = df[['x1', 'x2', 'x3', 'x4']].values
y = df['y'].values

X_tensor = torch.FloatTensor(X)
y_tensor = torch.FloatTensor(y)

# Estimate variance for lambda scaling using statsmodels
X_with_const = sm.add_constant(X)
ols_model = sm.OLS(y, X_with_const).fit()
print(ols_model.summary())
sigma2_hat = ols_model.mse_resid
    
   
l0_lambda = 0.5 * sigma2_hat  # Increase penalty to achieve better sparsity
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
weights

bias = model.bias.data.item()
bias

# Let's match a prediction ------
bias + np.dot(X[0, :], weights)
predictions[0]

bias + np.dot(X[15, :], weights)
predictions[15]

