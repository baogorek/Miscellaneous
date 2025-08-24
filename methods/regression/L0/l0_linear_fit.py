"""
Fit L0Linear regression model to simulated data using the L0 package.
"""

from pathlib import Path

import pandas as pd
import torch
import torch.nn as nn
import torch.nn.functional as F
import statsmodels.api as sm

from l0.layers import L0Linear


torch.manual_seed(42)

# Create this file from l0.R 
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
l2_lambda=0.0
learning_rate=0.01
epochs=5000  # Match R implementation

model = L0Linear(
    in_features=4,
    out_features=1,
    bias=True,
    init_sparsity=0.5,  # Match R's keep_prob=0.5
    temperature=2/3.0,
    use_l2=False
)
 
optimizer = torch.optim.Adam(model.parameters(), lr=learning_rate)

for epoch in range(epochs):
    predictions = model(X_tensor).squeeze()
    data_loss = F.mse_loss(predictions, y_tensor)
    l0_penalty = model.get_l0_penalty()
    l2_penalty = model.get_l2_penalty()
    
    loss = data_loss + l0_lambda * l0_penalty + l2_lambda * l2_penalty

    optimizer.zero_grad()
    loss.backward()
    optimizer.step()
    
    if (epoch + 1) % 200 == 0:
        sparsity = model.get_sparsity()
        gates = model.weight_gates().detach()
        weights = model.weight.data.squeeze()
        effective_coefs = weights * gates.squeeze()
        
        print(f"Epoch {epoch+1}/{epochs}")
        print(f"  Loss: {loss.item():.4f}, Data: {data_loss.item():.4f}, L0: {l0_penalty.item():.4f}")
        print(f"  Gates: [{gates[0,0].item():.3f}, {gates[0,1].item():.3f}, {gates[0,2].item():.3f}, {gates[0,3].item():.3f}]")
        print(f"  Coefs: [{effective_coefs[0].item():.3f}, {effective_coefs[1].item():.3f}, {effective_coefs[2].item():.3f}, {effective_coefs[3].item():.3f}]")
        print(f"  Sparsity: {sparsity:.4f}")
        print()

# analyze results --------
model.eval()

torch.set_grad_enabled(False)

predictions = model(X_tensor).squeeze()
mse = F.mse_loss(predictions, y_tensor)

weight = model.weight.data.squeeze()
bias = model.bias.data.item()

gates = model.weight_gates()

sparsity = model.get_sparsity()

print("=" * 50)
print("FINAL RESULTS")
print("=" * 50)
print(f"Final MSE: {mse.item():.4f}")
print(f"Final Sparsity: {sparsity:.4f}")
print()

print("Coefficients (weight * gate):")
print(f"  Intercept (bias): {bias:.4f}")
for i in range(len(weight)):
    effective_weight = weight[i].item() * gates[0, i].item()
    print(f"  x{i+1}: {effective_weight:.4f} (weight={weight[i].item():.4f}, gate={gates[0, i].item():.4f})")

print()
print("Gate probabilities (closer to 1 = more active, closer to 0 = pruned):")
for i in range(len(weight)):
    print(f"  x{i+1}: {gates[0, i].item():.4f}")

# From the R simulation, the true coefficients are:
# b1 = 1, b2 = 0, b3 = -2, b4 = 0
print()
print("True coefficients (from simulation):")
print("  x1: 1.0")
print("  x2: 0.0")
print("  x3: -2.0")
print("  x4: 0.0")
