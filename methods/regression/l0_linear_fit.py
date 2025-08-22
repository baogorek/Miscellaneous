"""
Fit L0Linear regression model to simulated data using the L0 package.
"""

from pathlib import Path

import pandas as pd
import torch
import torch.nn as nn
import torch.nn.functional as F
from sklearn.linear_model import LinearRegression

from l0.layers import L0Linear


torch.manual_seed(42)


df = pd.read_csv('/home/baogorek/devl/regr.csv')

X = df[['x1', 'x2', 'x3', 'x4']].values
y = df['y'].values

X_tensor = torch.FloatTensor(X)
y_tensor = torch.FloatTensor(y)

# Estimate variance for lambda scaling
lr = LinearRegression()
lr.fit(X, y)
residuals = y - lr.predict(X)
sigma2_hat = residuals.var()
    
    
model = L0Linear(
    in_features=4,
    out_features=1,
    bias=True,
    init_sparsity=0.5,
    temperature=2/3,
    use_l2=False
)
    
    
l0_lambda = 0.1 * sigma2_hat / X.shape[0]  # Normalize by sample size (Claude: really?)

# Train model -------

l0_lambda=0.01
l2_lambda=0.0001, 
learning_rate=0.01
epochs=2000


optimizer = torch.optim.Adam(model.parameters(), lr=learning_rate)

for epoch in range(epochs):
    predictions = model(X_tensor).squeeze()
    data_loss = F.mse_loss(predictions, y_tensor)
    l0_penalty = model.get_l0_penalty()
    l2_penalty = model.get_l2_penalty()
    
    loss = data_loss + l0_lambda * l0_penalty # + l2_lambda * l2_penalty # Claude: something is wrong here  TypeError: only integer tensors of a single element can be converted to an index

    optimizer.zero_grad()
    loss.backward()
    optimizer.step()
    
    if (epoch + 1) % 100 == 0:
        sparsity = model.get_sparsity()
        print(f"Epoch {epoch+1}/{epochs}")
        print(f"  Loss: {loss.item():.4f}")
        print(f"  Data Loss: {data_loss.item():.4f}")
        print(f"  L0 Penalty: {l0_penalty.item():.4f}")
        print(f"  L2 Penalty: {l2_penalty.item():.4f}")
        print(f"  Sparsity: {sparsity:.4f}")
        print()

# analyze results --------
model.eval()

torch.set_grad_enabled(False)

predictions = model(X).squeeze()

# Calculate final MSE
mse = F.mse_loss(predictions, y)

# Get the weight parameters
weight = model.weight.data.squeeze()
bias = model.bias.data.item()

# Get gate values (probability of being active)
gates = model.weight_gates()

# Get sparsity
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


def main():
    """Main training script."""
    
    # Set random seed for reproducibility
    torch.manual_seed(42)
    
    # Load data
    X, y, df = load_data()
    print(f"Loaded data: {X.shape[0]} samples, {X.shape[1]} features")
    print()
    
    # Initialize model directly using L0Linear
    # Using parameters similar to the R implementation
    model = L0Linear(
        in_features=4,
        out_features=1,  # single output for regression
        bias=True,
        init_sparsity=0.5,  # Start with 50% sparsity expectation
        temperature=2/3,    # Beta parameter from R code
        use_l2=False        # Pure L0 for now
    )
    
    # Estimate variance for lambda scaling (similar to R code)
    # Quick linear regression to estimate noise variance
    from sklearn.linear_model import LinearRegression
    lr = LinearRegression()
    lr.fit(X.numpy(), y.numpy())
    residuals = y.numpy() - lr.predict(X.numpy())
    sigma2_hat = residuals.var()
    
    # Set L0 lambda based on noise variance (similar to R code)
    l0_lambda = 0.1 * sigma2_hat / X.shape[0]  # Normalize by sample size
    
    print(f"Estimated noise variance: {sigma2_hat:.4f}")
    print(f"L0 lambda: {l0_lambda:.6f}")
    print()
    
    # Train model
    print("Training L0 Linear Regression Model...")
    print("=" * 50)
    model = train_model(
        model, 
        X, 
        y,
        l0_lambda=l0_lambda,
        l2_lambda=0.0,  # No L2 for now
        learning_rate=0.01,
        epochs=2000
    )
    
    # Analyze results
    analyze_results(model, X, y)


if __name__ == "__main__":
    main()
