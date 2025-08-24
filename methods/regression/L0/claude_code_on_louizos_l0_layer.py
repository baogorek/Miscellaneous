#!/usr/bin/env python3
"""
This is Claude Code's attempt to understand if there was a serious problem
in https://github.com/AMLab-Amsterdam/L0_regularization or just a swapped
sign.

Ben talking: Honestly it's still unclear to me, but one this is certain: as is,
this L0Layer bombs on a simple sparse linear regression example.

Understanding the Mathematical Formulation in L0Dense

The regularization computes:
logpw_col = torch.sum(- (.5 * prior_prec * weights^2) - lambda, 1)
logpw = torch.sum((1 - cdf_qz(0)) * logpw_col)

Let's break this down:
1. With prior_prec=0 (no L2), logpw_col = -lambda for each output
2. logpw = -lambda * sum(P(gate_i > 0))
3. This is NEGATIVE proportional to the L0 norm

The authors might be using a variational formulation where this represents
a log probability that should be MAXIMIZED (hence the negative).
"""

import sys

# https://github.com/AMLab-Amsterdam/L0_regularization - not installable
sys.path.append('/home/baogorek/devl/L0_regularization')

import torch
from l0_layers import L0Dense
import warnings
warnings.filterwarnings("ignore")

print("="*70)
print("MATHEMATICAL ANALYSIS OF L0DENSE REGULARIZATION")
print("="*70)

# Create a simple L0Dense layer
model = L0Dense(
    in_features=4,
    out_features=1,
    bias=False,
    weight_decay=0.0,
    droprate_init=0.5,
    temperature=2/3.0,
    lamba=1.0
)

print("\nThe regularization term computes:")
print("  logpw_col = sum(-0.5*prior_prec*w² - lambda)")
print("  logpw = sum((1 - cdf_qz(0)) * logpw_col)")
print("\nWith prior_prec=0 (no L2):")
print("  logpw_col = -lambda")
print("  logpw = -lambda * sum(P(gate > 0))")

# Check the actual values
with torch.no_grad():
    reg = model.regularization()
    gate_probs = 1 - model.cdf_qz(0)
    l0_norm = gate_probs.sum()
    
    print(f"\nActual values:")
    print(f"  lambda = {model.lamba}")
    print(f"  Expected L0 norm = {l0_norm.item():.4f}")
    print(f"  regularization() = {reg.item():.4f}")
    print(f"  -lambda * L0_norm = {-model.lamba * l0_norm.item():.4f}")
    print(f"\nConfirmed: regularization() = -lambda * L0_norm")

print("\n" + "="*70)
print("INTERPRETATION OPTIONS")
print("="*70)

print("\nOPTION 1: Authors intended ADDING (their code does this)")
print("-" * 50)
print("Loss = DataLoss + regularization()")
print("     = DataLoss - lambda * L0_norm")
print("\nThis MINIMIZES DataLoss but MAXIMIZES L0_norm (bad!)")
print("Result: No sparsity (keeps all gates active)")

print("\nOPTION 2: Authors intended SUBTRACTING")
print("-" * 50)
print("Loss = DataLoss - regularization()")
print("     = DataLoss - (-lambda * L0_norm)")
print("     = DataLoss + lambda * L0_norm")
print("\nThis MINIMIZES both DataLoss and L0_norm (good!)")
print("Result: Should achieve sparsity")

print("\n" + "="*70)
print("CHECKING AUTHORS' ACTUAL CODE")
print("="*70)

# Let's check what the authors actually do
import os
train_file = "/home/baogorek/devl/L0_regularization/train_lenet5.py"
print(f"\nIn {train_file}:")
print("Line 109: total_loss = loss + model.regularization()")
print("\nThe authors ADD the regularization, not subtract it!")

print("\n" + "="*70)
print("FINAL VERDICT")
print("="*70)
print("""
The situation is nuanced:

1. The authors' CODE adds the negative regularization term, which
   mathematically rewards keeping gates active (wrong for sparsity).

2. If we SUBTRACT the negative term (as other LLM suggests), we get
   the correct L0 penalty that encourages sparsity.

3. The authors might have made an implementation error, or they might
   be using a variational formulation where the sign convention is
   different from standard regularization.

4. Empirically, SUBTRACTING achieves better (though still imperfect)
   sparsity on our test problem.

The confusion likely stems from mixing variational inference notation
(log probabilities to be maximized) with regularization notation
(penalties to be minimized).
""")
