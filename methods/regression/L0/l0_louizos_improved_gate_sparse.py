# LEARNING SPARSE NEURAL NETWORKS THROUGH L0 REGULARIZATION - SPARSE VERSION
# https://arxiv.org/pdf/1712.01312
# 
# Extended to handle sparse X matrices with variable p

import numpy as np
import torch
from scipy import sparse
from scipy.stats import norm
from sklearn.linear_model import LinearRegression
import sys

torch.manual_seed(12543)
np.random.seed(12543)

# Data generating process with sparse X -------
n = 1000
p = 100  # Now variable, can be much larger
X_sparsity = 0.95  # 95% of X entries are zero
beta_sparsity = 0.95  # 95% of true coefficients are zero

b0 = 30

# Create sparse true coefficients
num_nonzero = int(p * (1 - beta_sparsity))
nonzero_indices = np.random.choice(p, num_nonzero, replace=False)
b = np.zeros(p)
b[nonzero_indices] = np.random.choice([-3, -2, -1, 1, 2, 3], num_nonzero)

print(f"True non-zero coefficients: {dict(zip(nonzero_indices, b[nonzero_indices]))}")

# Generate sparse X matrix
# First create as dense for simulation purposes, then convert to sparse
X_dense = np.random.randn(n, p) * 0.5  # smaller variance
mask = np.random.random((n, p)) < X_sparsity
X_dense[mask] = 0

# Add some correlation structure to non-zero entries
for i in range(0, p-1, 2):
    if i+1 < p:
        corr_mask = ~(mask[:, i] | mask[:, i+1])
        X_dense[corr_mask, i+1] += 0.3 * X_dense[corr_mask, i]

# Convert to sparse format
X_sparse = sparse.csr_matrix(X_dense)

# Memory comparison
dense_memory = X_dense.nbytes
sparse_memory = X_sparse.data.nbytes + X_sparse.indices.nbytes + X_sparse.indptr.nbytes
print(f"\nMemory usage comparison:")
print(f"Dense matrix: {dense_memory / 1024:.2f} KB")
print(f"Sparse matrix: {sparse_memory / 1024:.2f} KB")
print(f"Compression ratio: {dense_memory / sparse_memory:.2f}x")
print(f"Sparsity: {1 - X_sparse.nnz / (n * p):.2%}")

# Generate y
sigma_e = 1.5
y_np = b0 + X_sparse @ b + sigma_e * np.random.randn(n)

# Quick check with linear regression (on sparse matrix)
lr = LinearRegression(fit_intercept=True).fit(X_sparse, y_np)
print(f"\nLinear regression found {np.sum(np.abs(lr.coef_) > 0.01)} non-zero coefficients")
print(f"Top 10 coefficients by magnitude (indexes): {np.argsort(np.abs(lr.coef_))[-10:]}")

# Onto the L0 part -----------------------

def sample_z(log_alpha, beta, zeta, gamma):
    p = log_alpha.numel()
    eps = 1e-6
    u = torch.rand(p).clamp(eps, 1 - eps)
    X = (torch.log(u) - torch.log(1 - u) + log_alpha) / beta
    s = torch.sigmoid(X)
    s_bar = s * (zeta - gamma) + gamma
    z = s_bar.clamp(0, 1)
    return z

def complexity_loss(log_alpha, beta, zeta, gamma):
    c = -beta * torch.log(torch.tensor(-gamma / zeta))
    pi = torch.sigmoid(log_alpha + c)
    return pi.sum()

def init_log_alpha(keep_prob, size, loc_sd=0.01):
    mu = np.log(keep_prob / (1 - keep_prob))
    init_vals = np.random.normal(loc=mu, scale=loc_sd, size=size)
    log_alpha = torch.tensor(init_vals, dtype=torch.float32, requires_grad=True)
    return log_alpha

# Convert sparse matrix to torch sparse tensor
X_coo = X_sparse.tocoo()
indices = torch.LongTensor(np.vstack([X_coo.row, X_coo.col]))
values = torch.FloatTensor(X_coo.data)
X_torch_sparse = torch.sparse_coo_tensor(indices, values, X_sparse.shape, dtype=torch.float32)

y = torch.tensor(y_np, dtype=torch.float32)

# Initialize parameters
b0_parm = torch.tensor(y_np.mean(), dtype=torch.float32, requires_grad=True)
b_parm = torch.tensor(0.1 * np.random.randn(p), dtype=torch.float32, requires_grad=True)

log_alpha = init_log_alpha(keep_prob=0.5, size=p)  # Higher initial keep prob
beta = 2 / 3
gamma = -0.1
zeta = 1.1

# Getting a good value for lambda
residuals = y_np - lr.predict(X_sparse)
sigma2_hat = np.var(residuals)
lambda_reg = 0.01 * sigma2_hat  # Much smaller lambda for sparse data

epochs = 5000
opt = torch.optim.Adam([b0_parm, b_parm, log_alpha], lr=1e-2)

print(f"\nStarting L0 optimization with {p} features...")
for k in range(1, epochs + 1):
    z = sample_z(log_alpha, beta, zeta, gamma)
    b_star = b_parm * z

    y_hat = b0_parm + torch.sparse.mm(X_torch_sparse, b_star.unsqueeze(1)).squeeze(1)

    data_loss = (y - y_hat).pow(2).mean()
    comp = complexity_loss(log_alpha, beta, zeta, gamma)
    loss = data_loss + lambda_reg * comp
    
    opt.zero_grad()
    loss.backward()
    opt.step()
    
    if k % 500 == 0:
        with torch.no_grad():
            c = -beta * torch.log(torch.tensor(-gamma / zeta))
            pi = torch.sigmoid(log_alpha + c)
            active_gates = (z == 1).sum().item()
            print(f"epoch {k:4d}  loss={loss.item():.4f}  comp={comp.item():.2f}  active_gates={active_gates}")

# Final results
with torch.no_grad():
    alpha_final = torch.exp(log_alpha)
    c = -beta * torch.log(torch.tensor(-gamma / zeta))
    pi_final = torch.sigmoid(log_alpha + c)
    z_final = ((log_alpha / beta).sigmoid() * (zeta - gamma) + gamma).clamp(0, 1)
    b_parm_final = b_parm * z_final
    
    # Find selected features
    selected = torch.where(z_final > 0.01)[0].numpy()
    
    print(f"\n=== Final Results ===")
    print(f"Selected {len(selected)} features out of {p}")
    print(f"True non-zero coefficients: {nonzero_indices}")
    print(f"Selected features: {selected}")
    
    # Check overlap
    true_positives = len(set(selected) & set(nonzero_indices))
    false_positives = len(set(selected) - set(nonzero_indices))
    false_negatives = len(set(nonzero_indices) - set(selected))
    
    print(f"\nPerformance:")
    print(f"True positives: {true_positives}/{len(nonzero_indices)}")
    print(f"False positives: {false_positives}")
    print(f"False negatives: {false_negatives}")
    
    if len(selected) > 0:
        precision = true_positives / len(selected)
        recall = true_positives / len(nonzero_indices) if len(nonzero_indices) > 0 else 0
        print(f"Precision: {precision:.2%}")
        print(f"Recall: {recall:.2%}")
    
    # Show coefficients for selected features
    if len(selected) <= 20:
        print(f"\nSelected coefficients:")
        for idx in selected:
            true_val = b[idx]
            est_val = b_parm_final[idx].item()
            print(f"  x{idx}: true={true_val:.3f}, estimated={est_val:.3f}")
