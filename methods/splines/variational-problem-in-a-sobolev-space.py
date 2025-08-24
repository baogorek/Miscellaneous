# Minimal, fast version: two plots and core derivations only
import numpy as np
import matplotlib
matplotlib.use("TkAgg")
import matplotlib.pyplot as plt

rng = np.random.default_rng(0)

def second_difference_matrix(n: int) -> np.ndarray:
    D2 = np.zeros((n-2, n))
    for i in range(n-2):
        D2[i, i] = 1.0
        D2[i, i+1] = -2.0
        D2[i, i+2] = 1.0
    return D2

# what looks like a humble “smoothing spline” in statistics textbooks
# is literally the Sobolev variational problem written in concrete form.
def smoothing_spline_discrete(y: np.ndarray, lam: float) -> tuple[np.ndarray, np.ndarray]:
    n = len(y)
    h = 1.0 / (n - 1)
    D2 = second_difference_matrix(n)
    P = (n / (h**3)) * (D2.T @ D2)
    A = np.eye(n) + lam * P
    f_hat = np.linalg.solve(A, y)
    return f_hat, P

def f0(x):
    return np.sin(4*np.pi*x) + 0.5*np.cos(2*np.pi*x)

# Data
n = 200
x = np.linspace(0, 1, n)
true_f = f0(x)
y = true_f + 0.3 * rng.standard_normal(n)

# Solve for two λ values
lam_small, lam_large = 1e-6, 1e-3
f_small, P = smoothing_spline_discrete(y, lam_small)
f_large, _ = smoothing_spline_discrete(y, lam_large)

# Plot: light smoothing
plt.figure()
plt.scatter(x, y, s=10, alpha=0.5, label="noisy data")
plt.plot(x, true_f, linewidth=2, label="true f")
plt.plot(x, f_small, linewidth=2, label=f"estimate (λ={lam_small:.1e})")
plt.title("Discrete smoothing spline — light smoothing")
plt.xlabel("x")
plt.ylabel("f(x)")
plt.legend()
plt.show()

# Plot: strong smoothing
plt.figure()
plt.scatter(x, y, s=10, alpha=0.5, label="noisy data")
plt.plot(x, true_f, linewidth=2, label="true f")
plt.plot(x, f_large, linewidth=2, label=f"estimate (λ={lam_large:.1e})")
plt.title("Discrete smoothing spline — strong smoothing")
plt.xlabel("x")
plt.ylabel("f(x)")
plt.legend()
plt.show()

# Verify optimality residuals and Sobolev seminorms
res_small = np.linalg.norm((np.eye(n) + lam_small * P) @ f_small - y)
res_large = np.linalg.norm((np.eye(n) + lam_large * P) @ f_large - y)

seminorm_small = float(f_small.T @ P @ f_small) / n
seminorm_large = float(f_large.T @ P @ f_large) / n

print("KKT/optimality residuals ||(I + λP)f - y||_2 (≈0):")
print(f"  λ={lam_small:.1e} -> {res_small:.3e}")
print(f"  λ={lam_large:.1e} -> {res_large:.3e}")

print("\nDiscrete H^2 seminorms of estimates (smaller = smoother):")
print(f"  λ={lam_small:.1e} -> {seminorm_small:.3e}")
print(f"  λ={lam_large:.1e} -> {seminorm_large:.3e}")

# Show 'functional' aspect by refining resolution (parameter count grows with n)
n_fine = 400
x_fine = np.linspace(0, 1, n_fine)
y_fine = f0(x_fine) + 0.3 * rng.standard_normal(n_fine)
f_fine, P_fine = smoothing_spline_discrete(y_fine, lam_small)

print("\nFunctional viewpoint demo:")
print(f"  Coarse grid parameter count n={n}, fine grid parameter count n={n_fine}.")
print("  Both solve the same *functional* variational problem at different resolutions.")


