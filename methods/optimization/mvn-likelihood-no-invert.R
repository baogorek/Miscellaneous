Sigma <- matrix(c(
   4, 10, 6,
   10, 26, 19,
   6, 19, 61
  ), ncol=3)  # A 3x3 covariance matrix
x <- c(1, -3, 7)  # Sampled value of x

# Naive approach to computing x^T * Sigma^(-1) * x
Sigma_inv <- solve(Sigma)
Sigma_det <- det(Sigma)

single_square <- t(x) %*% Sigma_inv %*% x

# The "don't invert that matrix" approach
L_root <- t(chol(Sigma))
L_root_inv <- backsolve(L_root, diag(3), upper.tri=FALSE)

L_root_inv_x <- L_root_inv %*% x

single_square2 <- t(L_root_inv_x) %*% L_root_inv_x
cat(single_square, single_square2, "\n")

# Now for the determinant
Sigma_det2 <- prod(diag(L_root)) ^ 2
cat(Sigma_det, Sigma_det2, "\n")

