library(lavaan)

set.seed(12345)

options(width=300)

# Set the parameters and sample size
lambda1 <- .6
lambda2 <- .4
lambda3 <- .7

sigma_e1 <- sqrt(.1)
sigma_e2 <- sqrt(.2)
sigma_e3 <- sqrt(.3)

N <- 10000

# Generate the data
L <- rnorm(N, mean=0, sd=1)
e_1 <- rnorm(N, mean=0, sd=sigma_e1)
e_2 <- rnorm(N, mean=0, sd=sigma_e2)
e_3 <- rnorm(N, mean=0, sd=sigma_e3)

y1 <- lambda1 * L + e_1
y2 <- lambda2 * L + e_2
y3 <- lambda3 * L + e_3

df <- data.frame(y1=y1, y2=y2, y3=y3)

# Now recover the parameters with lavaan

# We can tell levaan to use latent variable variance = 1 constraint w syntax below
model_string <- 
' L =~ NA*y1 + y1 + y2 + y3 

  y1 ~ 1
  y2 ~ 1
  y3 ~ 1
'
my_cfa <- cfa(model_string, data = df, std.lv=TRUE)

summary(my_cfa)
# The following should closely match our parameters (depending on N)
coef(my_cfa) # Note that the intercepts are in there

# get the factor scores from Lavaan
df$L_hat <- as.numeric(predict(my_cfa))

# Now apply the covariance formula to the factor scores
coefs <- as.numeric(coef(my_cfa))

lambda1_hat <- coefs[1]
lambda2_hat <- coefs[2]
lambda3_hat <- coefs[3]

mu1_hat <- coefs[4]
mu2_hat <- coefs[5]
mu3_hat <- coefs[6]

sigma_e1_hat <- sqrt(coefs[7])
sigma_e2_hat <- sqrt(coefs[8])
sigma_e3_hat <- sqrt(coefs[9])

E_cov_partial_hat <- matrix(
  c(lambda1_hat^2 + sigma_e1_hat^2, lambda1_hat * lambda2_hat, lambda1_hat * lambda3_hat,
    lambda1_hat * lambda2_hat, lambda2_hat^2 + sigma_e2_hat^2, lambda2_hat * lambda3_hat,
    lambda1_hat * lambda3_hat, lambda2_hat * lambda3_hat, lambda3_hat^2 + sigma_e3_hat^2),
  nrow=3, ncol=3, byrow=TRUE
)

lambda_vec_hat <- matrix(c(lambda1_hat, lambda2_hat, lambda3_hat), nrow=1)

# "Hat matrix" for creating L hat from observables
hat_matrix <- lambda_vec_hat %*% solve(E_cov_partial_hat)

# Using the interecepts estimated from the model, center the manifest variables
df$y1_centered <- df$y1 - mu1_hat
df$y2_centered <- df$y2 - mu2_hat
df$y3_centered <- df$y3 - mu3_hat

# Now do it for the whole data set for the centered variables
df$L_hat2 <- as.matrix(df[, c("y1_centered", "y2_centered", "y3_centered")]) %*% t(hat_matrix)

cat("Observe for 3 observations that we have matched successfully\n")
df[1:3, c("L_hat", "L_hat2")]
