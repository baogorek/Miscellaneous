# https://www.youtube.com/watch?v=9n-xMt-Sj3A

set.seed(4659)

alpha_0 <- -40
alpha_p <- 30
beta_0 <- 180
beta_p <- -25

P <- seq(0, 7, by=.1) 

# Deterministic equations
S = alpha_0 + alpha_p * P
D = beta_0 + beta_p * P

plot(S ~ P, type="l", col="blue", xlab="Price", ylab="Quantity")
lines(D ~ P, type="l", col="red")

# analytical equilibrium, S=D
abline(h=80, lty=2) # Q
abline(v=4, lty=2) # P

# Deterministic equations with Demand Shock of + 1
demand_shock <- 25
D2 = beta_0 + beta_p * P + demand_shock

lines(D2 ~ P, type="l", col="red")

new_price <- (beta_0 - alpha_0 + demand_shock) / (alpha_p - beta_p)
new_quantity <- alpha_0 + alpha_p * new_price
abline(h=new_quantity, lty=2, col='darkgreen') # Q
abline(v=new_price, lty=2, col='darkgreen') # P


# Show the effect of a price increase of +1 in the deterministic equations
# With random supply and demand shocks, no equilibrium condition imposed
N <- 1E6
P <- 7 * runif(N)

sigma_S <- 10
sigma_D <- 10

S = alpha_0 + alpha_p * P + sigma_S * rnorm(N)
D = beta_0 + beta_p * P + sigma_D * rnorm(N)

plot(S ~ P, col="blue", ylab="Quantity", xlab="Price")
points(D ~ P, col="red")

# The only thing I wasn't really thinking of is that this isn't a pair, it's 2 pairs

df <- data.frame(P = P, S = S, D = D)
df_close <- df[abs(df$S - df$D) < .75, ]
nrow(df_close)

plot(S ~ P, data=df_close, ylab="Quantity", xlab="Price")


# The reduced form, theoretical quantities -------
E_P <- (beta_0 - alpha_0) / (alpha_p - beta_p)
E_Q <- (alpha_p * beta_0 - alpha_0 * beta_p) / (alpha_p - beta_p)
V_P <- (sigma_S ^ 2 + sigma_D ^ 2) / (alpha_p - beta_p) ^ 2
V_Q <- (beta_p ^2 * sigma_S ^ 2 + alpha_p ^ 2 * sigma_D ^ 2) / (alpha_p - beta_p) ^ 2
Cov_PQ <- (beta_p * sigma_S ^ 2 + alpha_p * sigma_D ^ 2) / (alpha_p - beta_p) ^ 2

cat("Raw data. E(P), Expected:", E_P, "Estimated:", mean(df$P), "\n") 
cat("Subset. E(P), Expected:", E_P, "Estimated:", mean(df_close$P), "\n") 
cat("Raw data. E(Q), Expected:", E_Q, "Estimated:", mean(df$S), "\n")
cat("Subset. E(Q), Expected:", E_Q, "Estimated:", mean(df_close$S), "\n") 
cat("Raw data. Var(P), Expected:", V_P, "Estimated:", var(df$P), "\n")
cat("Subset. Var(P), Expected:", V_P, "Estimated:", var(df_close$P), "\n") 
cat("Raw data. Var(Q), Expected:", V_Q, "Estimated:", var(df$S), "\n") 
cat("Subset. Var(Q), Expected:", V_Q, "Estimated:", var(df_close$S), "\n") 
cat("Raw data. Cov(P, Q), Expected:", Cov_PQ, "Estimated:",
    cov(df$P, df$S), "\n")
cat("Subset. Cov(P, Q), Expected:", Cov_PQ, "Estimated:",
    cov(df_close$P, df_close$S), "\n")
