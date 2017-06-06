# This exercise is based on the paper by Neal,
#"Sampling Methods For Dirichlet Process Mixture Models" #
# Generating data from a DP using the Blackwell and MacQueen scheme #

# Let G_0 be N(0,3^3)
mu.0 <- 0
sigma.0 <- 3

alpha <- 2
N <- 1000

theta <- rep(0, times = N)

for(i in 1:N){
  cat("probability of base: ", alpha / (i - 1 + alpha) , "\n")
  
  if(runif(1) < alpha/(i - 1 + alpha)) { 
    theta[i] <- rnorm(1, mean = mu.0, sd = sigma.0)
    cat("Current Index: ", i,
        " \n Previous Index: NA - Using G0 to generate another atom: ",
        theta[i], " \n" )
  } else {
    previous.index <- sample(1:(i - 1), 1)
    cat("Current Index: ", i,  " \n Previous Index: ",
        previous.index, "with value: ", theta[previous.index], "\n" )
    theta[i] <- theta[previous.index]
  }
}

hist(theta, breaks = N)

### Making the DP continuous with the Mixtures.

y <- rep(0, times = N)

for(i in 1:N){ 
    y[i] <- rnorm(1, mean = theta[i], sd = 1) # Simple distribution for conjug.
}

hist(y)

#########################################################
theta.true <- theta
rm(list = setdiff(ls(), c("theta.true", "y", "N", "mu.0", "sigma.0", "alpha")) )

# Initialize theta, prepare for Gibbs Sampling
theta <- rep(mean(y), times = N) # all in one cluster
plot(theta)

for(MCMC.rep in 1:1000){
  for(i in 1:N){
  
    q.ij.unscaled <- sapply(theta, FUN = function(x) dnorm(y[i], mean = x, sd = 1) )
    r.i.unscaled <- alpha * dnorm(y[i], mean = 0, sd = sqrt(1 + sigma.0 ^ 2))
    
    b <- sum(q.ij.unscaled) - q.ij.unscaled[i] + r.i.unscaled
   
    if(runif(1) < r.i.unscaled / b) { 
      
      mu.post <- (sigma.0 ^ 2 * y[i]) / (1 + sigma.0 ^ 2)
      sigma.post <- sqrt((sigma.0 ^ 2) / (1 + sigma.0 ^ 2))
      
      theta[i] <- rnorm(1, mean = mu.post, sd = sigma.post)
      
    } else {
      c <- b - r.i.unscaled
      q.i <- q.ij.unscaled / c 
      q.i[i] <- 0
      
      previous.index <- which.max(rmultinom(n = 1, size = 1, prob = q.i))
      theta[i] <- theta[previous.index]
    }
  }
cat("MCMC Burn-in Rep:", MCMC.rep, "\n")
}
 
### How did we do?
plot(sort(theta.true) ~ sort(theta))

