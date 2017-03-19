library(terminalR)

NormalPosterior<- function(prior.mean, prior.sigma, data.mean, data.sigma) {
  post.mean <- (prior.sigma^2 / (prior.sigma^2 + data.sigma^2)) * prior.mean + 
               (data.sigma^2 / (prior.sigma^2 + data.sigma^2)) * data.mean
  post.var <- (prior.sigma^2 * data.sigma^2) / (prior.sigma^2 + data.sigma^2)
  return(list(mean = post.mean, var = post.var))
}



# prior (hard-coded) parameter
alpha <- .3
G0.mean <- 0
G0.var <- 1

# The data
n <- 500
x <- c(rnorm(n/2, -1), rnorm(n/2, 1))

# starting value for theta
theta <- rep(0, n)
reps <- 10000
results <- data.frame()
for (j in 1:reps) { 
  for (i in 1:n) {
    q <- dnorm(x[i], theta[-i]) # before normalization
    r <- alpha * dnorm(x[i], G0.mean, sqrt(G0.var))
    b <- 1 / (sum(q) + r)
    
    q <- b * q
    r <- b * r
    #sum(q) + r # if 1.0, time to rock and roll
    
    # Pick a value from the mixture
    ordered <- cumsum(c(q, r))
    u <- runif(1)
    component <- which.max(u <= ordered)
    len <- length(c(q, r))
    if (component < len) {
      theta[i] <- theta[-i][component] # Reusing one of the existing theta values
    } else {
     dist <- NormalPosterior(G0.mean, sqrt(G0.var), x[i], 1) # Assuming sigma = 1 is known
     theta[i] <- rnorm(1, dist$mean, sqrt(dist$var)) # Choosing a new value of theta
    }
  }
  cat("rep", j, "\n")
  print(table(theta))
  results <- rbind(results, data.frame(matrix(theta, nrow = 1)))
}
