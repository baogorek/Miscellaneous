library(lme4)
write.csv(sleepstudy, "/mnt/c/devl/data/sleepstudy.csv", row.names=F)

summary(lm(Reaction ~ Days, data = sleepstudy))

fm1 <- lmer(Reaction ~ 1 + Days + (1 + Days | Subject), sleepstudy)
summary(fm1)

vcov(fm1) # Fixed effects variance covariance matrix
VarCorr(fm1) # Random effects variance covariance matrix
x = VarCorr(fm1)
var(ranef(fm1)$Subject)

n_subjects <- length(table(sleepstudy$Subject))
subject_ids <- names(table(sleepstudy$Subject))

# New approach matching random effects notation

# guesses / starting values
#mu_bar <- 200
sigma_mu <- 10
#beta_bar <- 5
sigma_beta <- 3
sigma_e <- 20

mu_re <- rnorm(n_subjects, mean=0, sd=sigma_mu)
beta_re <- rnorm(n_subjects, mean=0, sd=sigma_beta)
# end guesses / starting values

X <- model.matrix(~ Days, data = sleepstudy)
Z <- model.matrix(~ 0 + Subject + Subject:Days, data = sleepstudy)
y <- sleepstudy$Reaction

get_pred <- function(mu_bar, beta_bar, mu_re, beta_re) {
  # X, Z are data set specific and in the global environment
  X %*% c(mu_bar, beta_bar) + Z %*% c(mu_re, beta_re)
}

# ERROR: you're mixing up the level of mu and beta with the random effects
# TODO: paper and pensil on this one
neg_log_posterior <- function(mu_bar, beta_bar, mu_re, beta_re) {
    # X, Z, y are data set specific and in the global environment
    ss_mu <- sum(mu_re ^ 2)  # Variance of the 
    ss_beta <- sum((beta_re - beta_bar) ^ 2)
    # Need to think about denomonators
    sse <- sum((get_pred(mu_bar, beta_bar, mu_re, beta_re) - y) ^ 2)
    neg_log_post <- sse + (sigma_e ^ 2 / sigma_mu ^ 2) * ss_mu +
      (sigma_e ^ 2 / sigma_beta ^ 2) * ss_beta
    sse 
}

# Problem here: bad optimization
my_optim <- optim(c(200, 10, mu_re, beta_re),
                  function(theta){
                    neg_log_posterior(theta[1], theta[2], theta[3:20], theta[21:38])
                  }, method = "BFGS",
                   control = list(maxit = 10000))
  
theta <- my_optim$par
print(theta)

neg_log_posterior(1, 4, mu_re, beta_re)

########################################################################
# First approach below, where random coefficients came out "fully baked"
# This works, though it is not as general as I would like it to be
# Step 1, get a prediction function with weights as the arguments

# starting values for random effects generating parameters
mu_bar <- 200
sigma_mu <- 10
beta_bar <- 5
sigma_beta <- 3
sigma_e <- 20

# Under this representation, there's not distinction
# between fixed and random effects

theta <- c(rnorm(n_subjects, mean=mu_bar, sd=sigma_mu), # intercepts
           rnorm(n_subjects, mean=beta_bar, sd=sigma_beta)) # slopes

f <- function(theta, days, subject) {
  subject_pos <- which(subject_ids == subject) 
  mu_i <- theta[subject_pos]
  beta_i <- theta[subject_pos + length(theta) / 2]
  mu_i + beta_i * days
}

f(theta, 5, 308)

get_sse <- function(theta) {
  resid <- sapply(1:nrow(sleepstudy),
             function(i) {
               sleepstudy$Reaction[i] - f(theta, sleepstudy$Days[i], sleepstudy$Subject[i])
            }
  )
  sum(resid ^ 2)
}
get_sse(theta)

for (iter in 1:10) {
  cat("Iteration ", iter, "---------------\n")
  neg_log_posterior <- function(theta) {
    mu <- theta[1:n_subjects]
    beta <- theta[(n_subjects + 1):(2 * n_subjects)]
    
    ss_mu <- sum((mu - mu_bar) ^ 2)
    ss_beta <- sum((beta - beta_bar) ^ 2)
    
    get_sse(theta) + (sigma_e ^ 2 / sigma_mu ^ 2) * ss_mu +
      (sigma_e ^ 2 / sigma_beta ^ 2) * ss_beta
  }
  
  my_optim <- optim(theta, neg_log_posterior, method = "BFGS",
                    control = list(maxit = 10000))
  
  theta <- my_optim$par
  print(theta)
  mu <- my_optim$par[1:n_subjects]
  beta <- my_optim$par[(n_subjects + 1):(2 * n_subjects)]
   
  mu_bar <- mean(mu) 
  sigma_mu <- sd(mu)
  beta_bar <- mean(beta) 
  sigma_beta <- sd(beta) 
  sigma_e <- sqrt(get_sse(my_optim$par)) / (n_subjects - 3) # is there a better effective degrees of freedom?
}

mu_bar
sigma_mu
beta_bar
sigma_beta
sigma_e
