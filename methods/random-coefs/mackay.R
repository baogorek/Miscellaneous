library(lme4)
#write.csv(sleepstudy, "/mnt/c/devl/data/sleepstudy.csv", row.names=F)

summary(lm(Reaction ~ Days, data = sleepstudy))

fm1 <- lmer(Reaction ~ 1 + Days + (1 + Days | Subject), sleepstudy)
summary(fm1)
# Figuring out why the variance of the random effects estimates is smaller
# than the estimated variance
#https://stats.stackexchange.com/questions/68106/understanding-the-variance-of-random-effects-in-lmer-models

vcov(fm1) # Fixed effects variance covariance matrix
x = VarCorr(fm1) # Random effects variance covariance matrix
x$Subject # look at the first matrix
var_re_hat <- var(ranef(fm1)$Subject)

# In the arm package code for computing standard errors for res
se.bygroup <- ranef(fm1, condVar=TRUE)
n.groupings <- length(se.bygroup)

vars.m <- attr(se.bygroup[[1]], "postVar") # where condVar=True is needed

# all subject's random effects variances are the same in this case,
# so the mean is just any individual variance matrix

mean_V <- vars.m[,,1]

var_re_hat + mean_V

## Start of Mackay's implementation in R
library(lme4)
n_subjects <- length(table(sleepstudy$Subject))
subject_ids <- names(table(sleepstudy$Subject))

X <- model.matrix(~ Days, data = sleepstudy)
Z <- model.matrix(~ 0 + Subject + Subject:Days, data = sleepstudy)
y <- sleepstudy$Reaction

get_pred <- function(mu_bar, beta_bar, mu_re, beta_re) {
  # X, Z are data set specific and in the global environment
  X %*% c(mu_bar, beta_bar) + Z %*% c(mu_re, beta_re)
}

neg_log_posterior <- function(mu_bar, beta_bar, mu_re, beta_re) {
    # X, Z, y are data set specific and in the global environment
    sse <- sum((get_pred(mu_bar, beta_bar, mu_re, beta_re) - y) ^ 2)
    (sse
      + (sigma_e ^ 2 / sigma_mu ^ 2) * sum(mu_re ^ 2)
      + (sigma_e ^ 2 / sigma_beta ^ 2) * sum(beta_re ^ 2)
    )
}

# Starting values
mu_re <- rep(0, n_subjects) # rnorm(n_subjects, mean=0, sd=sigma_mu)
beta_re <- rep(0, n_subjects) #rnorm(n_subjects, mean=0, sd=sigma_beta)
theta_starting <- c(200, 10, mu_re, beta_re)

# guesses / starting values
sigma_mu <- 10
sigma_beta <- 3
sigma_e <- 20

# Manually loop below
for(rep in 1:100) {
  my_optim <- optim(theta_starting,
                    function(theta){
                      neg_log_posterior(theta[1], theta[2], theta[3:20], theta[21:38])
                    }, method = "BFGS",
                     control = list(maxit = 10000))
    
  theta <- my_optim$par
  sigma_mu <- sd(theta[3:20])
  sigma_beta <- sd(theta[21:38])
  resid <- get_pred(theta[1], theta[2], theta[3:20], theta[21:38]) - y
  sigma_e <- sqrt(sum(resid ^ 2) / (length(resid) - 3.8))
  
  cat('sigma_mu: ', sigma_mu, ', sigma_beta: ', sigma_beta,
      'sigma_e: ', sigma_e, '\n')
}
theta

########################################################################
# First approach below, where random coefficients came out "fully baked"
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
