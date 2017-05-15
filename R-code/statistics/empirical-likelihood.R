# TODO: Needs documentation

n = 30
mu = 3
x <- rnorm(n, mu, 1.5)

ecdf <- function(t){ (1/n) * sum((x<=t)) }

plot(sapply(seq(0,6, by = .1), FUN="ecdf")~ seq(0,6, by=.1), type = "s")


lambda.left <-  (1 - 1/n) / (mu - max(x))
lambda.right <- (1 - 1/n) /(mu - min(x))

test <- function(lambda){
     y = (x-mu)/(1 + lambda*(x-mu))
     mean(y)**2
}

my.optim <- optim(par = .1, fn = test, method = "Brent", lower = lambda.left, upper = lambda.right)

lambda <- my.optim$par

ProbFromLambda <- function(x){ (1/n) * (1 + lambda*(x - mu))**(-1) }
weights <- ProbFromLambda(x)

ecdf2 <- function(t){ sum((x <= t)*weights)}

plot(sapply(seq(0,6, by = .1), FUN="ecdf")~ seq(0,6, by=.1), type = "s")
lines(sapply(seq(0,6, by = .1), FUN="ecdf2")~ seq(0,6, by=.1), type = "s", col = "blue")

chi_square_crit = qchisq(.95, 1)
r0 = exp(-.5*chi_square_crit)
r0

candidates = seq(min(x)+.1, max(x) - .1, .01)
results <- data.frame(mu.0 = candidates, value = numeric(length(candidates)))

rm(mu)
for(i in 1:length(candidates)){
  mu = candidates[i]

  lambda.left <-  (1 - 1/n) / (mu - max(x))
  lambda.right <- (1 - 1/n) /(mu - min(x))

  my.optim <- optim(par = .1, fn = test, method = "Brent", lower = lambda.left, upper = lambda.right)

  lambda <- my.optim$par
  weights <- ProbFromLambda(x)

  results[i, "value"] <- exp(sum(log(n*weights)))  

}

lower.df <- results[results$mu.0 < mean(x), ]
upper.df <- results[results$mu.0 > mean(x), ]

lower.cl <- lower.df[ which.min(abs(lower.df[,"value"]-r0)) , "mu.0"]
upper.cl <- upper.df[ which.min(abs(upper.df[,"value"]-r0)) , "mu.0"]


plot(value ~ mu.0, data = results, type = "b", 
    main = "Empirical likelihood-based confidence interval for a mean",
    xlab = expression(mu), ylab = "empirical likelihood ratio")

abline(h = r0, v=c(upper.cl, lower.cl), lty = 2)

text(x = lower.cl - .2, y = r0 + .1, round(lower.cl,1))
text(x = upper.cl + .2, y = r0 + .1, round(upper.cl,1))
