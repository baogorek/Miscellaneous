# topic - subsets of words that tend to co-occur within documents
rm(list = ls())
N.docs = 3

vocab = c("the", "dog", "sleeps", "cat", "wakes", "tree", "grows", "well")
V = length(vocab)

draw.topic <-function(eta){
  # Random draw from Dirichlet with parameter vector eta
  gamma.rv <- rgamma(V, shape = eta)
  gamma.rv / sum(gamma.rv)
}
draw.topic(eta = c(1,3,2,4,5,7,6, 8))

draw.one.multinom <- function(eta){
  # returns integer, takes unit simplex vector eta
  which.max(rmultinom(1, 1, eta))
}

do.CRP.iter <- function(ni.history, phi){
  # history is vector of "table counts", returns updated vector after new cust
  current.max.int = length(ni.history)
  eta <- c(ni.history, phi) / (sum(ni.history) + phi)
  new.draw <- draw.one.multinom(eta)
  
  if(new.draw == current.max.int + 1) ni.history <- c(ni.history,0)
  ni.history[new.draw] <- ni.history[new.draw] + 1
  ni.history  
}

draw.CRP <- function(N, phi){
  ni.history <- c()
  for(n in 1:N){
    ni.history <- do.CRP.iter(ni.history, phi)
  }
  ni.history
}

CRP.draw <- draw.CRP(200, .3) ; CRP.draw;

draw.doc.path <- function(gamma){
  # Random draw from nested Chinese Restraunt Process with scalar parm gamma
  
  
  
}