rm(list = ls())

library(mvtnorm)

N <- 10000

# Drawing from a multivariate normal distribution with zero mean and the following covariance matrix
cov.matrix <- matrix(c(1, .4, -.3, 
                       .4, 1,   .2, 
                       -.3, .2,   1 ), nrow = 3, byrow = TRUE)
cov.matrix

# Creating the latent variables
latent.df <- as.data.frame( rmvnorm(N, mean = rep(0, 3), sigma = cov.matrix) )
names(latent.df) <- c("F1", "F2", "F3")

#check
cor(latent.df)


latent.df$v1 <- .5*latent.df$F1 + .16*rnorm(N)
latent.df$v2 <- -.4*latent.df$F1 + .18*rnorm(N)
latent.df$v3 <- .8*latent.df$F1 + .14*rnorm(N)
latent.df$v4 <- .5*latent.df$F1 + .16*rnorm(N)

latent.df$v5 <- .5*latent.df$F2 + .16*rnorm(N)
latent.df$v6 <- .5*latent.df$F2 + .16*rnorm(N)
latent.df$v7 <- -.5*latent.df$F2 + .16*rnorm(N)
latent.df$v8 <- .5*latent.df$F2 + .16*rnorm(N)

latent.df$v9 <- .2*latent.df$F3 + .16*rnorm(N)
latent.df$v10 <- .2*latent.df$F3 + .16*rnorm(N)
latent.df$v11 <- -.3*latent.df$F3 + .16*rnorm(N)
latent.df$v12 <- .6*latent.df$F3 + .16*rnorm(N)

library("lavaan")

myModel <- '
f1 =~ NA*v1 + v2 + v3 + v4
f2 =~ NA*v5 + v6 + v7 + v8
f3 =~ NA*v9 + v10 + v11 + v12

f1 ~~ 1*f1
f2 ~~ 1*f2
f3 ~~ 1*f3
'
fit <- cfa(myModel, data = latent.df[,-c(1,2,3)])
summary(fit, fit.measures = TRUE)
