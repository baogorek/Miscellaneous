

test.df<- as.data.frame(matrix(rep(0, 450), ncol = 15) )
names(test.df) <- paste("t", 1:15, sep = "")


plot(1~1, type = "n", xlim = c(1,15), ylim = c(-2,2))
for(i in 1:30){
test.df[i,] <- round(sin(1:15) + .3*rnorm(15), 3)
lines(as.numeric(test.df[i,]), type = "l", col = topo.colors(30)[i])
}

orig.df <- test.df

test.mat <- as.matrix(test.df)
AtA <- t(test.mat) %*% test.mat

eigen.AtA <- eigen(AtA)

inv.Sigma <- diag(1/sqrt(eigen.AtA$values))

eigen.AAt <- (test.mat %*% eigen.AtA$vectors)%*% inv.Sigma

test.df <- sqrt(eigen.AtA$values[1]) * outer(eigen.AAt[,1],eigen.AtA$vectors[,1])

for(i in 2:15){
test.df <- test.df + sqrt(eigen.AtA$values[i]) * outer(eigen.AAt[,i],eigen.AtA$vectors[,i])
}

plot(1~1, type = "n", xlim = c(1,15), ylim = c(-2,2))

for(i in 1:30){
lines(as.numeric(test.df[i,]), type = "l", col = topo.colors(30)[i])
}





