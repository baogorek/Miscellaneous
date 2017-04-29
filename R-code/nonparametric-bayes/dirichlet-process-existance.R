# TODO: Add comment
# 
# Author: OGOREKB
###############################################################################

alpha = 15
INF = 250

N = 1000 # number of draws from the random distribution
data = c(rep(0,N))

#Mixture of Dirichlet Processes
#extra uncertainty
mu = 0
sigma = .05



uniformVector = runif(N)

betaVector = rbeta(INF, 1, alpha)
atoms = rnorm(INF, 0,1) #base distribution defined here

pi = c(rep(0,INF))
cumulPi = c(rep(0,INF))

pi[1] = betaVector[1]
cumulPi[1] = pi[1]
for (k in 2:INF){
	pi[k] = betaVector[k] * prod(1 - betaVector[1:(k-1)])
	cumulPi[k] = sum(pi[1:k])
}

for (i in 1:N){
	if (uniformVector[i] <= cumulPi[1]){data[i] = atoms[1]}
	for (j in 2:INF){
		if (uniformVector[i]>cumulPi[j-1] & uniformVector[i]<=cumulPi[j]){ data[i] = atoms[j] }
	}
}

MDPdata = data + rnorm(N, mu, sigma)
