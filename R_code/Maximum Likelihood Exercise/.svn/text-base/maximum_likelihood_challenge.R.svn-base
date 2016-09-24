

#change to your path
y <- read.table("C:\\Documents and Settings\\ogorekb\\workspace\\R_learning\\standard_techniques\\Maximum Likelihood Exercise\\data.txt")[,1]

# Step 1: visualize the data
hist(y)

#Step 2: Learn about the gamma density: http://en.wikipedia.org/wiki/Gamma_distribution

#Below, see the density of the gamma density at parameter values "close" to the true values
x = (1/100)* c(0:2500)
density.x <- dgamma(x, shape = 2.7, scale = 3.4)  

hist(y, freq = FALSE, main = "Not the best fit")
lines(density.x ~ x, col = "blue")  

#Step 3: R provides the gamma density function for you: dgamma(), but can you match it using the formula? 
	#Hint: You should still use the built in gamma() function. Recall that gamma(n) = (n-1)! for integers n.

#Step 4: find "better" estimates of the shape and scale parameter using the principle of Maximum Likelihood
	# hints: 
		#i. see nlm()
		#ii. use a function of a single parameter vector rather than of two scalar values
		#iii. assume independence, where Pr(x1, x2) = Pr(x1)*Pr(x2)
		#iv. logarithms are your friend, since log(w*v) = log(w) + log(v)
		#v. maximizing the logarithm of a function is equivalent to maximizing the original function


