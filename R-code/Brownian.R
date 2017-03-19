# TODO: Add comment
# 
# Author: OGOREKB
###############################################################################

rm(list = ls())

pdf("C:\\learning\\BrownianMotion.pdf")

createT = function(res){
	t = (1/res)*c(0:res)
	dim(t) = length(t)
	return( t )
}

t = createT(1000)

# Case n = 0
y_n = rnorm(1)*t
a = -.5 -abs(min(y_n))*1.5
b = .5 + abs(max(y_n))*1.5

par(mfrow = c(2,1))
plot(rep(1,length(t))~t, type = "l", lty = 2, lwd = .5, col = "blue", main = "Haar Wavelet and Schauder function for n = 0", ylim = c(-1.5, 1.5) )
lines(rep(1,length(t))*t ~t, type = "l", lty = 1, col = "green", lwd = 3)
lines(y_n~t, type = "h", lty = 1)
plot(y_n~t, type = "l", lty = 1, lwd = 1, col = "red", main = "Approximate Brownian Motion for n = 0", ylim = c(a, b))


# Cases n = 1,2,3,4,...

harrWavelet = function(n,k,t){
	if(t > (k-1)*2^(-n) & t <= (k)*2^(-n)) return( +2^((n-1)/2) ) 
	else if(t > (k)*2^(-n) & t <= (k+1)*2^(-n))	return( -2^((n-1)/2) )
	else return(0)
}

#Integral of the Harr Wavelet. Notice the use of recursion via lazy evaluation
schauderFunction = function(n,k,t){
	if(t <= 2^(-n)*(k-1) | t > (k+1)*2^(-n)) return(0)
	else if(t > (k-1)*2^(-n) & t <= (k)*2^(-n)){
		return( harrWavelet(n,k,t)*( t - (k-1)*2^(-n) )  )
	}
	else if(t > (k)*2^(-n) & t <= (k+1)*2^(-n)){
		return(schauderFunction(n,k,(k)*2^(-n)) + harrWavelet(n,k,t)*( t - (k)*2^(-n) ) )
	}
}

#could be much better implemented as an object
normalSequence = function(n) return (rnorm(1:2^(n-1)))
##below is like __getitem__ in Python
getNormalRV = function(normalSequence,k){
	return( normalSequence[k+1-(k+1)/2])
}

oddNumbers = function(n){
	b = 2^n - 1
	z = c(1:b)
	dim(z) <- length(z)
	return(z[z/2 != round(z/2)]) #list comprehension in R
}

for (n in 1:10)
{
	
	normalSequenceN = normalSequence(n)
	oddNumbersN = oddNumbers(n)
	if (n == 1) {dim(oddNumbersN) = 1} # why did I have to do this?
	
	Harr_n = apply(apply(oddNumbersN,1,function(w) apply(t,1,function(z) harrWavelet(n,k=w,z))) ,1, sum ) #sum over columns;
	Schauder_n = apply(apply(oddNumbersN ,1,function(w) apply(t,1,function(z) schauderFunction(n,k=w,z))) ,1, sum ) #sum over columns;
	addOn_n = apply(apply(oddNumbersN ,1,function(w) apply(t,1,function(z) getNormalRV(normalSequenceN,k=w)*schauderFunction(n,k=w,z))) ,1, sum ) #sum over columns;
	
	y_n = y_n + addOn_n
	

	par(mfrow = c(2,1))
	plot( Harr_n~t, type = "l", lty = 2, lwd = .5, col = "blue", main = paste("Haar Wavelet, Schauder functions for n = ", n), ylim = c(-1.5, 1.5))
	lines(Schauder_n~t, type = "l", lty = 1, col = "green", lwd = 3)
	lines(addOn_n~t, type = "h", lty = 1)
	plot(y_n~t, type = "l", lty = 1, lwd = 1, col = "red", main = paste("Approximate Brownian Motion for n = ",n), ylim = c(a,b))
}

dev.off()
