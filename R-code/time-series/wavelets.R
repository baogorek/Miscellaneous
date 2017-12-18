rm(list = ls())
### construction of Scaling Functions (not yet wavelets) ###

x <- seq(from = -.5, to = 3.5, by = .01)

phi.0 <- function(x){
  as.numeric(x >= 0 & x < 1)
}
plot(phi.0(x)~x, type = "l")

phi <- function(x, starting.index){
  
  if(starting.index==0) return(phi.0(x)) 
  else return(  2*phi(2*x, starting.index-1) ) 
}
  
  
plot(phi(x,5)~x, type = "l")

## Daubechies ##

c0 = .25*(1+sqrt(3))
c1 = .25*(3+sqrt(3))
c2 = .25*(3-sqrt(3))
c3 = .25*(1-sqrt(3))

rm(phi)
phi <- function(x, starting.index){

  if(starting.index==0) return(phi.0(x))
  else{
      return(  c0*phi(2*x, starting.index-1) +
                c1*phi(2*x-1, starting.index-1) +
                c2*phi(2*x-2, starting.index-1) +
                c3*phi(2*x-3, starting.index-1)
             ) 
      }
}

#Plot the D4 scaling function
plot(  phi(x,9) ~ x, type = "l", main = "D4 Scaling Function")
 
x<- seq(-1.5, 2.5, .01)
#Plot the Daubechies wavelet
plot( -c1*phi(2*x,9) + c0*phi(2*x-1,9) - c3*phi(2*x-2,9) + c2*phi(2*x-3,9) ~ x, type = "l", main = "Daubeches Wavelet")
  

rm(list = ls())
library(wavelets)

data(ecg)
plot(ecg)

T <- nrow(ecg)

J.exact <- log(T) / log(2)
2^(J.exact)
# We are lucky here.. the largest number of scales for the DWT is exactly equal to 11
# I.E., we have a time-series of Dyadic length
# we can talk about 2^j where j = 0, ..., 11, and 2^(11) = 2048, the number of rows



fine.to.course.indices<- function(coef, i, j.set){
  
  one.based.array.index <- 2*i - j.set + 1
  function.indicies <- which(one.based.array.index %in% c(1:length(coef)))
  
  return(function.indicies)
}

fine.to.course.indices(c(1,1), 1, c(1:4))


course.to.fine.indices<- function(coef, j, i.set){
  
  one.based.array.index <- 2*i.set - j + 1
  function.indicies <- which(one.based.array.index %in% c(1:length(coef)))
  
  return(function.indicies)
}

course.to.fine.indices(c(1,1), 2, c(1:2))


f <- as.numeric(ecg) # a(J), J = 11
T <- length(f)
plot(f, type = "l")
cat("length f: ", T)

#f <- c(9,1,2,0)
#T <- 4

rough.list <- list(NULL)
detail.list <- list(NULL)

### Begin ###
J = log(T)/log(2)
wavelet.coefs <- c(1,1) #Haar's

for(k in 1:J){
  i.set <- c(1:(T/2)) # The course grid
  j.set <- c(1:T) # The fine grid

  Lf <-  sapply(i.set, function(i) sum(.5 * f[ fine.to.course.indices(wavelet.coefs, i, j.set)]) )
  L.star.f <- sapply(j.set, function(j) sum( Lf[ course.to.fine.indices(wavelet.coefs, j, i.set) ]) )
    
  f.detail <- f - L.star.f
  
  rough.list[[k]] <- L.star.f; detail.list[[k]] <- f.detail;
  f <- Lf; T <- T/2;
}

# I need to sucessively apply the course-to-fine map to the detail functions
expanded.detail <- detail.list

for(l in J:2){
  cat("Master Interation ", J-l + 1, " of ", J-1, "\n", ".............. \n")
  T <- 2^(J - l +2) # from 2^2 = 4 to 2^J (the original scale)
  
  i.set <- c(1:(T/2))
  j.set <- c(1:T) # The fine grid
  
  for(k in l:J){
    
    h <- expanded.detail[[k]]
    L.star.h <- sapply(j.set, function(j) sum( h[ course.to.fine.indices(wavelet.coefs, j, i.set) ]) )
    
    expanded.detail[[k]] <- L.star.h
    cat("finished k= ", k, " of ", J, "\n")
  }
  T <- 2*T
}


## END

recon <- rough.list[[J]][1] * rep(1, times = 2^J) + 
          expanded.detail[[11]] + expanded.detail[[10]] +
          expanded.detail[[9]] + expanded.detail[[8]] +
          expanded.detail[[7]] + expanded.detail[[6]] +
          expanded.detail[[5]] + expanded.detail[[4]] +
          expanded.detail[[3]] + expanded.detail[[2]] +
          expanded.detail[[1]]
          

plot(recon, type = "l", ylab = "reconstructed", ylim = c(-1.5, 1.5), lwd = 2)
lines(expanded.detail[[1]])

lines(as.numeric(ecg), col = "blue")

test <- data.frame(as.numeric(ecg),  recon)
