z <- seq(from = -1, to = 1, by = .01)

I.z <- as.numeric(z >= 0)
sigmoid.z <- 1/(1 + exp(-905*z))
scaled.log.sigmoid.z <- 1 + log(sigmoid.z)/log(2)

plot(I.z~z, type = "l")
lines(sigmoid.z~z, col = "blue")
lines(scaled.log.sigmoid.z~z, col = "purple")
