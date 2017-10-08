
w <- c(0, 1, 1, 2, 0, 0, 0, 1, 0, 0, 0, 0, 0)
T <- length(w)

p0 <- 20 
ka <- 2
kf <- 8 
tau_a <- 8
tau_f <- .5 

cat("time to recover performance, t_n: ",
    (tau_a * tau_f) / (tau_a - tau_f) * log(kf / ka), "\n")

fitness <- rep(0, T)
fatigue <- rep(0, T)
perf <- rep(0, T)

for (t in 1:T) {
  s_range <- 1:(t - 1)
  fitness[t] <- sum(exp(-(t - s_range) / tau_a) * w[s_range])
  fatigue[t] <- sum(exp(-(t - s_range) / tau_f) * w[s_range])
}

perf <- p0 + ka * fitness - kf * fatigue
t_seq <- 1:T
max_ff <- max(c(ka * fitness, kf * fatigue))

par(mfrow = c(4, 1))
plot(perf ~ t_seq, type = "b", col = "blue", main = "performance function")
abline(h = 20)
plot(ka * fitness ~ t_seq, type = "b", col = "green",
     ylim = c(0, max_ff * 1.1), main = "Fitness function")
plot(kf * fatigue ~ t_seq, type = "b", col = "red", ylim = c(0, max_ff * 1.1),
     main = "Fatigue function")
plot(w, ylab = "load", type = "h", main = "load function")


