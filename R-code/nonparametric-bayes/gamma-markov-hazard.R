# http://allman.rhon.itam.mx/~lnieto/index_archivos/isi2003lenb.pdf
# Simulated data
# Corresponds to baseline hazard of h_0(t) = lambda * t

library(BGPhazard)

n <- 100
lambda <- 1
theta <- 2

sim_data <- data.frame(T = numeric(n), Z = numeric(n))

for (i in 1:n) {
  z_i <- i / (n / 2) 
  sim_data[i, "z_i"] <- z_i 
  sim_data[i, "T"] <- rweibull(1, theta * z_i + 2, lambda / (theta * z_i + 2))
}

sim_data$Z_t <- sim_data$z_i * log(sim_data$T)
sim_data$delta <- 1 # no censored observations

data_matrix <- as.matrix(sim_data[, c("T", "delta", "Z_t")])

my_markov_gam_model <- CGaMRes(data_matrix, type.t = 3, K = 10,
                               alpha = rep(.01, 10), beta = rep(.01, 10),
                               c.r = rep(1, 9), type.c = 2,
                               iterations = 2000, burn.in = 500,
                               thinning = FALSE, printtime = TRUE)

CGaPloth(my_markov_gam_model)
PlotTheta(my_markov_gam_model)
CGaPred(my_markov_gam_model)


