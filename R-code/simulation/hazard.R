library(survival)
library(splines)

N <- 1000
t_max_for_sim <- 25 # Go out far enough to exceed last event (censor or death)
delta <- .01

t_seq <- seq(0, t_max_for_sim, delta)

baseline_hazard <- .1 * (1.1 + sin(2 * pi * t_seq / 12))

sim_data <- data.frame(x1 = rnorm(N), x2 = rnorm(N))
simulate_lifetime <- function(x1, x2) {
  i <- 0
  alive <- TRUE
  censored <- FALSE
  t_max_observable <- rgamma(1, 25, 2) 
  while(alive & !censored) {
    i <- i + 1
    pr_die_in_interval <- delta * baseline_hazard[i] * exp(.1 * x1 - .1 * x2)
    alive <- !(runif(1) <= pr_die_in_interval)
    censored <- t_seq[i] >= t_max_observable
  }
  #cat("alive:", alive, " t:", t_seq[i], " censored: ", censored, "\n")
  return(c(t_seq[i], censored))
}

dat <- sapply(1:N, FUN = function(i) with(sim_data[i, ],
                                          simulate_lifetime(x1, x2)))
sim_data <- as.data.frame(cbind(sim_data, t(dat)))
names(sim_data) <- c("x1", "x2", "t", "censored")

# Start analysis with CoxPh

my_model <- coxph(Surv(time = t, event = !censored, type = 'right') ~ x1 + x2,
                  data = sim_data)
summary(my_model)
baseline_surv <- survfit(my_model)

surv_df <- data.frame(S = baseline_surv$surv,
                      H = baseline_surv$cumhaz,
                      t = baseline_surv$time)


surv_spline <- lm(H ~ ns(t, df = 12), data = surv_df)
plot(surv_df$H ~ surv_df$t)
lines(surv_spline$fitted ~ surv_df$t, col = "red")

grid_df <- data.frame(t = t_seq)
grid_df$H <- predict(surv_spline, newdata = grid_df)

plot(diff(grid_df$H) / delta ~ t_seq[-1], xlim = c(0, 18))
lines(baseline_hazard[-1] ~ t_seq[-1], col = "red")


# This doesn't seem to work. Maybe tell the author
library(BGPhazard)

bgp_dat <- cbind(sim_data$t, sim_data$censored, sim_data[, c("x1", "x2")])
cgm_model <- CGaMRes(bgp_dat, K = 26, iterations = 3000, type.t = 1)

CGaPloth(cgm_model)

library(rstan)
rstan_options(auto_write = TRUE)
#options(mc.cores = parallel::detectCores())

stanDat <- list(
  n_u = sum(sim_data$censored == 0),  
  X_u = as.matrix(sim_data[sim_data$censored == 0, c("x1", "x2")]),
  y_u = sim_data[sim_data$censored == 0, c("t")],

  n_c = sum(sim_data$censored == 1),
  X_c = as.matrix(sim_data[sim_data$censored == 1, c("x1", "x2")]),
  y_c = sim_data[sim_data$censored == 1, c("t")]
)

stan_model_fit <- stan(file = "../stan/gamma-markov-hazard.stan",
                       model_name = "Gamma_markov_hazard",
                       control = list(adapt_delta = .8),
                       data = stanDat,
                       iter = 2500,
                       warmup = 1000,
                       thin = 10,
                       chains = 1, verbose = FALSE)

ext <- rstan:::extract(stan_model_fit)
names(ext)

hist(ext$theta[, 1])
hist(ext$theta[, 2])


truth <- .1 * (1.1 + sin(2 * pi * seq(.5, 24.5, 1) / 12))
plot(truth ~ c(1:25), type = "l", lwd = 3, ylim = c(0, .65))

for (i in seq(1, 150, 1)) {
  lines(ext$h[i, ], col = "turquoise")
  lines(ext$h[i, ], col = "turquoise")
}

hist(ext$h[, 3])
cor(ext$h[, 1], ext$h[, 2])
