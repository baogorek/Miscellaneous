library(survival)
library(splines)
library(BGPhazard)

N <- 1000
t_max <- 25 
delta <- .01

t_seq <- seq(0, t_max, delta)

baseline_hazard <- .1 * (1.1 + sin(2 * pi * t_seq / 12))

sim_data <- data.frame(x1 = rnorm(N), x2 = rnorm(N))

simulate_lifetime <- function(x1, x2) {
  i <- 0
  alive <- TRUE
  censored <- FALSE
  while(alive & !censored) {
    i <- i + 1
    pr_die_in_interval <- delta * baseline_hazard[i] * exp(.1 * x1 - .1 * x2)
    alive <- !(runif(1) <= pr_die_in_interval)
    censored <- t_seq[i] >= t_max 
  }
  #cat("alive:", alive, " t:", t_seq[i], " censored: ", censored, "\n")
  return(c(t_seq[i], censored))
}

dat <- sapply(1:N, FUN = function(i) with(sim_data[i, ], simulate_lifetime(x1, x2)))
sim_data <- as.data.frame(cbind(sim_data, t(dat)))
names(sim_data) <- c("x1", "x2", "t", "censored")

my_model <- coxph(Surv(time = t, event = !censored, type = 'right') ~ x1 + x2,
                  data = sim_data)
baseline_surv <- survfit(my_model)
surv_df <- data.frame(surv = baseline_surv$surv, t = baseline_surv$time)

surv_spline <- lm(surv ~ ns(t, df = 12), data = surv_df)
plot(surv_df$surv ~ surv_df$t)
lines(surv_spline$fitted ~ surv_df$t, col = "red")

grid_df <- data.frame(t = t_seq)
grid_df$S <- predict(surv_spline, newdata = grid_df)
grid_df$H <- -log(grid_df$S)

plot(diff(grid_df$H) / delta ~ t_seq[-1])
lines(baseline_hazard[-1] ~ t_seq[-1], col = "red")

bgp_dat <- cbind(sim_data$t, sim_data$censored, sim_data[, c("x1", "x2")])
CG <- CGaMRes(bgp_dat, K = 25, iterations = 3000, type.t = 3,
              c.r = rep(1, 24), type.c = 2, thinning = FALSE)

CGaPloth(CG)


