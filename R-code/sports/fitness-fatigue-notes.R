library(dplyr)
library(splines)
library(ggplot2)

set.seed(1523)
train_df <- data.frame(day = 1:259, day_of_week = 0:258 %% 7)
train_df$period <- ifelse(train_df$day <= 147, "build-up", "competition")
train_df$w <- with(train_df, w <-
  -24 * (day_of_week == 0) +
   12 * (day_of_week == 1) +
    8 * (day_of_week == 2) + 
    0 * (day_of_week == 3) +
    6 * (day_of_week == 4) +
   -8 * (day_of_week == 5) +
    6 * (day_of_week == 6))

train_df$w <- rpois(nrow(train_df),
                    train_df$w + ifelse(train_df$period == "build-up", 34, 24))

mean(train_df$w[train_df$day <= 147])
mean(train_df$w[train_df$day > 147])

png("c:/devl/plots/training.png", width = 800, height = 480)
ggplot(train_df, aes(x = day, y = w)) +
  geom_bar(aes(fill = period), stat = "identity") +
  ggtitle("Simulated daily training intensities for hammer thrower") +
  xlab("Day") + ylab("Training intensity") +
  theme(text = element_text(size = 16))
dev.off()

# Exponential decay and fitness-fatigue profiles
exp_decay <- function(t, tau) {
  exp(-t / tau)
}

grid_df <- rbind(data.frame(day = 1:259, level = 400 * exp_decay(1:259, 13),
                            type = "fatigue"),
                 data.frame(day = 1:259, level = 100 * exp_decay(1:259, 60),
                            type = "fitness"))

png("c:/devl/plots/decay.png", width = 800, height = 480)
ggplot(grid_df, aes(x = day, y = level)) +
  geom_line(aes(color = type), size = 1.5) +
  ggtitle("Responses of fitness and fatigue to training impulse") +
  xlab("Day (n)") + ylab("Level of fitness or fatigue") +
  theme(text = element_text(size = 16))
dev.off()

convolve_training <- function(training, n, tau) {
  sum(training[1:(n - 1)] * exp_decay((n - 1):1, tau))
}

fitness <- sapply(1:nrow(train_df),
                  function(n) convolve_training(train_df$w, n, 60)) 

fatigue <- sapply(1:nrow(train_df),
                  function(n) convolve_training(train_df$w, n, 13)) 

E_perf <- 496 + .07 * fitness - .27 * fatigue

set.seed(45345)
train_df$perf <- E_perf + 7.0 * rnorm(nrow(train_df))

components_df <- rbind(
  data.frame(level = .27 * fatigue, day = train_df$day, type = "fatigue"),
  data.frame(level = .07 *fitness, day = train_df$day, type = "fitness"),
  data.frame(level = E_perf - 496, day = train_df$day, type = "performance"))

png("c:/devl/plots/components.png", width = 800, height = 480)
ggplot(components_df, aes(x = day, y = level)) +
  geom_col(data = train_df, aes(x = day, y = w), color = "grey", width = .2) +
  geom_line(aes(color = type), size = 1.5) +
  annotate("text", label = "training intensities", x = 70, y = 25,
           color = "grey32", size = 6) +
  ggtitle("Modeled fitness, fatigue and relative performance") +
  xlab("Day (n)") + ylab("Component level on performance scale") +
  theme(text = element_text(size = 16))
dev.off()

# Recover parameters using non-linear regression
rss <- function(theta) {
  int  <- theta[1] # performance baseline
  k1   <- theta[2] # fitness weight
  k2   <- theta[3] # fatigue weight
  tau1 <- theta[4] # fitness decay
  tau2 <- theta[5] # fatigue decay

  fitness <- sapply(1:nrow(train_df),
                    function(n) convolve_training(train_df$w, n, tau1)) 

  fatigue <- sapply(1:nrow(train_df),
                    function(n) convolve_training(train_df$w, n, tau2)) 

  perf_hat <- int + k1 * fitness - k2 * fatigue
  return(sum((train_df$perf - perf_hat) ^ 2))
}
 
optim_results <- optim(c(400, .05, .15, 20, 5), rss, method = "BFGS",
                       hessian = TRUE, control = list(maxit = 1000))
print(optim_results$par)
sqrt(diag(solve(optim_results$hessian)))

get_performance <- function(theta) {
  int  <- theta[1] # performance baseline
  k1   <- theta[2] # fitness weight
  k2   <- theta[3] # fatigue weight
  tau1 <- theta[4] # fitness decay
  tau2 <- theta[5] # fatigue decay

  fitness <- sapply(1:nrow(train_df),
                    function(n) convolve_training(train_df$w, n, tau1))

  fatigue <- sapply(1:nrow(train_df),
                    function(n) convolve_training(train_df$w, n, tau2))

  int + k1 * fitness - k2 * fatigue
}
                    
train_df$perf_hat <- get_performance(optim_results$par)
write.csv(train_df, "c:/devl/data/train_df.csv", row.names = FALSE,
          quote = FALSE)

png("c:/devl/plots/overall.png", width = 800, height = 480)
ggplot(train_df) +
  geom_point(aes(x = day, y = perf)) +
  geom_line(aes(x = day, y = perf_hat), color = "blue", size = .9) +
  ggtitle("Performance, observed and modeled") +
  xlab("Day (n)") + ylab("Performance") +
  theme(text = element_text(size = 16))
dev.off()

# Implement Spline Regression approach

exp_decay <- function(t, tau) {
  exp(-t / tau)
}

get_true_phi <- function(t) {
  0.07 * exp_decay(t, 60) - 0.27 * exp_decay(t, 13)
}
days_grid <- 1:259
my_spline <- ns(days_grid, Boundary.knots = c(1, 259), knots = c(14, 40, 100))

eta_star_lm <- lm(get_true_phi(days_grid) ~ my_spline)
print(eta_star_lm)

eta_star <- fitted(eta_star_lm)

plot_df <- rbind(
  data.frame(day = days_grid, level = get_true_phi(days_grid),
             type = "true \u03D5(t)"),
  data.frame(day = days_grid, level = eta_star,
             type = "best fit \u03B7*(t)")
)
# Plotting code omitted

png("c:/devl/plots/total-system-response.png", width = 800, height = 480)
ggplot(plot_df, aes(x = day, y = level, color = type, linetype = type)) +
  geom_line(size = 2) +
  ggtitle("Total system impulse response for H.T.'s training") +
  xlab("Day (n)") + ylab("Lag distribution value") +
  theme(text = element_text(size = 16))
dev.off()

# Now let's create the regression
# Starting with evenly space time series, no missing vals. delta_t = 1
# going for self-contained, only needs train_df to work
library(splines)
my_spline <- ns(1:259, Boundary.knots = c(1, 259), knots = c(14, 40, 100))

z_vars <- list()
for (n in 1:nrow(train_df)) {
  spline_pred  <- predict(my_spline, newx = (n - 1):1)
  spline_vars  <- colSums(spline_pred * train_df$w[1:(n - 1)]) # convolution
  spline_const <- sum(train_df$w[1:(n - 1)])
  z_vars[[n]]  <- c(spline_const, spline_vars)
}

z_vars_df <- Reduce(rbind.data.frame, z_vars)
names(z_vars_df) <- paste0("z_", 1:ncol(z_vars_df))

train_aug_df <- cbind(train_df, z_vars_df)

spline_reg <- lm(perf ~ z_1 + z_2 + z_3 + z_4 + z_5, data = train_aug_df)
summary(spline_reg)

train_aug_df$perf_hat <- spline_reg$fitted

# Finish the article by visualizing the spline function
get_true_phi <- function(t) {
  0.07 * exp_decay(t, 60) - 0.27 * exp_decay(t, 13)
}

get_eta_hat <- function(t_seq) {
  spline_vars_grid <- predict(my_spline, newx = t_seq)
  spline_vars_grid <- cbind(1, spline_vars_grid)
  eta_hat <- spline_vars_grid %*% coef(spline_reg)[-1]
  as.numeric(eta_hat)
}

convolve_with_fn <- function(training, n, impulse_fn) {
  sum(training[1:(n - 1)] * impulse_fn((n - 1):1))
}

cumulative_impact_eta <- sapply(1:nrow(train_df),
                                function(n) convolve_with_fn(train_df$w, n,
                                                             get_eta_hat))
cumulative_impact_phi <- sapply(1:nrow(train_df),
                                function(n) convolve_with_fn(train_df$w, n,
                                                             get_true_phi))

train_aug_df$perf_hat_convo_eta <- coef(spline_reg)[1] + cumulative_impact_eta
train_aug_df$perf_hat_fitness_fatigue <- 496 + cumulative_impact_phi

# Removing end effects, convolving with spline is same as regression fitted vals
all(abs(train_aug_df$perf_hat_convo_eta  - train_aug_df$perf_hat)[-1] < .001)

days_grid <- 1:259
eta_hat <- get_eta_hat(days_grid)
phi <- get_true_phi(days_grid)

plot_df <- data.frame(day = days_grid, level = phi, type = "true \u03D5(t)")
plot_df <- rbind(plot_df, data.frame(day = 1:259, level = eta_hat,
                                     type = "fitted \u03B7(t)"))

png("c:/devl/plots/spline-recon.png", width = 800, height = 480)
ggplot(plot_df, aes(x = day, y = level, color = type, linetype = type)) +
  geom_line(size = 2) +
  ggtitle("Spline-based estimation of impulse response \u03D5(t)") +
  xlab("Day (n)") + ylab("Lag distribution value") +
  theme(text = element_text(size = 16))
dev.off()

png("c:/devl/plots/overall-again.png", width = 800, height = 480)
ggplot(train_aug_df) +
  geom_point(aes(x = day, y = perf)) +
  geom_line(aes(x = day, y = perf_hat_fitness_fatigue, color = "blue"),
            size = .9) +
  geom_line(aes(x = day, y = perf_hat, color = "dark orange"), size = .9) +
  ggtitle("Performance, observed and modeled") +
  xlab("Day (n)") + ylab("Performance") +
  theme(text = element_text(size = 16)) +
  scale_colour_manual(name = "model",
    values =c("blue" = "blue","dark orange" = "dark orange"),
    labels = c("fitness-fatigue", "spline-based"))
dev.off()


