library(dplyr)
library(splines)
library(ggplot2)

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


# Implement Spline Regression approach

# The ultimate convolving function for the training data is:
combined_fn <- function(t) {
  0.07 * exp_decay(t, 60) - 0.27 * exp_decay(t, 13)
}

plot(combined_fn(1:259) ~ c(1:259))

# First, a check to make sure convolving with this one fn acutally works
convolve_training2 <- function(training, n) {
  sum(training[1:(n - 1)] * combined_fn((n - 1):1))
}


total <- sapply(1:nrow(train_df),
                function(n) convolve_training2(train_df$w, n)) 

plot(496 + total ~ train_df$day) # yep, it works

# Next step is to "cheat" and get a set of knots that will lead to good approx
# Construct a spline
library(splines)
my_spline <- ns(1:259, Boundary.knots = c(1, 259), knots = c(14, 40, 100))
output <- combined_fn(1:259)

my_lm <- lm(output ~ my_spline)
plot(output ~ c(1:259))
lines(predict(my_lm) ~ c(1:259), col = "blue") # Not bad!

# Now let's create the regression
delta_t <- train_df$day[2] - train_df$day[1] # TODO: what if there's missing d?

# Confusing: n is also day
new_vars <- list()
for (n in 1:nrow(train_df)) { # looping through rows of data set
  spline_arg <- (n - 1:n - 1) * delta_t # n, n - 1, ..., 1 times delta_t
  spline_pred <- predict(my_spline, newx = spline_arg)
  spline_vars <- colSums(spline_pred * train_df$w) # convolution
  spline_const <- sum(train_df$w * delta_t)
  new_vars[[n]] <- c(spline_const, spline_vars)
}

new_vars_df <- Reduce(rbind.data.frame, new_vars)
names(new_vars_df) <- paste0("z_", 1:ncol(new_vars_df))

train_df <- cbind(train_df, new_vars_df)

spline_reg <- lm(perf ~ z_1 + z_2 + z_3 + z_4 + z_5, data = train_df)
summary(spline_reg)

spline_recon <- coef(spline_reg)[2] + my_spline %*% coef(spline_reg)[3:6]
plot(combined_fn(1:259) ~ c(1:259))
lines(spline_recon ~ c(1:259), col = "red")
