#Info from paper:
# Daily trainingdata collected during a phase of 160 days
# Intensity levels are split into swimming and dryland
# swimming speed is translated into intensities 1, 2, 3, 5, 8 and integrated
# Dryland training converted to water training equivalents
# Load for a day is intensity weighted training volume 
# Performance: semi-tethered swimming test, three reps of 20 meters
# conducted weekly
# performance expressed in *mean velocity* reached for total of 60 meters

# What I'm seeing in the data is that performance was tested at different
# frequencies for different individual

# running function from fitness-fatigue-notes.R

# Over time, performance can be observed to follow:
# y = L + a exp(-t / b)
# a: amplitude parameter (positive for running, negative for throwing)
# b: time paramter
# L: ultimate limit
#
# Suggests a transformation where you treat your performance as where
# you are on the x-axis in approaching the limit
#
# g(y) = b * ln(a / (y - L))
# Get a & b through setting:
# 1000 = g(world record performance)
#    0 = g(able-bodied individual performance)

# Code Block 1 ---------------------------------------------------------------
get_perf_params <- function(world_record_perf, able_bodied_perf, limit,
                             type = "running") {
  # sum of squares around fixed point
  rss <- function(a, b, world_record_perf, able_bodied_perf, limit) {
    (1000 - b * log(a / (world_record_perf - limit))) ** 2 +
    (0    - b * log(a / (able_bodied_perf - limit))) ** 2
  }

  stopifnot(type %in% c("running", "jumping"))
  a_starting <- ifelse(type == "running", 5, -5)
  b_starting <- 100

 optim_results <- optim(c(a_starting, b_starting),
                        function(params) rss(params[1], params[2],
                                             world_record_perf,
                                             able_bodied_perf, limit))
 list(a = optim_results$par[1], b = optim_results$par[2], limit = limit,
      optim_results = optim_results)
}


# Code Block 2 ---------------------------------------------------------------

library(ggplot2)

# Files are at ...
folder_path <- "c:/devl/swimming" # Update accordingly

perf_params <- get_perf_params(world_record_perf = 1.8,
                               able_bodied_perf = 0.5, limit = 2.0,
                               type = "jumping")

get_perf_in_cp <- function(perf, a, b, limit) {
  b * log(a / (perf - limit))
}


get_swimming_data <- function(subject_id, folder_path, perf_params) {
  file <- paste0(file.path(folder_path, "subject"), subject_id, ".txt")
  swimming_df <- read.table(file)
  swimming_df$subject <- subject_id
  swimming_df$day <- 1:nrow(swimming_df)
  names(swimming_df) <- c("training", "performance", "subject", "day")
  swimming_df$performance <- ifelse(swimming_df$performance == 0, NA,
                                    swimming_df$performance)

  swimming_df$perf_cp <- get_perf_in_cp(swimming_df$perf, perf_params$a,
                                        perf_params$b, perf_params$limit)
  return(swimming_df)
}

subject1_df <- get_swimming_data(1, folder_path, perf_params)
subject2_df <- get_swimming_data(2, folder_path, perf_params)
subject3_df <- get_swimming_data(3, folder_path, perf_params)
subject4_df <- get_swimming_data(4, folder_path, perf_params)
subject5_df <- get_swimming_data(5, folder_path, perf_params)

subjects_df <- rbind(subject1_df, subject2_df, subject3_df, subject4_df,
                     subject5_df)

png("c:/devl/plots/training.png", width = 800, height = 960)
ggplot(subjects_df, aes(x = day, y = perf_cp)) +
  geom_col(data = subjects_df, aes(y = 10 * training), color = "grey",
           width = .2) +
  geom_point() +
  facet_grid(subject ~ .) +
  annotate("text", label = "Relative training intensities (x10)",
           x = 70, y = 25, color = "grey32") +
  ggtitle("Performance and training for five swimmers") +
  xlab("Day (n)") + ylab("Performance Scale") +
  theme(text = element_text(size = 16))
dev.off()

#plot(perf_cp ~ performance, data = subject1_df)


# Code block 3 ---------------------------------------------------------------
library(dplyr)
library(gridExtra)
library(ggplot2)

exp_decay <- function(t, tau) {
    exp(-t / tau)
}

convolve_training<- function(training, n, tau) {
   sum(training[1:(n - 1)] * exp_decay((n - 1):1, tau))
}

predict_performance <- function(theta, training) {
  int  <- theta[1] # performance baseline
  k1   <- theta[2] # fitness weight
  k2   <- theta[3] # fatigue weight
  tau1 <- theta[4] # fitness decay
  tau2 <- theta[5] # fatigue decay

  fitness <- sapply(1:length(training),
                    function(n) convolve_training(training, n, tau1))

  fatigue <- sapply(1:length(training),
                    function(n) convolve_training(training, n, tau2))

  E_hat_perf <- int + k1 * fitness - k2 * fatigue
  E_hat_perf
}

rss <- function(theta, training, performance) {
  perf_hat <- predict_performance(theta, training) 
  rss <- sum((performance - perf_hat) ^ 2, na.rm = TRUE)
  rss
}

get_jacobian <- function(theta, training, h = .0001) {
  p <- length(theta)
  J_theta <- matrix(numeric(length(training) * p), ncol = p)

  for (j in 1:p) {
    theta_plus <- theta
    theta_plus[j] <- theta_plus[j] + h

    f_prime <- (predict_performance(theta_plus, training) -
                predict_performance(theta, training)) / h
    J_theta[, j] <- f_prime
  }
  J_theta
}

estimate_sigma_sq <- function(theta, training, performance) {
  n <- length(training)
  p <- length(theta)

  return(rss(theta, training, performance) / (n - p))
}

get_standard_errors <- function(theta, training, performance) {
  X_theta <- get_jacobian(theta, training)
  sigma_sq_hat <- estimate_sigma_sq(theta, training, performance)
  
  V <- sigma_sq_hat * solve(t(X_theta) %*% X_theta)
  sqrt(diag(V))
}

params_df <- data.frame()
se_df <- data.frame()
perf_hat <- c()

for (j in 1:5) {
  subject_df <- subjects_df %>% filter(subject == j)
  training <- subject_df$training
  perf_cp <- subject_df$perf_cp
  
  mean_perf <- mean(perf_cp, na.rm = TRUE)
  #starting_vals <- c(mean_perf, .07, .027, 60, 13) # H.T.'s parameters
  starting_vals <- c(mean_perf, .07, .027, 10, 2) # new starting vals
  optim_results <- optim(starting_vals,
                         function(theta) rss(theta, training, perf_cp),
                         method = "BFGS", control = list(maxit = 4000))
  params <- optim_results$par

  std_errors <- get_standard_errors(params, training, perf_cp)
  names(params) <- c("baseline", "fitness_weight", "fatigue_weight",
                     "fitness_time_const", "fatigue_time_const") 
  names(std_errors) <- names(params)

  params_df <- rbind(params_df, as.data.frame(t(params)))
  se_df <- rbind(se_df, as.data.frame(t(std_errors)))

  perf_hat <- c(perf_hat, predict_performance(params, training))
}
subjects_df$perf_hat <- perf_hat

# https://www.r-graph-gallery.com/115-study-correlations-with-a-correlogram/
png("c:/devl/plots/table.png", width = 600, height = 300)
qplot(1:10, 1:10, geom = "blank") +
  theme_bw() +
  theme(line = element_blank(), text = element_blank()) +
  annotation_custom(grob = tableGrob(round(params_df, 2)))
dev.off()

png("c:/devl/plots/table2.png", width = 600, height = 300)
qplot(1:10, 1:10, geom = "blank") +
  theme_bw() +
  theme(line = element_blank(), text = element_blank()) +
  annotation_custom(grob = tableGrob(round(se_df, 2)))
dev.off()


png("c:/devl/plots/swim-ff-pred.png", width = 800, height = 960)
ggplot(subjects_df, aes(x = day, y = perf_cp)) +
  geom_col(data = subjects_df, aes(y = 10 * training), color = "grey",
           width = .2) +
  geom_point() +
  geom_line(aes(y = perf_hat), color = "blue", size = 1) +
  facet_grid(subject ~ .) +
  annotate("text", label = "Relative training intensities (x10)",
           x = 70, y = 25, color = "grey32") +
  ggtitle("Fitness-fatigue based performance prediction") +
  xlab("Day (n)") + ylab("Performance Scale") +
  theme(text = element_text(size = 16))
dev.off()

# Commented code shows impulse response of fitness fatigue model
#combined_fn <- function(t, theta) {
#  k1   <- theta[2] # fitness weight
#  k2   <- theta[3] # fatigue weight
#  tau1 <- theta[4] # fitness decay
#  tau2 <- theta[5] # fatigue decay
#
#  k1 * exp_decay(t, tau1) - k2 * exp_decay(t, tau2)
#}
#
#plot(combined_fn(1:180, params))

# Code Block 4, Spline-based estimation --------------------------------------
library(dplyr)
library(ggplot2)
library(splines)

days_grid <- 0:180
interior_knots <- c(2, 6, 25)
#interior_knots <- c(6, 25, 80)

my_spline <- ns(days_grid, knots = interior_knots)

perf_hat <- c()
eta_df <- data.frame()

for (j in 1:5) {
  subject_df <- subjects_df %>% filter(subject == j) 
  training <- subject_df$training
  
  z_vars <- list()
  for (n in 1:nrow(subject_df)) {
    spline_pred  <- predict(my_spline, newx = (n - 1):1)
    spline_vars  <- colSums(spline_pred * training[1:(n - 1)]) # convolution
    spline_const <- sum(training[1:(n - 1)])
    z_vars[[n]]  <- c(spline_const, spline_vars)
  }
  
  z_vars_df <- Reduce(rbind.data.frame, z_vars)
  names(z_vars_df) <- paste0("z_", 1:ncol(z_vars_df))
  
  subject_aug_df <- cbind(subject_df, z_vars_df)
  
  spline_reg <- lm(perf_cp ~ z_1 + z_2 + z_3 + z_4 + z_5, data = subject_aug_df)
  summary(spline_reg)
  
  subject_aug_df$perf_hat <- predict(spline_reg, subject_aug_df)
  perf_hat <- c(perf_hat, subject_aug_df$perf_hat)
  
  
  spline_vars_grid <- cbind(1, predict(my_spline, newx = days_grid))
  eta <- as.numeric(spline_vars_grid %*% coef(spline_reg)[-1])
  eta_df <- rbind(eta_df, data.frame(subject = j, day = days_grid, eta = eta))

}
subjects_df$perf_hat_spline <- perf_hat

png("c:/devl/plots/swim-eta.png", width = 800, height = 960)
ggplot(eta_df, aes(x = day, y = eta)) +
  geom_line(color = "blue", size = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "green", size = 1) +
  geom_point(data = data.frame(x = interior_knots), aes(x = x, y = 0),
             shape = 10, size = 3, color = "brown") +
  facet_grid(subject ~ .) +
  ggtitle("Spline-based estimation of impulse response \u03D5(t)") +
  xlab("Day (n)") + ylab("Lag distribution value") +
  theme(text = element_text(size = 16))
dev.off()

png("c:/devl/plots/swim-conv-pred.png", width = 800, height = 960)
ggplot(subjects_df, aes(x = day, y = perf_cp)) +
  geom_col(data = subjects_df, aes(y = 10 * training), color = "grey",
           width = .2) +
  geom_point() +
  geom_line(aes(y = perf_hat), color = "blue", size = 1) +
  geom_line(aes(y = perf_hat_spline), color = "dark orange", size = 1) +
  facet_grid(subject ~ .) +
  annotate("text", label = "Relative training intensities (x10)",
           x = 70, y = 25, color = "grey32") +
  ggtitle("Adding convolution-based performance prediction") +
  xlab("Day (n)") + ylab("Performance Scale") +
  theme(text = element_text(size = 16))
dev.off()

# Put a knot out at 80 and rerun to show what a bad knot placement can do
png("c:/devl/plots/swim-eta-bad-knots.png", width = 800, height = 960)
ggplot(eta_df, aes(x = day, y = eta)) +
  geom_line(color = "blue", size = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "green", size = 1) +
  geom_point(data = data.frame(x = interior_knots), aes(x = x, y = 0),
             shape = 10, size = 3, color = "brown") +
  facet_grid(subject ~ .) +
  ggtitle(paste("Spline-based estimation of \u03D5(t) with less favorable",
          "knot placement")) +
  xlab("Day (n)") + ylab("Lag distribution value") +
  theme(text = element_text(size = 16))
dev.off()

