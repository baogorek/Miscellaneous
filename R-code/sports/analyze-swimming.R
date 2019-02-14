#Info from paper:
# Daily trainingdata collected during a phase of 160 days
# Intensity levels are split into swimming and dryland
# swimming speed is translated into intensities 1, 2, 3, 5, 8 and integrated
# Dryland training converted to water training equivalents
# Load for a day is intensity weighted training volume 
# Performance: semi-tethered swimming test, three reps of 20 meters
# conducted weekly
# performance expressed in mean velocity reached for total of 60 meters

# What I'm seeing in the data is that performance was tested at different
# frequencies for different individual

library(dplyr)

folder_path <- "c:/devl/swimming"

get_swimming_data <- function(subject_id, folder_path) {
  file <- paste0(file.path(folder_path, "subject"), subject_id, ".txt")
  swimming_df <- read.table(file)
  swimming_df$day <- 1:nrow(swimming_df)
  names(swimming_df) <- c("training", "performance", "day")
  return(swimming_df)
}

subject1_df <- get_swimming_data(5, folder_path)
subject1_df$performance <- ifelse(subject1_df$performance == 0, NA,
                                  subject1_df$performance)

plot(subject1_df$performance)

nrow(subject1)

exp_decay <- function(t, tau) {
    exp(-t / tau)
}

convolve_training<- function(training, n, tau) {
   sum(training[1:(n - 1)] * exp_decay((n - 1):1, tau))
}

training <- subject1_df$training


# Recover parameters using non-linear regression
rss <- function(theta, training, performance) {
  int  <- theta[1] # performance baseline
  k1   <- theta[2] # fitness weight
  k2   <- theta[3] # fatigue weight
  tau1 <- theta[4] # fitness decay
  tau2 <- theta[5] # fatigue decay

  fitness <- sapply(1:length(training),
                  function(n) convolve_training(training, n, tau1))

  fatigue <- sapply(1:length(training),
                  function(n) convolve_training(training, n, tau2))

  perf_hat <- int + k1 * fitness - k2 * fatigue
  rss <- sum((performance - perf_hat) ^ 2, na.rm = TRUE)
  rss
}

# TODO: think about the scale
mean_perf <- mean(subject1_df$performance, na.rm = TRUE)
starting_vals <- c(mean_perf, .01, .001, 60, 13)
                   


optim_results <- optim(starting_vals,
                       function(theta) rss(theta, subject1_df$training,
                                           subject1_df$performance),
                       method = "BFGS",
                       hessian = TRUE, control = list(maxit = 1000))

# first maximization was kind of a bust
$par
[1]   0.83590997   0.02579673   0.02621360 125.46430704 119.92012387
