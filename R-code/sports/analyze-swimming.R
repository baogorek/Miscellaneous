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


perf_params <- get_perf_params(world_record_perf = 1.8,
                               able_bodied_perf = 0.5, limit = 2.0,
                               type = "jumping")

get_perf_in_cp <- function(perf, a, b, limit) {
  b * log(a / (perf - limit))
}

folder_path <- "c:/devl/swimming"

get_swimming_data <- function(subject_id, folder_path, perf_params) {
  file <- paste0(file.path(folder_path, "subject"), subject_id, ".txt")
  swimming_df <- read.table(file)
  swimming_df$day <- 1:nrow(swimming_df)
  names(swimming_df) <- c("training", "performance", "day")
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

plot(perf_cp ~ performance, data = subject1_df)
plot(perf_cp ~ performance, data = subject2_df)
plot(perf_cp ~ performance, data = subject3_df)
plot(perf_cp ~ performance, data = subject4_df)
plot(perf_cp ~ performance, data = subject5_df)

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



# Debugging with H.T.
subject_df <- train_df
subject_df$perf_cp <- train_df$perf
training <- train_df$w
perf_cp <- train_df$perf


# Swimmers

subject_df <- subject1_df
training <- subject_df$training
perf_cp <- subject_df$perf_cp

mean_perf <- mean(perf_cp, na.rm = TRUE)

starting_vals <- c(mean_perf, .07, .027, 60, 13) # H.T.'s parameters
                   
#starting_vals <- c(mean_perf, .07, .027, 10, 2) # new starting vals
optim_results <- optim(starting_vals,
                       function(theta) rss(theta, training, perf_cp),
                       method = "BFGS",
                       hessian = TRUE, control = list(maxit = 4000))

r2 <- 1 - optim_results$value / sum((perf_cp - mean_perf) ^ 2, na.rm = TRUE)
params <- optim_results$par
cat("optim results.\nbaseline:", params[1], "\nfitness weight k1:", params[2],
    "\nfatigue weight k2:", params[3], "\nfitness time constant:", params[4],
    "\nfatigue time constant:", params[5], "\nr-squared:", r2, "\n")

subject_df$perf_hat <- predict_performance(params, training)

plot(subject_df$perf_cp ~ subject_df$day)
lines(subject_df$perf_hat ~ subject_df$day, col = 'red')

# Subject 1:
optim results.
baseline: 378.0693
fitness weight k1: 0.4657578
fatigue weight k2: 2.15723
fitness time constant: 34.52629
fatigue time constant: 1.354281
r-squared: .66 

# Subject 2:
optim results.
baseline: 436.2137
fitness weight k1: 3.415835
fatigue weight k2: 5.594413
fitness time constant: 4.250244
fatigue time constant: 1.643186
r-squared: 0.1973328

# Subject 3: Starting at HT's vals gives slightly higher R^2 but less interp
optim results.
baseline: 385.812
fitness weight k1: 0.4703905
fatigue weight k2: 0.7229054
fitness time constant: 23.48448
fatigue time constant: 13.39863
r-squared: 0.232773

# Subject 4:
optim results.
baseline: 390.9804
fitness weight k1: 0.05362491
fatigue weight k2: 1.408042
fitness time constant: 46.11724
fatigue time constant: 1.764495
r-squared: 0.1505 

# Subject 5:  # dramatic dependence on starting vals
baseline: 125.5009
fitness weight k1: 75.79572
fatigue weight k2: 75.92196
fitness time constant: 1915.647
fatigue time constant: 1806.457
r-squared: 0.473

combined_fn <- function(t, theta) {
  k1   <- theta[2] # fitness weight
  k2   <- theta[3] # fatigue weight
  tau1 <- theta[4] # fitness decay
  tau2 <- theta[5] # fatigue decay

  k1 * exp_decay(t, tau1) - k2 * exp_decay(t, tau2)
}

plot(combined_fn(1:180, params))

# Spline-based estimation

T <- 180 # max number of days considered in spline
library(splines)
#my_spline <- ns(1:T, Boundary.knots = c(1, T), knots = c(14, 40, 100))

my_spline <- ns(1:T, Boundary.knots = c(1, T), knots = c(3, 12, 25))

#subject_df <- train_df
#subject_df$perf_cp <- subject_df$perf
#training <- subject_df$w

subject_df <- subject1_df # TODO generalize
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

not_missing <- !is.na(subject_df$perf_cp)
subject_aug_df$perf_hat <- NA
subject_aug_df[not_missing, "perf_hat"] <- spline_reg$fitted

plot(perf_cp ~ day, data = subject_aug_df)
points(perf_hat ~ day, col = "red", data = subject_aug_df) # can't do lines: NAs

# Visualize the spline function
days_grid <- 0:T
spline_vars_grid <- predict(my_spline, newx = days_grid)
spline_vars_grid <- cbind(1, spline_vars_grid)
eta <- spline_vars_grid %*% coef(spline_reg)[-1]
plot(eta ~ days_grid, main = "subject 1")
lines(combined_fn(days_grid, params) ~ days_grid, col = "blue")
