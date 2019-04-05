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

library(ggplot2)

png("c:/devl/plots/training.png", width = 800, height = 960)
ggplot(subjects_df, aes(x = day, y = perf_cp)) +
  geom_col(data = subjects_df, aes(y = 10 * training), color = "grey",
           width = .2) +
  geom_point() +
  facet_grid(subject ~ .) +
  annotate("text", label = "Relative training intensities", x = 70, y = 25,
           color = "grey32") +
  ggtitle("Performance and training for five swimmers") +
  xlab("Day (n)") + ylab("Performance Scale") +
  theme(text = element_text(size = 16))
dev.off()

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

# TODO: what things have to be added now that there is missing data: the na.rm?
rss <- function(theta, training, performance) {
  perf_hat <- predict_performance(theta, training) 
  rss <- sum((performance - perf_hat) ^ 2, na.rm = TRUE)
  rss
}

# Debugging with H.T. - need to get train df in memory to do it
subject_df <- train_df
subject_df$perf_cp <- train_df$perf
training <- train_df$w
perf_cp <- train_df$perf


# Swimmers
params_df <- data.frame()
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
                         method = "BFGS",
                         hessian = TRUE, control = list(maxit = 4000))
  
  r2 <- 1 - optim_results$value / sum((perf_cp - mean_perf) ^ 2, na.rm = TRUE)
  params <- optim_results$par
  cat("optim results.\nbaseline:", params[1], "\nfitness weight k1:", params[2],
      "\nfatigue weight k2:", params[3], "\nfitness time constant:", params[4],
      "\nfatigue time constant:", params[5], "\nr-squared:", r2, "\n")
  
  params_df <- rbind(params_df, data.frame(baseline = params[1],
                                           fitness_weight = params[2],
                                           fatigue_weight = params[3],
                                           fitness_time_const = params[4],
                                           fatigue_time_const = params[5]))
  
  perf_hat <- c(perf_hat, predict_performance(params, training))
}

# https://www.r-graph-gallery.com/115-study-correlations-with-a-correlogram/
png("c:/devl/plots/table.png", width = 600, height = 300)
qplot(1:10, 1:10, geom = "blank") +
  theme_bw() +
  theme(line = element_blank(), text = element_blank()) +
  annotation_custom(grob = tableGrob(round(params_df, 2)))
dev.off()

subjects_df$perf_hat <- perf_hat

png("c:/devl/plots/swim-ff-pred.png", width = 800, height = 960)
ggplot(subjects_df, aes(x = day, y = perf_cp)) +
  geom_col(data = subjects_df, aes(y = 10 * training), color = "grey",
           width = .2) +
  geom_point() +
  geom_line(aes(y = perf_hat), color = "blue") +
  facet_grid(subject ~ .) +
  annotate("text", label = "Relative training intensities", x = 70, y = 25,
           color = "grey32") +
  ggtitle("Fitness-fatigue based performance prediction") +
  xlab("Day (n)") + ylab("Performance Scale") +
  theme(text = element_text(size = 16))
dev.off()


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

library(splines)

days_grid <- 0:180
interior_knots <- c(2, 6, 25)

my_spline <- ns(days_grid, Boundary.knots = c(1, T), knots = interior_knots)

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

png("c:/devl/plots/swim-conv-pred.png", width = 800, height = 960)
ggplot(subjects_df, aes(x = day, y = perf_cp)) +
  geom_col(data = subjects_df, aes(y = 10 * training), color = "grey",
           width = .2) +
  geom_point() +
  geom_line(aes(y = perf_hat), color = "blue") +
  geom_line(aes(y = perf_hat_spline), color = "dark orange") +
  facet_grid(subject ~ .) +
  annotate("text", label = "Relative training intensities", x = 70, y = 25,
           color = "grey32") +
  ggtitle("Adding convolution-based performance prediction") +
  xlab("Day (n)") + ylab("Performance Scale") +
  theme(text = element_text(size = 16))
dev.off()

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

# Put a knot out at 80 or 100 to show what a bad knot placement can do



