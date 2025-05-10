
# First, copy the simulation and loop-based convolution mehthod from the article:
# https://medium.com/towards-data-science/modeling-cumulative-impact-part-ii-2bf65db3bb98
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

set.seed(1523)
train_df$w <- rpois(nrow(train_df),                                             
                    train_df$w + ifelse(train_df$period == "build-up", 34, 24)) 
                                                                                
exp_decay <- function(t, tau) {                                                 
  exp(-t / tau)                                                                 
}                                                                               
                                                                                
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

# Method as introduced in the article
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


# Try to fix the off-by-one problem 

# Did I mean to predict outside of the spline's boundary knot?

# Now I'm going to try to replicate this without a loop through observations

# Did I mean to predict outside of the spline's boundary knot?
rev_spline <- predict(my_spline, newx=258:0)
# I had to to match this closely. I'm a little confused above.
# Since we're estimating the spline coefficients, this should be relatively
# forgiving *IF* we just stay consistent! 

# As close as I'm going to get, I have a zero in the first position but
# My index math was 1:0 so that was unintended
convolve(train_df$w, c(rep(1, nrow(train_df) - 1), 0), type="open")[1:nrow(train_df)] |> round(3)
z_vars_df[, 1]

convolve(train_df$w, rev_spline[, 1], type="open")[1:nrow(train_df)] |> round(3)
z_vars_df[, 2] |> round(3)

convolve(train_df$w, rev_spline[, 2], type="open")[1:nrow(train_df)] |> round(3)
z_vars_df[, 3] |> round(3)

convolve(train_df$w, rev_spline[, 3], type="open")[1:nrow(train_df)] |> round(3)
z_vars_df[, 4] |> round(3)

convolve(train_df$w, rev_spline[, 4], type="open")[1:nrow(train_df)] |> round(3)
z_vars_df[, 5] |> round(3)

# Weird how close they are but they're not exact


# Ok now time to do this right ---------------
# Make boundary knots the same 

w_vars_df <- train_df
my_spline <- ns(1:259, Boundary.knots = c(0, 258), knots = c(14, 40, 100))
rev_spline <- predict(my_spline, newx=258:0)
w_vars_df$z_1 <- convolve(train_df$w, rep(1, nrow(train_df)), type="open")[1:nrow(train_df)]

for (j in 1:ncol(my_spline)) {
  w_vars_df[, paste0("z_", j + 1)] <- convolve(train_df$w, rev_spline[, j], type="open")[1:nrow(train_df)]
}
w_vars_df |> head()

spline_reg2 <- lm(perf ~ z_1 + z_2 + z_3 + z_4 + z_5, data = w_vars_df)
summary(spline_reg2)

w_vars_df$perf_hat <- spline_reg2$fitted

# Now let's get the impulse response
forward_spline <- predict(my_spline, newx=0:258)
eta_t <- coef(spline_reg2)[2] + forward_spline %*% coef(spline_reg2)[3:6]  # 0-based
plot(eta_t ~ c(0:258))
abline(h=0)


# Make boundary knots less than the size of the series
# You can see in the plot that it does degrade performance by just a little.
w_vars_df <- train_df
my_spline <- ns(1:259, Boundary.knots = c(0, 150), knots = c(14, 40, 100))
rev_spline <- predict(my_spline, newx=258:0)
w_vars_df$z_1 <- convolve(train_df$w, rep(1, nrow(train_df)), type="open")[1:nrow(train_df)]

for (j in 1:ncol(my_spline)) {
  w_vars_df[, paste0("z_", j + 1)] <- convolve(train_df$w, rev_spline[, j], type="open")[1:nrow(train_df)]
}
w_vars_df |> head()

spline_reg2 <- lm(perf ~ z_1 + z_2 + z_3 + z_4 + z_5, data = w_vars_df)
summary(spline_reg2)

w_vars_df$perf_hat <- spline_reg2$fitted

# Now let's get the impulse response
forward_spline <- predict(my_spline, newx=0:258)
eta_t <- coef(spline_reg2)[2] + forward_spline %*% coef(spline_reg2)[3:6]  # 0-based
plot(eta_t ~ c(0:258))
abline(h=0)


  # 1-D Convolution in R
  w <- c(0, 0, 1, 1, 0)
  y <- c(1, 2, 3, 4, 5)
  
  convolve(y, w, type="open")[1:5] |> round(1)
  # 0 1 3 5 7 
  
  w <- c(0, 0, 0, 1, 1)
  y <- c(1, 2, 3, 4, 5)
  
  convolve(y, w, type="open")[1:5] |> round(1)
  # 1 3 5 7 9


  # Trying a much longer weights vector in the convolution
  w <- c(900, 0, 0, 0, 0, 0, 1, 1, 0)
  y <- c(1, 2, 3, 4, 5)
    
  convolve(y, w, type="open")[1:5] |> round(1)
  # 0 1 3 5 7
  
  w <- c(900, 0, 0, 0, 0, 0, 0, 1, 1)
  y <- c(1, 2, 3, 4, 5)
    
  convolve(y, w, type="open")[1:5] |> round(1)
  # 1 3 5 7 9
 

