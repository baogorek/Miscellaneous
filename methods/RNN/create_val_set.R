val_df <- data.frame(day = 1:259, day_of_week = 0:258 %% 7)                   
val_df$period <- ifelse(val_df$day <= 100, "build-up", "rest")       
val_df$w <- with(val_df, w <-                                               
  -10 * (day_of_week == 0) +                                                    
   6 * (day_of_week == 1) +                                                    
    0 * (day_of_week == 2) +                                                    
    0 * (day_of_week == 3) +                                                    
    5 * (day_of_week == 4) +                                                    
   -4 * (day_of_week == 5) +                                                    
    3 * (day_of_week == 6))                                                     

set.seed(9245)
val_df$w <- rpois(nrow(val_df),                                             
                    val_df$w + ifelse(val_df$period == "build-up", 40, 10))
                                                                                
exp_decay <- function(t, tau) {                                                 
  exp(-t / tau)                                                                 
}                                                                               
                                                                                
convolve_training <- function(training, n, tau) {                               
  sum(training[1:(n - 1)] * exp_decay((n - 1):1, tau))                          
}                                                                               
                                                                                
fitness <- sapply(1:nrow(val_df),                                             
                  function(n) convolve_training(val_df$w, n, 60))             
                                                                                
fatigue <- sapply(1:nrow(val_df),                                             
                  function(n) convolve_training(val_df$w, n, 13))             
                                                                                
E_perf <- 496 + .07 * fitness - .27 * fatigue
                  
val_df$perf <- E_perf + 7.0 * rnorm(nrow(val_df))
write.csv(val_df, 'val_df.csv', row.names=FALSE)
