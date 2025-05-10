
## Initial exploration with randomized play the winner -------------------
N.units <- 500 # Number of experimental units that will arrive sequentially 
alpha <-  1 # Initial state of the urn: there are alpha balls of each type.

## Success Probabilities ##
p.A <- .45
p.B <- .65

Q <- (1 - p.B)/ (1 - p.A + 1-p.B)
cat("The theoretical resting place of the probability of assignment to A is", Q, "\n")

# Will make the missing data problem explicit #
contest <- data.frame(unit.id = 1:N.units, Y.A = NA, Y.B = NA, pr.unit.to.A = NA)

# initialize
contest$pr.unit.to.A[1] <- 1 / 2
  
for(i in 1:N.units) {
  # Probabilities
  if(i > 1){
    A.success.ct <- sum(contest[contest$unit.id < i, "Y.A"], na.rm = TRUE)
    B.failure.ct <- sum( 1 - contest[contest$unit.id < i, "Y.B"] , na.rm = TRUE)                   
    contest[contest$unit.id == i, "pr.unit.to.A"] <- (
      (alpha + A.success.ct + B.failure.ct) / (i - 1 + 2 * alpha)
    )
  }
  ## Outcome ##  
  if (runif(1) < contest[contest$unit.id == i, "pr.unit.to.A"]) {
      contest[contest$unit.id == i, "Y.A"] <- rbinom(1, 1, p.A)
  } else {
    contest[contest$unit.id == i, "Y.B"] <- rbinom(1, 1, p.B)
  }
}

contest$cumul.prop.to.A <-  cumsum(!is.na(contest$Y.A)) / c(1:nrow(contest))
contest$p.A.est <-  cumsum( contest$Y.A %in% c(1) )  / cumsum( contest$Y.A %in% c(0,1) )
contest$p.B.est <-  cumsum( contest$Y.B %in% c(1) )  / cumsum( contest$Y.B %in% c(0,1) )

plot(contest$pr.unit.to.A ~ contest$unit.id, type = "o", cex = .3, col = "blue", 
     ylim = c(0,1), xlab = "Unit Id", ylab = "Pr(Unit assigned to A)",
     main = "Unit level proportiion of trt A")
lines(rep(Q, times = nrow(contest)) ~ contest$unit.id, type = "l", col = "green")

plot(contest$cumul.prop.to.A ~ contest$unit.id, type = "o", cex = .3, col = "blue", 
     ylim = c(0,1), xlab = "Unit Id", ylab = "Pr(Unit assigned to A)",
     main = "Cumulative proportion of trt A")
lines(rep(Q, times = nrow(contest)) ~ contest$unit.id, type = "l", col = "green")

plot(contest$p.A.est ~ contest$unit.id, type = "o", cex = .3, col = "red", 
     ylim = c(0,1), xlab = "Unit Id", ylab = "Pr(Success|trt A)",
     main = "Estimated Probability of Success for treatment A")
lines(rep(p.A, times = nrow(contest)) ~ contest$unit.id, type = "l", col = "green")

plot(contest$p.B.est ~ contest$unit.id, type = "o", cex = .3, col = "red", 
     ylim = c(0,1), xlab = "Unit Id", ylab = "Pr(Success|trt B)",
     main = "Estimated Probability of Success for treatment B")
lines(rep(p.B, times = nrow(contest)) ~ contest$unit.id, type = "l", col = "green")


# Starting over on 2023-11-01 after reading paper ------

simulate_trial <- function(p_a, p_b, stop_value) {
  # Initial counts
  r_a <- 0
  r_b <- 0
  
  # Keep track of the history
  history_a <- c()
  history_b <- c()
  
  while (r_a < stop_value && r_b < stop_value) {
    # Calculate the current probability to draw treatment A
    prob_a <- r_a / (r_a + r_b)
    
    # If r_a and r_b are both 0, we need to randomly select a treatment since prob_a will be NaN
    if (is.nan(prob_a)) {
      prob_a <- 0.5
    }
    
    # Randomly draw a treatment
    treatment_drawn <- ifelse(runif(1) < prob_a, "A", "B")
    
    if (treatment_drawn == "A") {
      # Check for success or failure
      if (runif(1) < p_a) {
        # Success for treatment A
        r_a <- r_a + 1
      } else {
        # Failure for treatment A
        r_b <- r_b + 1
      }
    } else {
      # Check for success or failure
      if (runif(1) < p_b) {
        # Success for treatment B
        r_b <- r_b + 1
      } else {
        # Failure for treatment B
        r_a <- r_a + 1
      }
    }
    
    # Append to history
    history_a <- c(history_a, r_a)
    history_b <- c(history_b, r_b)
  }
  
  return(list(history_a = history_a, history_b = history_b))
}

# Simulation parameters
grid <- merge(
  data.frame(r=c(5, 10, 15, 30, 50)),
  data.frame(p_a=c(.3, .7, .9, .5, .8, .7), p_b = c(.1, .5, .7, .1, .4, .1)),
  all=TRUE
)[, c(2, 3, 1)]

M <- 100000  # Monte Carlo reps
for (j in 1:nrow(grid)) {
  p_a <- grid[j, "p_a"]
  p_b <- grid[j, "p_b"]
  r <- grid[j, "r"]
  cat("running with p_a =", p_a, ", p_b =", p_b, ", r =", r, "for", M, "reps\n")
  correct_stopping <- numeric(M)
  sample_size <- numeric(M)
  for (rep in 1:M) {
    result <- simulate_trial(p_a, p_b, r)
    correct_stopping[rep] <- max(result$history_a) == r
    sample_size[rep] <- length(result$history_a)
  }
  grid[j, "pr_correct_stopping"] <- mean(correct_stopping)
  grid[j, "average_sample_number"] <- mean(sample_size)
}

print(grid, digits=3)


# Modified Randomized Play the Winner with no balls given when the other team loses
simulate_trial2 <- function(p_a, p_b, stop_value) {
  # NOTE: in this modified RPW, if you don't start with 1 ball, you'll have 0 probabilities
  #  appearing after the first win
  r_a <- 1
  r_b <- 1
  history_a <- c()
  history_b <- c()
  while (r_a < stop_value && r_b < stop_value) {
    # Calculate the current probability to draw treatment A
    prob_a <- r_a / (r_a + r_b)
    treatment_drawn <- ifelse(runif(1) < prob_a, "A", "B")
    if (treatment_drawn == "A") {
      # Check for success or failure
      if (runif(1) < p_a) {
        # Success for treatment A
        r_a <- r_a + 1
      } else {
        # Failure for treatment A - NOTHING HAPPENS
      }
    } else {
      # Check for success or failure
      if (runif(1) < p_b) {
        # Success for treatment B
        r_b <- r_b + 1
      } else {
        # Failure for treatment B - NOTHING HAPPENS
      }
    }
    # Append to history
    history_a <- c(history_a, r_a)
    history_b <- c(history_b, r_b)
  }
  
  return(list(history_a = history_a, history_b = history_b))
}

# Simulation parameters
grid <- merge(
  data.frame(r=c(5, 10, 15, 30, 50)),
  data.frame(p_a=c(.3, .4, .1),
             p_b=c(.1, .2, .05)),
  all=TRUE
)[, c(2, 3, 1)]

M <- 10000  # Monte Carlo reps
for (j in 1:nrow(grid)) {
  p_a <- grid[j, "p_a"]
  p_b <- grid[j, "p_b"]
  r <- grid[j, "r"]
  cat("running with p_a =", p_a, ", p_b =", p_b, ", r =", r, "for", M, "reps\n")
  correct_stopping <- numeric(M)
  sample_size <- numeric(M)
  for (rep in 1:M) {
    result <- simulate_trial2(p_a, p_b, r)
    correct_stopping[rep] <- max(result$history_a) == r
    sample_size[rep] <- length(result$history_a)
  }
  grid[j, "pr_correct_stopping"] <- mean(correct_stopping)
  grid[j, "average_sample_number"] <- mean(sample_size)
}
print(grid, digits=3)
