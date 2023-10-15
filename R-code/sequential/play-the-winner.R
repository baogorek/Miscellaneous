rm(list = ls())

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






