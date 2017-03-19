rm(list = ls())

N.units <- 1500 # for instance, household/renewal-date combinations
alpha <-  1 # Initial state of the urn: alpha balls of each type. DOES NOT HAVE TO BE AN INTEGER.
#i.e., how much weight do you put on the prior hypothesis that the treatments are equal?

## Success Probabilities ##
p.A <- .45
p.B <- .90

Q <- (1-p.B)/ (1-p.A + 1-p.B)
cat("The theoretical resting place of the probability of assignment to A is", Q)

# Will make the missing data problem explicit #
contest <- data.frame(unit.id = 1:N.units, Y.A = NA, Y.B = NA, pr.unit.to.A = NA)

#initial probability of recieving treatment A #
contest$pr.unit.to.A[1] <- 1/2 # what the formula reduces to at n = 1
  
for(i in 1:N.units){
 ## Randomized Experimental Assignment ##
  
  if(i > 1){
    A.success.ct <- sum(contest[contest$unit.id < i, "Y.A"], na.rm = TRUE)
    B.failure.ct <- sum( 1 - contest[contest$unit.id < i, "Y.B"] , na.rm = TRUE)                   
    contest[contest$unit.id == i, "pr.unit.to.A"] <- (alpha + A.success.ct + B.failure.ct) / (i - 1 + 2*alpha)
  }

## Observed Experimental Outcome ##  
  if (runif(1) < contest[contest$unit.id == i, "pr.unit.to.A"]){ 
      contest[contest$unit.id == i, "Y.A"] <- rbinom(1,1,p.A) 
    } else{
      contest[contest$unit.id == i, "Y.B"] <- rbinom(1,1,p.B)
    }  
}

contest$cumul.prop.to.A <-  cumsum(!is.na(contest$Y.A)) / c(1:nrow(contest))
contest$p.A.est <-  cumsum( contest$Y.A %in% c(1) )  / cumsum( contest$Y.A %in% c(0,1) )
contest$p.B.est <-  cumsum( contest$Y.B %in% c(1) )  / cumsum( contest$Y.B %in% c(0,1) )



plot(contest$pr.unit.to.A ~ contest$unit.id, type = "o", cex = .3, col = "blue", 
     ylim = c(0,1), xlab = "Unit Id", ylab = "Pr(Unit assigned to A)",
     main = "Unit level probabilities of trt A")
lines(rep(Q, times = nrow(contest)) ~ contest$unit.id, type = "l", col = "green")

plot(contest$cumul.prop.to.A ~ contest$unit.id, type = "o", cex = .3, col = "blue", 
     ylim = c(0,1), xlab = "Unit Id", ylab = "Pr(Unit assigned to A)",
     main = "Cumulative probability of trt A")
lines(rep(Q, times = nrow(contest)) ~ contest$unit.id, type = "l", col = "green")


plot(contest$p.A.est ~ contest$unit.id, type = "o", cex = .3, col = "red", 
     ylim = c(0,1), xlab = "Unit Id", ylab = "Pr(Success|trt A)",
     main = "Probability of Success for treatment A")
lines(rep(p.A, times = nrow(contest)) ~ contest$unit.id, type = "l", col = "green")

plot(contest$p.B.est ~ contest$unit.id, type = "o", cex = .3, col = "red", 
     ylim = c(0,1), xlab = "Unit Id", ylab = "Pr(Success|trt B)",
     main = "Probability of Success for treatment B")
lines(rep(p.B, times = nrow(contest)) ~ contest$unit.id, type = "l", col = "green")






