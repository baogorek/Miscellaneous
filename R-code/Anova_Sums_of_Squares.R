#################################################################################
# This demonstration illustrates how the relative "importance" of the effects 
# can be visualized by simple bar charts of sums of squares. The interaction is
# significant but not relatively meaningful, and the amount of Sums of Squares
# it explains is low. This code demonstrates a Type III analysis, and be aware 
# that philosophical arguments about why you both should and should not engage 
# in the approach below. Notice that when all sample sizes are equal, the Sums 
# of Squares add up to (n-1) times the variance. But change the sample sizes as
# suggested in the comments, and they no longer do! 
############################################################################


rm(list = ls())

library(qcc) 
set.seed(123)

mu.a1b1 <- 30.5
mu.a1b2 <- 31
mu.a2b1 <- 40
mu.a2b2 <- 42

# Equal sample sizes needed for "?" but try it both ways.
n.a1b1 <- 5
n.a1b2 <- 45
n.a2b1 <- 30
n.a2b2 <- 20

n <- n.a1b1 + n.a2b1 + n.a1b2 + n.a2b2

test.data <- data.frame(group = 
                        c(rep("a1b1", n.a1b1), 
                          rep("a1b2", n.a1b2),
                          rep("a2b1", n.a2b1),
                          rep("a2b2", n.a2b2)),
                        y = 
                        c(rnorm(n.a1b1, mean=mu.a1b1), 
                        rnorm(n.a1b2, mean=mu.a1b2),
                        rnorm(n.a2b1, mean=mu.a2b1),
                        rnorm(n.a2b2, mean=mu.a2b2))
)
test.data$y <- round(test.data$y, 1)

library(qcc)
my.qcc <- qcc(test.data$y, type = "xbar.one", add.stats=FALSE)

cat("Mean of y ", mean(test.data$y),  "Variance of y ", var(test.data$y))
cat("Sums of Squares of y ", var(test.data$y)*(n-1))
    


boxplot(y~group, data = test.data, main = "one way layout")

test.data$a <- factor(substr(test.data$group, 1,2))
test.data$b <- factor(substr(test.data$group, 3,4))

interaction.plot(test.data$a,  test.data$b,test.data$y)

tbl <- table(test.data$a, test.data$b)
tbl
plot(tbl)

# SAS-type options for a Type III analysis
options(contrasts=c("contr.sum","contr.poly")) 

my.lm <- lm(y ~ a + b + a:b, data = test.data)

my.aov <- aov(my.lm)
summary(my.aov)


results <- drop1(anova, scope = ~., test = "F")

results

# See the following webpage for a discussion:
#http://stats.stackexchange.com/questions/23197/type-iii-sum-of-squares-from-sas-and-r

## Adding Sums of Squares for balanced case
2685.31 + 52.42 + 16.48 + 81.38

## Adding Sums of Squares for unbalanced case (commented out sample sizes)
1396.32 + 23.58 + 13.74 + 80.70


