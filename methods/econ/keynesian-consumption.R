library(dplyr)
library(car)

N <- 1E6

# Consumption structural regression parameters
a <- 5
b <- .6

investment <- 25 + 5 * rnorm(N)

income <- 80 + 5 * rnorm(N)  # Start by making income exogenous, will sample

consumption <- a + b * income + rnorm(N)

hist(income - (consumption + investment))

df_full <- data.frame(consumption=consumption, income=income,
                      investment=investment)

df <- df_full |>
  mutate(S = abs(income - (consumption + investment))) |>
  filter(S < .1)

nrow(df)


lm_full <- lm(consumption ~ income, data=df_full)
lm_sampled <- lm(consumption ~ income, data=df)

linearHypothesis(lm_full, "income=.6")
linearHypothesis(lm_sampled, "income=.6")

# ------------------------------------------------------------------------
# Generating data based on reduced form equation

n <- nrow(df)
i <- 80 + 5 * rnorm(n)  # income still exogenous with new sample size 
epsilon <- rnorm(n)
y <- a / (1 - b) + 1 / (1 - b) * i + 1 / (1 - b) * epsilon
c <- a / (1 - b) + b / (1 - b) * i + 1 / (1 - b) * epsilon
df2 <- data.frame(y=y, c=c, i=i)

lm_red <- lm(c ~ y, data=df2)
summary(lm_red)


# Notes: the first equation doesn't care about equilibrium.
# In fact, equilibrium can be broken and that equation would still hold
# This is why the structural equations still have value.
# But equilibrium does hold, by construction.
# This model does not include mechanisms that would keep the system in equalibrium
# That would involve time dynamics by necessity.
# What are the overlaps between selection and causality
# Title: is simultaneous causation real?
