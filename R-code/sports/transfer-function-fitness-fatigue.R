# started with train_df from 
# https://gist.github.com/baogorek/6d682e42079005b3bde951e98ebae89e

library(dplyr)
library(nlme)
train_df <- read.csv("/mnt/c/devl/data/train_df.csv")
head(train_df)

# True parameter values:
mu <- 496
theta1 <- exp(-1 / 60) 
theta2 <- exp(-1 / 13)
k1 <- .07 
k2 <- .27 

#Theoretical coefficient for intercept is
(1 - theta1) * (1 - theta2) * mu

#Theoretical coefficient for performance lagged once is
theta1 + theta2

#Theoretical coefficient for performance lagged twice is
-theta1 * theta2

#Theoretical coefficient for training lagged once is
k1 * theta1 - k2 * theta2

#Theoretical coefficient for training lagged twice is
-theta1 * theta2 * (k1 - k2)

# run code here: https://gist.github.com/baogorek/6d682e42079005b3bde951e98ebae89e
# or get file here: https://drive.google.com/open?id=1kk40wiVYzPXOkrPffU55Vzy-LLTrgAVh
train_df <- read.csv("/mnt/c/devl/data/train_df.csv")

train_aug <- train_df %>%
  mutate(perf_lag1 = lag(perf, n = 1, order_by = day),
         perf_lag2 = lag(perf, n = 2, order_by = day),
         train_lag1 = lag(w, n = 1, order_by = day),
         train_lag2 = lag(w, n = 2, order_by = day))

my_gls <- gls(perf ~ perf_lag1 + perf_lag2 + train_lag1 + train_lag2,
              data = train_aug[3:nrow(train_aug), ],
              corARMA(form = ~day, p = 0, q = 2))
summary(my_gls)

my_lm <- lm(perf ~ perf_lag1 + perf_lag2 + train_lag1 + train_lag2,
            data = train_aug[3:nrow(train_aug), ])
summary(my_lm)

# Longer code from working through stages

train_df <- read.csv("/mnt/c/devl/data/train_df.csv")

train_aug <- as.data.frame(train_df %>%
  mutate(perf_lag1 = lag(perf, n = 1, order_by = day),
         perf_lag2 = lag(perf, n = 2, order_by = day),
         train_lag1 = lag(w, n = 1, order_by = day),
         train_lag2 = lag(w, n = 2, order_by = day),
         z = perf - 496)
)

train_aug <- as.data.frame(train_aug %>%
  mutate(z_filter1 = z - theta1 * lag(z, n = 1, order_by = day),
         z_lag1 = lag(z, n = 1, order_by = day),
         z_lag2 = lag(z, n = 2, order_by = day),
  )
)

train_aug <- as.data.frame(train_aug %>%
  mutate(z_filter2 = z_filter1 - theta2 * lag(z_filter1, n = 1, order_by = day),
         z_filter1_lag1 = lag(z_filter1, n = 1, order_by = day)
  )
)

lag_regr1 <- gls(z_filter2 ~ 0 + train_lag1 + train_lag2,
                 data = train_aug[5:nrow(train_aug), ],
                 corARMA(form = ~day, p = 0, q = 2))
summary(lag_regr1)

## Next level of ambitiousness

lag_regr2a <- gls(z_filter1 ~ 0 + z_filter1_lag1 + train_lag1 + train_lag2,
                data = train_aug[3:nrow(train_aug), ],
                corARMA(form = ~day, p = 0, q = 2))
summary(lag_regr2a)

## Next level of ambitiousness

lag_regr3a <- gls(z ~ 0 + z_lag1 + z_lag2 + train_lag1 + train_lag2,
                data = train_aug[3:nrow(train_aug), ],
                corARMA(form = ~day, p = 0, q = 2))
summary(lag_regr3a)

## Final level of ambitiousness
lag_regr4a <- gls(perf ~ perf_lag1 + perf_lag2 + train_lag1 + train_lag2,
                  data = train_aug[3:nrow(train_aug), ],
                  corARMA(form = ~day, p = 0, q = 2))
summary(lag_regr4a)

lag_regr4b <- lm(perf ~ perf_lag1 + perf_lag2 + train_lag1 + train_lag2,
                data = train_aug[3:nrow(train_aug), ])
summary(lag_regr4b)



# sim
T <- 100
u <- rpois(T, lambda = 2)
prev_x <- 0
x <- c(prev_x)
for (t in 2:T) {
    newval <- .5 * prev_x + u[t - 1]
    x <- c(x, newval)
    prev_x <- newval
}

y <- 2 * x + rnorm(T)
sim_df <- data.frame(t = 1:T, x = x, y = y)

library(dplyr)
library(nlme)
sim_df <- sim_df %>%
  mutate(y_lag = lag(y, 1, order_by = t),
         u_lag = lag(u, 1, order_by = t)) %>%
  filter(complete.cases(.))

my_reg <- gls(y ~ y_lag + u_lag, data = sim_df,
                corARMA(value = -.5, form = ~t, p = 0, q = 1, TRUE))
summary(my_reg)


