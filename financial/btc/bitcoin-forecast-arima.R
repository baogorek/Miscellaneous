# Load required package
library(forecast)
library(dplyr)
library(readr)
library(lubridate)


# CPI-U, not seasonally adjusted
cpi_df <- read_csv("cpi-CPIAUCNS.csv")

# Since CPI for July 2020 (for example) represents the average for all of July,
# it is conceptually aligned with financial prices recorded on July 31, 2020.
cpi_df <- cpi_df %>%
  mutate(adj_observation_date = ceiling_date(observation_date, "month") - days(1)) %>%
  select(date=adj_observation_date, cpi=CPIAUCNS)

# Impute a CPI for Feb 2025
latest_cpi <- cpi_df %>% filter(date == "2025-01-31") %>% pull(cpi)
estimated_cpi <- latest_cpi * (1 + 0.025 / 12)  # 2.5% annual inflation

cpi_df <- cpi_df %>%
  add_row(date = as.Date("2025-02-28"), cpi = estimated_cpi)

plot(cpi ~ date, data = cpi_df)

df <- read_csv("btc_month_end_closes.csv")

# Create an inflation adjusted btc series, using the CPI for December 2019
cpi_base <- cpi_df %>% filter(date == "2019-12-31") %>% pull(cpi)

df <- df %>%
  inner_join(cpi_df, by="date") %>%
  mutate(adj_close = close * (cpi_base / cpi))

plot(close ~ date, data = df)
lines(adj_close ~ date, data = df, col = "blue")

future_steps <- 12
future_dates <- seq.Date(max(df$date) + 1, by = "month", length.out = future_steps + 1) - 1
future_dates <- future_dates[2:length(future_dates)]

df_future <- data.frame(date = future_dates, close = NA)

df <- bind_rows(df, df_future)
df$t <- 1:nrow(df)

# Trying to force an outlier here
df[df$t == 177, "close"] = 250000


df$log_close_orig <- log(df$close)
df$log_close <- log(df$adj_close)

plot(log_close ~ date, data=df, type = "l")
lines(log_close_orig ~ date, data=df, col="blue")

t_stop <- 140   # we'll need to put this in a grid
df$t_before_stop <- ifelse(df$t <= t_stop, df$t, 0)
df$t2_before_stop <- ifelse(df$t <= t_stop, df$t ^ 2, 0)
df$plateau <- ifelse(df$t > t_stop, 1, 0)

# Regression matrix (t, t2_before_stop, plateau)
xreg <- as.matrix(df[, c("t_before_stop", "t2_before_stop", "plateau")])

# Fit regression with AR(1) errors (ARIMA(1,0,0))
my_mod <- Arima(
  df$log_close,
  order = c(1, 0, 0),  # AR(1)
  xreg = xreg,
  method = "ML"  # Maximum likelihood for AIC
)

pred_obj <- predict(my_mod, newxreg = xreg)
df$pred <- pred_obj$pred

pred_obj <- fitted(my_mod, newxreg = xreg)
df$pred <- pred_obj

plot(log_close ~ t, data=df, xlim = c(0, 185), ylim = c(-3, 15))
lines(pred ~ t, data = df, col = "red")

  
 




# Step 1: Define the Range of t_stop Values to Search
t_stop_range <- seq(5, 12, by = 0.1)  # From 5 to 12 (2014 to 2021), step size 0.1

# Step 2: Initialize Storage for Results
aic_values <- numeric(length(t_stop_range))
models <- list()

# Step 3: Grid Search Over t_stop
for (i in 1:length(t_stop_range)) {
  t_stop <- t_stop_range[i]
  
  # Create regression terms for this t_stop
  data$t <- data$time
  data$t2_before_stop <- ifelse(data$time <= t_stop, data$time^2, 0)
  data$plateau <- ifelse(data$time > t_stop, 1, 0)
  
  # Regression matrix (t, t2_before_stop, plateau)
  xreg <- as.matrix(data[, c("t", "t2_before_stop", "plateau")])
  
  # Fit regression with AR(1) errors (ARIMA(1,0,0))
  fit <- tryCatch(
    {
      Arima(
        data$log_price,
        order = c(1, 0, 0),  # AR(1)
        xreg = xreg,
        method = "ML"  # Maximum likelihood for AIC
      )
    },
    error = function(e) NULL
  )
  
  # Store the model and AIC
  if (!is.null(fit)) {
    models[[i]] <- fit
    aic_values[i] <- fit$aic
  } else {
    aic_values[i] <- Inf  # If model fails, set AIC to Inf
  }
}

# Step 4: Find the Best t_stop
best_idx <- which.min(aic_values)
best_t_stop <- t_stop_range[best_idx]
best_model <- models[[best_idx]]

# Print results
cat("Best t_stop:", best_t_stop, "\n")
cat("AIC at best t_stop:", aic_values[best_idx], "\n")
summary(best_model)

# Step 5: Compute Fitted Values with the Best Model
# Recreate regression terms for the best t_stop
data$t2_before_stop <- ifelse(data$time <= best_t_stop, data$time^2, 0)
data$plateau <- ifelse(data$time > best_t_stop, 1, 0)
xreg_best <- as.matrix(data[, c("t", "t2_before_stop", "plateau")])

# Fitted values (trend + AR(1) component)
fitted_values <- fitted(best_model)

# Trend component (regression part only)
trend <- as.numeric(xreg_best %*% best_model$coef[1:3])

# Step 6: Plot the Results
plot(data$time + 2009, data$log_price, 
     type = "p", 
     col = "red", 
     pch = 16, 
     xlab = "Year", 
     ylab = "Log of Inflation-Adjusted Price (2019 USD)", 
     main = paste("Regression with AR(1) Errors: Best t_stop =", round(best_t_stop, 2)),
     ylim = c(-2, 12))
lines(data$time + 2009, fitted_values, col = "blue", lwd = 2)
lines(data$time + 2009, trend, col = "green", lwd = 2, lty = 2)
abline(v = best_t_stop + 2009, col = "black", lty = 3)  # Mark t_stop
legend("topleft", 
       legend = c("Actual Log Price", "Fitted (Trend + AR(1))", "Trend Only", "t_stop"), 
       col = c("red", "blue", "green", "black"), 
       lty = c(NA, 1, 2, 3), 
       pch = c(16, NA, NA, NA), 
       cex = 0.8)
grid()

# Step 7: Plot AIC vs. t_stop
plot(t_stop_range + 2009, aic_values, 
     type = "l", 
     xlab = "t_stop (Year)", 
     ylab = "AIC", 
     main = "AIC vs. t_stop")
abline(v = best_t_stop + 2009, col = "red", lty = 2)
