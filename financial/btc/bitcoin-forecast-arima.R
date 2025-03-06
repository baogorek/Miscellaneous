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
cpi_base <- cpi_df %>% filter(date == "2025-01-31") %>% pull(cpi)

df <- df %>%
  inner_join(cpi_df, by="date") %>%
  mutate(adj_close = close * (cpi_base / cpi))

df$t <- 1:nrow(df)
df$log_close_orig <- log(df$close)
df$log_close <- log(df$adj_close)

plot(close ~ date, data = df)
lines(adj_close ~ date, data = df, col = "blue")

plot(log_close ~ date, data=df, type = "l")
lines(log_close_orig ~ date, data=df, col="blue")

# Parameter: t_stop
aic_vec <- c()
t_list <- 55:175
for (t_stop in t_list) {
  df$t_before_stop <- ifelse(df$t <= t_stop, df$t, 0)
  df$t2_before_stop <- ifelse(df$t <= t_stop, df$t ^ 2, 0)
  df$plateau <- ifelse(df$t > t_stop, 1, 0)
  
  xreg <- as.matrix(df[, c("t_before_stop", "t2_before_stop", "plateau")])
  
  my_mod <- Arima(
    df$log_close,
    order = c(1, 0, 0),  # AR(1)
    xreg = xreg,
    method = "ML"  # Maximum likelihood for AIC
  )
  aic_vec <- c(aic_vec, my_mod$aic)
  cat("t_stop of", t_stop, "corresponds to", as.character(pull(df[df$t == t_stop, "date"])),
      "has AIC", round(my_mod$aic, 2), "has sigma2", round(my_mod$sigma2, 2),
      "has loglike", round(my_mod$loglik, 2),
      "rho", round(coef(my_mod)["ar1"], 2),
      "plateau", round(coef(my_mod)["plateau"], 2), "theoretical plateau",
      round(coef(my_mod)["t_before_stop"] ^ 2 / (4 * abs(coef(my_mod)["t2_before_stop"])), 2), "\n")
}

plot(aic_vec ~ t_list)

# Final model
t_stop <- 146
df$t_before_stop <- ifelse(df$t <= t_stop, df$t, 0)
df$t2_before_stop <- ifelse(df$t <= t_stop, df$t ^ 2, 0)
df$plateau <- ifelse(df$t > t_stop, 1, 0)

xreg <- as.matrix(df[, c("t_before_stop", "t2_before_stop", "plateau")])

my_mod <- Arima(
  df$log_close,
  order = c(1, 0, 0),  # AR(1)
  xreg = xreg,
  method = "ML"  # Maximum likelihood for AIC
)

pred_obj <- fitted(my_mod, newxreg = xreg)
df$pred <- pred_obj
coef(my_mod)

df$trend <- as.numeric(coef(my_mod)["intercept"] + xreg %*% coef(my_mod)[3:5])

plot(log_close ~ t, data=df, xlim = c(0, 185), ylim = c(-3, 15))
lines(pred ~ t, data = df, col = "red")
lines(trend ~ t, data = df, col = "blue")

plateau_dollars <- exp(coef(my_mod)["intercept"] + coef(my_mod)["plateau"])
round(as.numeric(1 / (1 - abs(coef(my_mod)["ar1"]))), 1)

# Forecast 2 years out
n_steps_future <- 24
xreg_future <- matrix(c(rep(0, n_steps_future * 2), rep(1, n_steps_future)), nrow = n_steps_future, ncol = 3)
colnames(xreg_future) <- c("t_before_stop", "t2_before_stop", "plateau")
xreg_future <- as.matrix(xreg_future)

last_date <- max(df$date)
future_dates <- seq(last_date %m+% months(1), by = "month", length.out = n_steps_future)
future_dates <- ceiling_date(future_dates, "month") - days(1)  # Ensure end-of-month

t_values <- forecast(my_mod, xreg = xreg_future)

png("bitcoin_forecast.png", width = 1600, height = 900, res = 150)
plot(forecast_values, xaxt = "n", yaxt = "n",
     main ="Curling Stone Model of Log Bitcoin Prices",
     ylab = "Bitcoin Prices (Adjusted to Jan 2025 USD)"
)
lines(trend ~ t, data = df, col = "red", lty=1)
formatted_dates <- format(c(df$date, future_dates), "%Y-%m")
tick_positions <- seq(1, length(formatted_dates), by = 3)
axis(1, at = tick_positions, labels = formatted_dates[tick_positions], las = 2)

y_ticks <- c(log(.10), log(1), log(10), log(100), log(1000), log(10000), log(38400), log(100000), log(200000))
y_labels <- c(".10", "1", "10", "100", "1000", "10k", "38.4k", "100k", "200k")
axis(2, at = y_ticks, labels = y_labels, las = 2) 

abline(h = y_ticks, col = "gray80", lty = "dotted")
dev.off()
