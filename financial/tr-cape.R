library(dplyr)
library(tidyverse)
library(RcppRoll)

working_dir <- "c:/devl/Miscellaneous/R-code/financial"
cape_monthly <- read_csv(file.path(working_dir, "shiller-trcape.csv"))

# Data dictionary
#
# raw_sp500: spot price of s&p 500 at the end of the month
# yearly_div: is presented as an annual rate, but it is interpolated monthly
#     from quarter to quarter 

# Calculate 10 year returns
# Interpretation is going to be returns on Total Returns series if entering the
#   market at the last day of the previous month.
# Note: it's crucial not to confuse price snapshots with flow quantities, which
#       is why calculating the return at the monthly bucket is so important

cape_monthly <- cape_monthly %>%
  mutate(lag_real_price_1mo = lag(real_price, n = 1, order_by = Date),
         lag_cape_1mo = lag(cape, n = 1, order_by = Date),
         lag_tr_cape_1mo = lag(tr_cape, n = 1, order_by = Date))

cape_monthly <- cape_monthly %>%
  mutate(tr_multiplier = (real_price + real_div / 12) / lag_real_price_1mo)

cape_monthly <- cape_monthly %>%
  arrange(Date) %>%
  mutate(ten_yr_return = 100 * exp(roll_mean(log(tr_multiplier), n = 120,
                                             na.rm = FALSE, align = "left",
                                             fill = NA)) ^ 12 - 100)

# Creating a Lead return which effectively lags cape and TR-Cape 
cape_monthly <- cape_monthly %>%
  mutate(lead_10yr_ret = lead(ten_yr_return, n = 1, order_by = Date))

# Sampling the January months
cape_yearly <- cape_monthly %>%
  filter(round(Date %% 1, 2) == .01) %>%
  mutate(snap_jan_yr = round(Date))

add_century_symbol <- function(year) {
  symbol <- ""
  if(substr(year, 1, 2) == "18") {
    symbol <- "*"
  } else if (substr(year, 1, 2) == "20") {
    symbol <- "\u2020"
  }
  paste0(symbol, substr(year, 3, 4))
}

cape_yearly$display_year <- sapply(1:nrow(cape_yearly),
  function(i) add_century_symbol(cape_yearly$snap_jan_yr[i]))
                       
# Figure 1.1 Irrational Exhuberance
ggplot(cape_yearly, aes(x = snap_jan_yr)) +
  geom_line(aes(y = 5 * real_earnings, color = "Real earnings x5")) +
  geom_line(aes(y = real_price, color = "Real S&P 500 price")) +
  scale_x_continuous(name = "Year", limit = c(1860, 2020),
                     breaks = seq(1860, 2020, 20)) +
  scale_y_continuous(name = "Value", limit = c(0, 3000),
                     breaks = seq(0, 3000, 200)) +
  scale_color_manual(name = "Series",
                     values = c("Real earnings x5" = "green",
                                "Real S&P 500 price" = "red"))

# Figure 1.2 Irrational Exuberance
ggplot(cape_yearly, aes(x = snap_jan_yr, y = cape)) +
  geom_line() +
  scale_x_continuous(name = "Year", limit = c(1860, 2020),
                     breaks = seq(1860, 2020, 20)) +
  scale_y_continuous(name = "Value", limit = c(0, 45),
                     breaks = seq(0, 45, 5))

# Figure 1.3 Irrational Exuberance
ggplot(cape_yearly, aes(x = cape, y = lead_10yr_ret)) +
  #geom_point() + keep alive just to calibrate it with hjust, vjust
  geom_text(aes(label = display_year), hjust = .5, vjust = .35, size = 2.5) +
  scale_x_continuous(name = "CAPE", limit = c(5, 45),
                     breaks = seq(5, 45, 5)) +
  scale_y_continuous(name = "Ten year Return", limit = c(-5, 20),
                     breaks = seq(-5, 20, 5))

ggplot(cape_yearly, aes(x = tr_cape, y = lead_10yr_ret)) +
  #geom_point() + keep alive just to calibrate it with hjust, vjust
  geom_text(aes(label = display_year), hjust = .5, vjust = .35, size = 2.5) +
  scale_x_continuous(name = "TR-CAPE", limit = c(5, 50),
                     breaks = seq(5, 50, 5)) +
  scale_y_continuous(name = "Ten year Return", limit = c(-5, 20),
                     breaks = seq(-5, 20, 5)) +
  ggtitle("Ten year return of S&P against TR-CAPE month before investing")

# For reference:
sum(is.na(cape_yearly$cape) | is.na(cape_yearly$ten_yr_return))


library(MASS)

#cape_yearly2 <- cape_yearly
cape_yearly <- cape_yearly2
cape_yearly <- cape_yearly %>% filter(!(snap_jan_yr %in% c(1999, 2000, 2001)))
cape_yearly <- cape_yearly %>%
  add_row(display_year = "yy", tr_cape = 35, lead_10yr_ret = -10) %>%
  add_row(display_year = "zz", tr_cape = 33, lead_10yr_ret = -10)


my_rlm <- rlm(lead_10yr_ret ~ tr_cape, data = cape_yearly)
my_lm <- lm(lead_10yr_ret ~ tr_cape, data = cape_yearly)

cape_yearly$pred_return_rlm <- predict(my_rlm, newdata = cape_yearly)
cape_yearly$pred_return_lm <- predict(my_lm, newdata = cape_yearly)

# Augmented TR-CAPE version of Figure 1.3

ggplot(cape_yearly, aes(x = tr_cape, y = lead_10yr_ret)) +
  #geom_point() + keep alive just to calibrate it with hjust, vjust
  geom_text(aes(label = display_year), hjust = .5, vjust = .35, size = 2.5) +
  geom_line(aes(y = pred_return_rlm, color = "Robust Regression")) + 
  geom_line(aes(y = pred_return_lm, color = "OLS Regression")) + 
  scale_x_continuous(name = "TR-CAPE", limit = c(5, 50),
                     breaks = seq(5, 50, 5)) +
  scale_y_continuous(name = "Ten year Return", limit = c(-11, 20),
                     breaks = seq(-10, 20, 5)) +
  scale_color_manual(name = "Series",
                     values = c("Robust Regression" = "blue",
                                "OLS Regression" = "red")) + 
  ggtitle("Ten year return of S&P against TR-CAPE month before investing")

my_rlm2 <- rlm(lead_10yr_ret ~ tr_cape
              + I((snap_jan_yr - 1950) / 10),
              ,data = cape_yearly)
summary(my_rlm2)

# Time Series view

cape_yearly <- cape_yearly2
complete_df <- cape_yearly[complete.cases(cape_yearly), ]
complete_df$log_tr_cape <- log(complete_df$tr_cape)
library(forecast)

library(nlme)
my_gls <- gls(lead_10yr_ret ~ log_tr_cape,# + I((snap_jan_yr - 1950) / 10),
              correlation = corARMA(p = 1, q = 0),
              data = complete_df)
summary(my_gls)


complete_df$pred_return_gls <- predict(my_gls, newdata = complete_df)
ggplot(complete_df, aes(x = log_tr_cape, y = lead_10yr_ret)) +
  #geom_point() + keep alive just to calibrate it with hjust, vjust
  geom_text(aes(label = display_year), hjust = .5, vjust = .35, size = 2.5) +
  geom_line(aes(y = pred_return_gls, color = "GLS Regression")) + 
 # geom_line(aes(y = pred_return_lm, color = "OLS Regression")) + 
  scale_x_continuous(name = "log TR-CAPE", limit = c(1.8, 4.5),
                     breaks = seq(2, 4, 1)) +
  scale_y_continuous(name = "Ten year Return", limit = c(-5, 20),
                     breaks = seq(-5, 20, 5)) +
  scale_color_manual(name = "Series",
                     values = c("GLS Regression" = "blue",
                                "OLS Regression" = "red")) + 
  ggtitle("Ten year return of S&P against TR-CAPE month before investing")






# I used this to plot the line moving up and down with time.
ggplot(cape_yearly, aes(x = tr_cape, y = lead_10yr_ret)) +
  #geom_point() + keep alive just to calibrate it with hjust, vjust
  geom_text(aes(label = display_year), hjust = .5, vjust = .35, size = 2.5) +
  geom_line(aes(y = pred_return_rlm2, color = "Robust Regression")) + 
  scale_x_continuous(name = "TR-CAPE", limit = c(5, 50),
                     breaks = seq(5, 50, 5)) +
  scale_y_continuous(name = "Ten year Return", limit = c(-5, 20),
                     breaks = seq(-5, 20, 5)) +
  scale_color_manual(name = "Series",
                     values = c("Robust Regression" = "blue",
                                "OLS Regression" = "red")) + 
  ggtitle("Ten year return of S&P against TR-CAPE with simple time trend")




cape_yearly$return_resid <- cape_yearly$lead_10yr_ret - cape_yearly$pred_return

ggplot(cape_yearly, aes(x = snap_jan_yr, y = return_resid)) +
  geom_text(aes(label = display_year), hjust = .5, vjust = .35, size = 2.5) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed", col = "red") + 
  scale_x_continuous(name = "Year invested end of Jan", limit = c(1860, 2020),
                     breaks = seq(1860, 2020, 20)) +
  scale_y_continuous(name = "10 year return residual", limit = c(-15, 10),
                     breaks = seq(-15, 10, 5)) +
  ggtitle("TR-CAPE Residuals over time")

ggplot(cape_yearly, aes(x = snap_jan_yr)) +
  geom_text(aes(y = lead_10yr_ret, label = display_year),
            hjust = .5, vjust = .35, size = 2.5, col = "black") +
  geom_point(aes(y = pred_return), col = "blue") + 
  geom_line(aes(y = pred_return), col = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", col = "red") + 
  scale_x_continuous(name = "Year invested end of Jan", limit = c(1860, 2020),
                     breaks = seq(1860, 2020, 20)) +
  scale_y_continuous(name = "10 year return TR-CAPE predictions (blue) Actuals (text)",
                     limit = c(-15, 20),
                     breaks = seq(-15, 20, 5)) +
  ggtitle("S&P500 returns: Actual (text) and predicted (blue) via TR-CAPE Regression")


