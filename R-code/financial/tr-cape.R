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
# Sampling the January months
cape_yearly <- cape_monthly %>%
  filter(round(Date %% 1, 2) == .01) %>%
  mutate(snap_jan_yr = round(Date))

cape_yearly <- cape_yearly %>%
  mutate(display_year = paste0(ifelse(substr(snap_jan_yr, 1, 2) == "18",
                                      "*", ""),
                               substr(snap_jan_yr, 3, 4)))
# %>%
#  select(snap_jan_yr, display_year, cpi, interest_rate10, raw_sp500, real_price,
#         real_earnings, cape, tr_cape, ten_yr_return, lag_tr_cape_1mo,
#         lag_cape_1mo)

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
ggplot(cape_yearly, aes(x = cape, y = ten_yr_return)) +
  #geom_point() + keep alive just to calibrate it with hjust, vjust
  geom_text(aes(label = display_year), hjust = .5, vjust = .35, size = 2.5) +
  scale_x_continuous(name = "CAPE", limit = c(5, 45),
                     breaks = seq(5, 45, 5)) +
  scale_y_continuous(name = "Ten year Return", limit = c(-5, 20),
                     breaks = seq(-5, 20, 5))

ggplot(cape_yearly, aes(x = tr_cape, y = ten_yr_return)) +
  #geom_point() + keep alive just to calibrate it with hjust, vjust
  geom_text(aes(label = display_year), hjust = .5, vjust = .35, size = 2.5) +
  scale_x_continuous(name = "TR CAPE", limit = c(5, 50),
                     breaks = seq(5, 50, 5)) +
  scale_y_continuous(name = "Ten year Return", limit = c(-5, 20),
                     breaks = seq(-5, 20, 5))

# For reference:
sum(is.na(cape_yearly$cape) | is.na(cape_yearly$ten_yr_return))

