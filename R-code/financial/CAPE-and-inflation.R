# Paper about cape:
# https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3258404

# Why am I getting this data from multipl.com when Shiller is updating it
# himself? It's here: http://www.econ.yale.edu/~shiller/data.htm



library(rvest)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(lubridate)
library(RcppRoll)

# CAPE data ------------------------------------------------------------
cape_url <- "http://www.multpl.com/shiller-pe/table"

table_list <- cape_url %>%
              read_html()  %>%
              html_nodes("table#datatable") %>%
              html_table()

cape_table <- table_list[[1]]
cape_table$Date <- as.Date(cape_table$Date, format = "%B %d, %Y")
names(cape_table) <- c("date", "cape")

# %TODO: these are not real interest rates
# For constructing long-run real interest rates:
# https://www.minneapolisfed.org/research/economic-policy-papers/real-interest-rates-over-the-long-run
# 10 year treasury rate data -------------------------------------------
treasury_rate10_url <- "http://www.multpl.com/10-year-treasury-rate/table"

table_list <- treasury_rate10_url %>%
              read_html()  %>%
              html_nodes("table#datatable") %>%
              html_table()

t_rate_table <- table_list[[1]]
t_rate_table$Date <- as.Date(t_rate_table$Date, format = "%B %d, %Y")
names(t_rate_table) <- c("date", "treasury_rate10")
t_rate_table$treasury_rate10 <- as.numeric(gsub("%", "",
                                           t_rate_table$treasury_rate10))

# Inflation rate
inflation_url <- "http://www.multpl.com/inflation/table"
table_list <- inflation_url %>%
              read_html()  %>%
              html_nodes("table#datatable") %>%
              html_table()

inflation_table <- table_list[[1]]
inflation_table$Date <- as.Date(inflation_table$Date, format = "%B %d, %Y")
names(inflation_table) <- c("date", "inflation")
inflation_table$inflation <- as.numeric(gsub("%", "",
                                           inflation_table$inflation))

# Inflation adjusted S&P price data -----------------------------------
adj_sp500_u <- "http://www.multpl.com/inflation-adjusted-s-p-500/table/by-year"
table_list <- adj_sp500_u %>%
              read_html()  %>%
              html_nodes("table#datatable") %>%
              html_table()

adj_sp500_tbl <- table_list[[1]]
names(adj_sp500_tbl) <- c("date", "adj_sp500")
adj_sp500_tbl$date <- as.Date(adj_sp500_tbl$date, format = "%B %d, %Y")
adj_sp500_tbl$adj_sp500 <- as.numeric(gsub(",", "", adj_sp500_tbl$adj_sp500))

# Raw S&P Prices table
raw_sp500_url <- "http://www.multpl.com/s-p-500-historical-prices/table/by-year"
table_list <- raw_sp500_url %>%
              read_html()  %>%
              html_nodes("table#datatable") %>%
              html_table()

raw_sp500_table <- table_list[[1]]
names(raw_sp500_table) <- c("date", "raw_sp500")
raw_sp500_table$date <- as.Date(raw_sp500_table$date, format = "%B %d, %Y")
raw_sp500_table$raw_sp500 <- as.numeric(gsub(",", "",
                                             raw_sp500_table$raw_sp500))

# Inflation adjusted S&P earnings data ---------------------------------
earnings_url <- "http://www.multpl.com/s-p-500-earnings/table"

table_list <- earnings_url %>%
              read_html()  %>%
              html_nodes("table#datatable") %>%
              html_table()

earnings_table <- table_list[[1]]
earnings_table$Date <- as.Date(earnings_table$Date, format = "%B %d, %Y") + 1
names(earnings_table) <- c("date", "adj_earnings")
# Adding 2018 final earnings estimate from:
# https://us.spindices.com/documents/additional-material/sp-500-eps-est.xlsx
earnings_table <- rbind(data.frame(date = as.Date("2019-01-01"),
                                   adj_earnings = 157.76), earnings_table)

# S&P Real Dividends
dividends_url <- "http://www.multpl.com/s-p-500-dividend/table"
table_list <- dividends_url %>%
              read_html()  %>%
              html_nodes("table#datatable") %>%
              html_table()

dividends_table <- table_list[[1]]
# Note: The dividends are reported at end of year. +1 sends to next year
dividends_table$Date <- as.Date(dividends_table$Date, format = "%B %d, %Y") + 1
names(dividends_table) <- c("date", "lag_adj_div")

# Data processing and analysis
data_tbl <- cape_table %>%
  inner_join(t_rate_table) %>%
  inner_join(adj_sp500_tbl) %>%
  inner_join(earnings_table) %>%
  inner_join(inflation_table) %>%
  inner_join(raw_sp500_table) %>%
  inner_join(dividends_table) %>%
  mutate(lag_adj_sp500 = lag(adj_sp500, n = 1, order_by = date)) %>%
  arrange(date)

data_tbl$year_return <- with(data_tbl,
  (adj_sp500 + lag_adj_div) / lag_adj_sp500)

data_tbl <- data_tbl %>%
  mutate(invest_jan_of_year = year(date) - 1,
         adj_10yr_ret = 100 * exp(roll_mean(log(year_return), n = 10,
                                            na.rm = FALSE, align = "left",
                                            fill = NA)) - 100,
         adj_10yr_earn = roll_mean(adj_earnings, n = 10, na.rm = FALSE,
                                   align = "right", fill = NA),
         real_interest = treasury_rate10 - inflation) %>%
  arrange(desc(date))
data_tbl$cape2 <- data_tbl$adj_sp500 / data_tbl$adj_10yr_earn

# Did a manual spot check for 3 period moving average. A todo would be to
#  get this into a function and test against a known standard

# Figure 1.1 Irrational Exhuberance
ggplot(data_tbl, aes(x = year)) +
  geom_line(aes(y = 5 * adj_earnings, color = "adj earnings x5")) +
  geom_line(aes(y = adj_sp500, color = "adj sp500")) +
  scale_x_continuous(name = "Year", limit = c(1860, 2020),
                     breaks = seq(1860, 2020, 20)) +
  scale_y_continuous(name = "Value", limit = c(0, 3000),
                     breaks = seq(0, 3000, 200)) +
  scale_color_manual(name = "Series",
                     values = c("adj earnings x5" = "green",
                                "adj sp500" = "red"))

# Figure 1.2 IE
ggplot(data_tbl, aes(x = year, y = cape)) +
  geom_line() +
  scale_x_continuous(name = "Year", limit = c(1860, 2020),
                     breaks = seq(1860, 2020, 20)) +
  scale_y_continuous(name = "Value", limit = c(0, 45),
                     breaks = seq(0, 45, 5))

# Figure 1.3 IE
# Annualized 10-year returns
  # For January of the year indicated
  # Geometric average real annual return per year, reinvesting dividends
# 18xx gets asterics
cent_prefix <- ifelse(substr(data_tbl$invest_jan_of_year, 1, 2) == "18",
                             "*", "")
data_tbl$display_year <- paste0(cent_prefix,
                                substr(data_tbl$invest_jan_of_year, 3, 4))

n <- nrow(data_tbl)
#TODO: figure out how I got off by one with CAPE measurements
data_tbl$cape2 <- c(data_tbl$cape[-1], NA)

ggplot(data_tbl, aes(x = cape2, y = adj_10yr_ret)) +
  #geom_point() + keep alive just to calibrate it with hjust, vjust
  geom_text(aes(label = display_year), hjust = .5, vjust = .35, size = 2.5) + 
  scale_x_continuous(name = "Price-earnings", limit = c(5, 30),
                     breaks = seq(5, 30, 5)) +
  scale_y_continuous(name = "Ten year Return", limit = c(-5, 20),
                     breaks = seq(-5, 20, 5))



# Quick test for return calculation
library(RcppRoll)
test_tbl <- data.frame(year = c(1, 2, 3, 4), price = c(100, 120, 114, 116))
test_tbl <- test_tbl %>%
  mutate(rel_return = price / (lag(price, n = 1, order_by = year)))

stopifnot(all(test_tbl$rel_return[2:3] == c(1.20, .95)))

test_tbl <- test_tbl %>%
  mutate(two_year_return = exp(roll_mean(log(rel_return), n = 2, na.rm = TRUE,
                                     align = "right", fill = NA)))




# What is going on? Need a better measure of short term real interest
plot(real_interest ~ date, data = data_tbl)
abline(h = 0)

# remember, these are inflation adjusted
plot(adj_ten_year_return ~ date, data = data_tbl, type = "b")
abline(h = 0)

plot(nom_ten_year_return ~ date, data = data_tbl, type = "b")
abline(h = 0)


plot(ten_year_return ~ cape, data = data_tbl)

cape_lm <- lm(ten_year_return ~ cape + real_interest, data = data_tbl)
summary(cape_lm)

data_tbl$cape_resid <- c(rep(NA, 10), resid(cape_lm))
plot(cape_resid ~ date, data = data_tbl)
abline(h = 0)


