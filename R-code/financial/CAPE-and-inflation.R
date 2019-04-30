library(rvest)
library(dplyr)
library(ggplot2)
library(ggthemes)

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
prices_url <- "http://www.multpl.com/inflation-adjusted-s-p-500/table/by-year"
table_list <- prices_url %>%
              read_html()  %>%
              html_nodes("table#datatable") %>%
              html_table()

prices_table <- table_list[[1]]
names(prices_table) <- c("date", "price")
prices_table$date <- as.Date(prices_table$date, format = "%B %d, %Y")
prices_table$price <- as.numeric(gsub(",", "", prices_table$price))

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
names(earnings_table) <- c("date", "earnings")
# Adding 2018 final earnings estimate from:
# https://us.spindices.com/documents/additional-material/sp-500-eps-est.xlsx
earnings_table <- rbind(data.frame(date = as.Date("2019-01-01"),
                                   earnings = 157.76), earnings_table)
# Data processing and analysis
data_tbl <- cape_table %>%
  inner_join(t_rate_table) %>%
  inner_join(prices_table) %>%
  inner_join(earnings_table) %>%
  inner_join(inflation_table) %>%
  inner_join(raw_sp500_table) %>%
  mutate(adj_price_in_10yrs = lead(price, n = 10, order = date),
         sp500_price_in_10yrs = lead(raw_sp500, n = 10, order = date)) %>%
  arrange(desc(date))

data_tbl <- data_tbl %>%
  mutate(real_ten_year_return = adj_price_in_10yrs / price - 1.0,
         nom_ten_year_return = sp500_price_in_10yrs / raw_sp500 - 1, 
         real_interest = treasury_rate10 - inflation
)

# TODO: reconcile the 300% number. Is that annualized?

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


