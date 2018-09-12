# https://seekingalpha.com/article/4085454-shiller-cape-ratio-misleading-right-now

# https://us.spindices.com/documents/additional-material/sp-500-eps-est.xlsx

library(rvest)
library(ggplot2)
library(ggthemes)

earnings_url <- "http://www.multpl.com/s-p-500-earnings/table"

table_list <- earnings_url %>%
              read_html()  %>%
              html_nodes("table#datatable") %>%
              html_table()

earnings_table <- table_list[[1]]
earnings_table$Date <- as.Date(earnings_table$Date, format = "%B %d, %Y") + 1
names(earnings_table) <- c("date", "earnings")
earnings_table <- earnings_table[earnings_table$date <= "2018-01-01", ]

# Adding 2018 final earnings estimate from:
# https://us.spindices.com/documents/additional-material/sp-500-eps-est.xlsx
# Accessed 9/9/2018
earnings_table <- rbind(data.frame(date = as.Date("2019-01-01"),
                                   earnings = 157.76), earnings_table)

prices_url <- "http://www.multpl.com/inflation-adjusted-s-p-500/table/by-year"
table_list <- prices_url %>%
              read_html()  %>%
              html_nodes("table#datatable") %>%
              html_table()

prices_table <- table_list[[1]]
names(prices_table) <- c("date", "price")
prices_table$date <- as.Date(prices_table$date, format = "%B %d, %Y")
prices_table$price <- as.numeric(gsub(",", "", prices_table$price))
prices_table <- prices_table[prices_table$date <= "2018-01-01", ]

# S&P price as of Sept 7, recording its value for 2019
prices_table <- rbind(data.frame(date = as.Date("2019-01-01"),
                                 price = 2871.68), prices_table)

pe_table <- merge(prices_table, earnings_table, by = "date")
pe_table <- pe_table[order(pe_table$date), ]
pe_table <- within(pe_table, {
  earnings_ma3  <- as.numeric(filter(earnings, rep(1/3, 3), sides = 1))
  earnings_ma5  <- as.numeric(filter(earnings, rep(1/5, 5), sides = 1))
  earnings_ma7  <- as.numeric(filter(earnings, rep(1/7, 7), sides = 1))
  earnings_ma10 <- as.numeric(filter(earnings, rep(1/10, 10), sides = 1))
  price_to_earnings <- price / earnings
  price_to_earnings_ma3 <- price / earnings_ma3
  price_to_earnings_ma5 <- price / earnings_ma5
  price_to_earnings_ma7 <- price / earnings_ma7
  price_to_earnings_ma10 <- price / earnings_ma10
})


pe_table$col <- "black"
pe_table[pe_table$date == "1929-01-01", "col"] <- "red"
pe_table[pe_table$date == "1987-01-01", "col"] <- "red"
pe_table[pe_table$date == "2018-01-01", "col"] <- "blue"
pe_table[pe_table$date == "2019-01-01", "col"] <- "green"

pe_table$size <- 1
pe_table[pe_table$date == "1929-01-01", "size"] <- 2
pe_table[pe_table$date == "1987-01-01", "size"] <- 2 
pe_table[pe_table$date == "2018-01-01", "size"] <- 2 
pe_table[pe_table$date == "2019-01-01", "size"] <- 2 

ggplot(pe_table, aes(x = date, y = price_to_earnings_ma10)) + 
  geom_line(size = .8, color = "grey", linetype = 2) +
  geom_point(colour = pe_table$col, size = pe_table$size) +
  ggtitle("Historical CAPE Ratio of S&P 500, 10-year earnings average") +
  labs(x = "Date", y = "Price to earnings (July 2018 dollars)") +
  annotate("text", x = as.Date('1929-01-01'), y = 28,
           label = "Black Tuesday", size = 2.5, colour = "black") +
  annotate("text", x = as.Date('1987-01-01'), y = 16.5,
           label = "Black Monday", size = 2.5, colour = "black") +
  theme_economist() +
  theme(plot.title      = element_text(size = 18, hjust = 0.5),
        axis.text       = element_text(size = 12), 
        axis.title      = element_text(size = 16))


ggplot(pe_table, aes(x = date, y = price_to_earnings)) + 
  geom_line(size = .8, color = "grey", linetype = 2) +
  geom_point(colour = pe_table$col, size = pe_table$size) +
  ggtitle("Historical Price to Earnings of S&P 500") + 
  labs(x = "Date", y = "Price to earnings (July 2018 dollars)") +
  annotate("text", x = as.Date('1929-01-01'), y = 28,
           label = "Black Tuesday", size = 2.5, colour = "black") +
  annotate("text", x = as.Date('1987-01-01'), y = 16.5,
           label = "Black Monday", size = 2.5, colour = "black") +
  theme_economist() +
  theme(plot.title      = element_text(size = 18, hjust = 0.5),
        axis.text       = element_text(size = 12), 
        axis.title      = element_text(size = 16))

ggplot(pe_table, aes(x = date, y = price_to_earnings_ma7)) + 
  geom_line(size = .8, color = "grey", linetype = 2) +
  geom_point(colour = pe_table$col, size = pe_table$size) +
  ggtitle("Historical CAPE Ratio of S&P 500, 7-year earnings average") + 
  labs(x = "Date", y = "Price to earnings (July 2018 dollars)") +
  annotate("text", x = as.Date('1929-01-01'), y = 28,
           label = "Black Tuesday", size = 2.5, colour = "black") +
  annotate("text", x = as.Date('1987-01-01'), y = 16.5,
           label = "Black Monday", size = 2.5, colour = "black") +
  theme_economist() +
  theme(plot.title      = element_text(size = 18, hjust = 0.5),
        axis.text       = element_text(size = 12), 
        axis.title      = element_text(size = 16))


