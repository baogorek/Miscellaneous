#!/opt/homebrew/bin/Rscript

library(arrow)
library(reticulate)

client <- flight_connect(port = 8089)
my_df <- flight_get(client, path = "test_data/my_df")

print("This is an R script that needs arrow input `my_df` to run")

my_df <- as.data.frame(my_df)  # arrow data table to R data frame
my_df$z <- c(9, 10, 11)

print(ls())

flight_put(client, my_df, path = "test_data/my_df2")
