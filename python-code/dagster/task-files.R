#!/opt/homebrew/bin/Rscript

print("This is an R script that needs arrow input file `my_df.csv` to run")

my_df <- read.csv('my_df.csv')
my_df$z <- c(9, 10, 11)

print(ls())

write.csv(my_df, "my_df2.csv", row.names=FALSE)
