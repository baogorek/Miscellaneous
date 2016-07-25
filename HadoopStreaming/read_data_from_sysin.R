
system_in <- file("stdin")
open(system_in)
data <- read.csv(system_in, nrows = 5)
print(data)
