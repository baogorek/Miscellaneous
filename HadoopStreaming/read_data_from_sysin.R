

cat("\nchunk 1\n\n")
# chunk 1
system_in <- file("stdin")
open(system_in)
data <- read.csv(system_in, nrows = 5)
write.csv(data, file = stdout(), row.names = F, quote = F)

cat("\n\nchunk 2\n\n")
# chunk 2
data <- read.csv(system_in, nrows = 5, header = F)
write.table(data, file = stdout(), row.names = F, col.names = F, quote = F)

