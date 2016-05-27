read.delim("Data/fiveRows.txt", header = F)
five <- read.delim("Data/fiveRows.txt", header = F)
names(five) <- c("x", "z", "y")
five <- read.delim("Data/fiveRows.txt", header = F, delim = " ")
five <- read.delim("Data/fiveRows.txt", header = F, sep = " ")
names(five) <- c("x", "z", "y")
lm(y ~ x + z, data = five)

