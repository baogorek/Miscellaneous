# TODO: Add comment
# 
# Author: OGOREKB
###############################################################################


##Tip 1: When possible, use R''s built in math functions!
## For example: use %*% to multiply matrices, rather than your own code!!

erlangsB <- function(c,r) {dpois(c,r)/ppois(c,r)}
erlangsB(7,3)

## Tip 2 : When storing lookup tables, consider implementing the table using an environment
### First, a convenient but naive way:
	b <- c(Joe = 1, Bob = 2, Jim = 3)
	b["Bob"]
	names(b)
	
	e <- new.env(hash=TRUE,size=3) #environments are implemented using hash tables, unlike vectors and lists
	assign(x = "Joe", value = 1, envir = e)
	assign(x = "Bob", value = 2, envir = e)
	assign(x = "Jim", value = 3, envir = e)
	
	get("Bob", envir = e)
	e[["Bob"]] # '[[' is an operator
	
## Tip 3: Preallocation of memory for Speed!

v <- c()
before <- proc.time()
for(i in 1:100000){v[i] <- i;}
proc.time() - before

v2 <- c(NA)
length(v2) <- 500000
before <- proc.time()
for (i in 1:100000){v2[i] <-i;}
proc.time() - before

# Tip 4: Garbage Collection
gc()

# Tip 5: Monitor Memory
object.size("Hello World!")
memory.profile()
memory.size()
memory.limit()