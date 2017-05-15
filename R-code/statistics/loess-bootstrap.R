# Plots bootstrap replicates of a loess fit

library(bootstrap)

data(cholost)
print(cholost)
x <- cholost[ , 1]
y <- cholost[ , 2]

library(modreg)
out <- loess(y ~ x,
		control = loess.control(surface = "direct"))
summary(out)
plot(x, y)
curve(predict(out, newdata = data.frame(x = x)),
		add = TRUE)

n <- length(x)
nboot <- 100
for (i in 1:nboot) {
	k.star <- sample(n, replace = TRUE)
	x.star <- x[k.star]
	y.star <- y[k.star]
	out.star <- loess(y.star ~ x.star,
			control = loess.control(surface = "direct"))
	curve(predict(out.star, data.frame(x.star = x)),
			add = TRUE, col = "turquoise")
}
points(x, y)
curve(predict(out, newdata = data.frame(x = x)),
		add = TRUE, lwd = 2)
cat("Calculation took", proc.time()[1], "seconds\n")
