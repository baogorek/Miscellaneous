#lot shading demo
set.seed(7100)
draws <- rnorm(100)^2
dens <- density(draws)
plot(dens, main="Shading Areas Under a Curve")


x.lower.index <- min(which(dens$x >= 1.5))  
x.upper.index <- max(which(dens$x <  3))


polygon(x = dens$x[c(x.lower.index,x.lower.index:x.upper.index,x.upper.index)], 
              y = c(0, dens$y[x.lower.index:x.upper.index], 0), col="gray")
# See how x1 maps to 0, x1:x1 maps to y[x1:x2], and x2 maps to 0


> plot(1~1, xlim = c(-3,3), ylim = c(-3,3))
> polygon(x=c(1,1,2,2), y = c(2,1,1,2))

library('ggplot2')
set.seed(7100)
draws <- rnorm(100)^2
dens <- density(draws)
dd <- with(dens,data.frame(x,y))
library(ggplot2)
qplot(x,y,data=dd,geom="line", asp=1)+
geom_ribbon(data=subset(dd,x>quantile(draws, .75) & x<quantile(draws, .95)),aes(ymax=y),ymin=0,
fill="blue",colour=NA,alpha=0.75)
