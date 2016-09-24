library(rpart)
library(survival)

my_data <- read.delim("/home/user/eclipse_space/Social_Media/con_set.txt")

my_data <- my_data[my_data$start_month != -999,]
my_data$months_in <- (2013 - my_data$start_year)*12 - (my_data$start_month - 1)

hist(my_data$n_tokens)

censoring_info <- rep(0, nrow(my_data)) # all right censored
my.surv.object <- Surv(time=my_data$months_in, event = censoring_info)
my.surv.object

predictors <- names(my_data)[5:17]
predictors

ph.fit <- survreg(my.surv.object ~ my_data$lexical_diversity, dist='exponential')
#not going to happen

fit <- rpart(months_in ~ connection_ct +  n_tokens + market + manag + develop + busi + data +
               custom + num + analyt + model + team, 
               data = my_data[my_data$n_tokens>0,],   
               method="anova")
printcp(fit) # display the results 

fit <- rpart(months_in ~ connection_ct , data = my_data, method="anova")
printcp(fit)

fit2 <- prune(fit, cp=.02)
printcp(fit2)


pdf("/home/user/eclipse_space/Social_Media/tree.pdf")
plot(fit, uniform=TRUE, 
     main="Exploratory Regression Tree for Mileage")
text(fit, use.n=TRUE, all=TRUE, cex=.5)
dev.off()

library(ggplot2)
qplot(data = my_data, connection_ct, months_in)
qplot(data = my_data, n_tokens, months_in)
my.lm <- lm(months_in ~ n_tokens + connection_ct + (connection_ct==500), data = my_data)
summary(my.lm)

qplot(data = my_data[my_data$n_tokens > 50,], lexical_diversity, months_in)


z <- cor(my_data[, c("market", "manag", "develop", "busi", "data", "custom", "num", "analyt", "model", "team")])
require(lattice)
my.palette <- colorRampPalette(c("blue", "red"))
levelplot(z, col.regions = my.palette(100))

head(my_data[,-c(1,2)])
table(my_data$lexical_diversity > 0.0 & my_data$lexical_diversity < 1.0)