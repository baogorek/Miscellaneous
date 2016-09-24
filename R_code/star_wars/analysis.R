library(lme4)
library(splines)
library(xtable)
director.movies <- read.csv("C:/Users/User/Documents/statistics/movies.csv")
jj.movies <- director.movies[director.movies$director == "JJ Abrams", ]

#remove not-rated and NC-17 movies
director.movies <- director.movies[director.movies$rating %in% c("PG", "PG-13", "R"), ]
director.movies$scaled.year <- scale(director.movies$year)

movie.full <- glmer(cbind(tomato.fresh, tomato.rotten) ~ 
           (1 + director.writes + director.produces + 
            director.writes:director.produces| director) + 
            ns(scaled.year, df = 2) + rating + 
            director.writes + director.produces + director.writes:director.produces, 
    family = binomial, data = director.movies)
summary(movie.full)

actual.pct.fresh <- with(director.movies, tomato.fresh / (tomato.fresh + tomato.rotten))
pred.pct.fresh <- predict(movie.full, type = "response")
plot(actual.pct.fresh ~ pred.pct.fresh)

# Outliers
director.movies$pred.pct.fresh <- pred.pct.fresh
director.movies$actual.pct.fresh <- actual.pct.fresh
outliers <- director.movies[abs(actual.pct.fresh - pred.pct.fresh) > .3,
                c("director", "movie", "year",
                  "actual.pct.fresh", "pred.pct.fresh")]

table.1 <- xtable(outliers[order(outliers$actual.pct.fresh - outliers$pred.pct.fresh), ])
print(table.1, include.rownames = FALSE, type = "html")

#R-squared like metricb
cor(actual.pct.fresh, pred.pct.fresh)

random.coefs <- coef(movie.full)$director[, c("(Intercept)", "director.writes",
                                              "director.produces", "director.writes:director.produces")]

names(random.coefs)[1] <- "intercept"
names(random.coefs)[4] <- "interaction"
random.coefs$director <- rownames(random.coefs)
rownames(random.coefs) <- NULL

random.coefs <- within(random.coefs, {
  writes.only <- intercept + director.writes
  produces.only <- intercept + director.produces
  writes.and.produces <- intercept + director.writes + director.produces + interaction
})

random.coefs[order(random.coefs$intercept, decreasing = TRUE), c("director", "intercept")]
random.coefs[order(random.coefs$writes.only, decreasing = TRUE), c("director", "writes.only")]
random.coefs[order(random.coefs$produces.only, decreasing = TRUE), c("director", "produces.only")]
random.coefs[order(random.coefs$writes.and.produces, decreasing = TRUE), c("director", "writes.and.produces")]

# Next TODO: predict Star Wars
newdata <- data.frame(director = "JJ Abrams",
                      movie = "Star Wars: The Force Awakens",
                      scaled.year = (2015 - mean(director.movies$year)) / sd(director.movies$year),
                      rating = "PG-13", 
                      director.writes = 1,
                      director.produces = 1)

predict(movie.full, newdata, type = "response")


sw.predict <- function(model) {
  predict(model, newdata, type = "response")
}

bootstrap.results <- bootMer(movie.full, FUN = sw.predict, nsim = 100,
                             .progress = "txt", seed = 14324)

hist(bootstrap.results$t)
mean(bootstrap.results$t) - 2 * sd(bootstrap.results$t) # 40%
mean(bootstrap.results$t) + 2 * sd(bootstrap.results$t) $ 100%

