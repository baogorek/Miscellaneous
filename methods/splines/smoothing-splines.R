library(splines)
library(dplyr)
#library(splines2)
#https://cran.r-project.org/web/packages/splines2/vignettes/splines2-intro.html


plot(dist ~ speed, data = cars, main = "data(cars)  &  smoothing splines")
cars.spl <- smooth.spline(x=cars$speed, y=cars$dist, all.knots=TRUE)
cars.spl
# Penalized Criterion RSS said to be 4187.776

cars.spl$fit # has knots and coefs in there

knots <- cars.spl$fit$knot

coef <- cars.spl$fit$coef
a <- cars.spl$fit$min
b <- cars.spl$fit$min + cars.spl$fit$range

x <- (cars$speed - a) / cars.spl$fit$range
bsplinemat <- bs(cars$speed, knots=unique(cars$speed)[2:18],
                 Boundary.knots = c(4, 25), intercept=TRUE, degree=3)

bsplinemat2 <- bSpline(cars$speed, knot=unique(cars$speed)[2:18], degree = 3,
		       intercept = TRUE, Boundary.knots = c(4, 25)) # from splines2


matplot(cars$speed, bsplinemat2, type = "l", ylab = "y")

using_bspline <- bsplinemat %*% coef %>% as.numeric()
using_bspline2 <- bsplinemat2 %*% coef %>% as.numeric()
using_pred <- predict(cars.spl, x=cars$speed)$y

all(abs(using_bspline - using_pred) < 1E-6) # Woo hoo!
all(abs(using_bspline - using_bspline2) < 1E-6)

# Prediction on a point that's not in the data
newspeed <- c(10.5, 11.5)
newbsplinemat <- bs(newspeed, knots=unique(cars$speed)[2:18],
                 Boundary.knots = c(4, 25), intercept=TRUE, degree=3)

new_using_bspline <- newbsplinemat %*% coef %>% as.numeric()
newpreds <- predict(cars.spl, x = data.frame(speed = newspeed),
                  deriv = 0)$y

all(abs(new_using_bspline - newpreds) < 1E-6) # Woo hoo!

# RSS seemes a little high
sum((using_pred - cars$dist) ** 2)

# 2nd derivative of bsplines, from splines2
bsdoubleprime <- dbs(cars$speed, derivs=2, knot=unique(cars$speed)[2:18], degree=3,
    intercept = TRUE, Boundary.knots=c(4, 25))
fdoubleprime <- bsdoubleprime %*% coef # This is f"(s) evaluated at speeds

# Calculating the squared second derivative of f with its own resolution
delta <- .1
s <- seq(4, 25, delta)

bsdoubleprime <- dbs(s, derivs=2, knot=unique(cars$speed)[2:18], degree=3,
    intercept = TRUE, Boundary.knots=c(4, 25))


# Trying it out
fdoubleprime <- bsdoubleprime %*% coef # This is f"(s) evaluated at speeds cars$speed, and there are dupes
fdoubleprime_sq <- fdoubleprime ** 2 %>% as.numeric()
plot(fdoubleprime_sq ~ s)
integral2 <- sum(fdoubleprime_sq * delta)
integral2

# Had to dig into the bs function to figure out how to use splineDesign
ord <- 4 # order of a cubic spline
boundary_knots <- c(4, 25)
knots <- unique(cars$speed)[2:18]
Aknots <- sort(c(rep(boundary_knots, ord), knots)) # put the boundary knots back on 4 times!

basis <- splineDesign(Aknots, s, ord, derivs=0)
deriv2basis <- splineDesign(Aknots, s, ord, derivs=2)
fdoubleprime <- deriv2basis %*% coef # This is f"(s) evaluated at speeds
f <- basis %*% coef # This is f(s) evaluated at speeds
integral <- sum(fdoubleprime ** 2 * delta)
integral

plot(f ~ s)
points(cars$dist ~ cars$speed)

# empirical test - not quite right
s_unique <- sort(unique(using_pred))
dist_pred_unique <- predict(cars.spl, x=s_unique)$y
plot(dist_pred_unique)
plot(diff(dist_pred_unique))
plot(diff(diff(dist_pred_unique)))


empirical_deriv <- diff(diff(dist_pred_unique))
plot(fdoubleprime ~ s)
plot(c(NA, NA, empirical_deriv) ~ s_unique)

# empirical integral
sum(empirical_deriv ** 2 * diff(s_unique)[2:18])

# Revisiting the smooth.spline fit
fitted_coefs <- cars.spl$fit$coef

# Once the bs derivatives have been calculated, the pentalty function is easier
penalty <- function(c) {
  # c is a 21-length numeric vector for this specific cars example 
  # bdoubleprime and delta come from the environment (keeping it simple)
  fdoubleprime <- deriv2basis %*% c
  fdoubleprime_sq <- fdoubleprime ** 2 %>% as.numeric()
  sum(fdoubleprime_sq * delta)
}

sse <- function(c) {
  # Sum of squared errors given car speed specific 21-length b-spline coef vector c
  # cars data set assumed to be in the environment
  f <- bsplinemat %*% c %>% as.numeric()
  y <- cars$dist
  sum((y - f) ** 2)
}

lambda <- 1000 # 1000 works pretty well
objective <- function(c) {
   # Now lamda is assumed to be in the environment! (quick and dirty)
   sse(c) + lambda * penalty(c) 
}

sse(coef)
penalty(coef)
objective(coef)

# Now, with the specific lambda in hand, trying to do the penalized optimization
c_starting <- coef + 3 * rnorm(21) # rep(0, 21)
objective(c_starting)

opt <- optim(c_starting, objective, method='L-BFGS-B', control = list(maxit = 1000), lower=0)
coef <- opt$par

plot(coef ~ fitted_coefs, main=paste('correlation: ', round(cor(coef, fitted_coefs), 5)))
abline(a=0, b=1)


f <- basis %*% coef # This is f(s) evaluated at speeds
plot(f ~ s)
points(cars$dist ~ cars$speed)


f_opt <- bsplinemat %*% opt$par
plot(cars$dist ~ cars$speed)
lines(f_opt ~ cars$speed, col='blue')
lines(using_pred ~ cars$speed, col='purple')


