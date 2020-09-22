library(dplyr)

require(graphics)
plot(dist ~ speed, data = cars, main = "data(cars)  &  smoothing splines")
cars.spl <- smooth.spline(x=cars$speed, y=cars$dist, all.knots=TRUE)
cars.spl

cars.spl$fit # has knots and coefs in there

knots = cars.spl$fit$knot
coef <- cars.spl$fit$coef
a <- cars.spl$fit$min
b <- cars.spl$fit$min + cars.spl$fit$range

library(splines)
x <- (cars$speed - a) / cars.spl$fit$range
bsplinemat <- bs(cars$speed, knots=unique(cars$speed)[2:18],
                 Boundary.knots = c(4, 25), intercept=TRUE, degree=3)

using_bspline <- bsplinemat %*% coef %>% as.numeric()
using_pred <- predict(cars.spl, x=cars$speed)$y
all(abs(using_bspline - using_pred) < 1E-6) # Woo hoo!


# Prediction on a point that's not in the data
newspeed <- c(10.5, 11.5)
newbsplinemat <- bs(newspeed, knots=unique(cars$speed)[2:18],
                 Boundary.knots = c(4, 25), intercept=TRUE, degree=3)

new_using_bspline <- newbsplinemat %*% coef %>% as.numeric()
newpreds <- predict(cars.spl, x = data.frame(speed = newspeed),
                  deriv = 0)$y

all(abs(new_using_bspline - newpreds) < 1E-6) # Woo hoo!




# Predict on the points.
# Not much to see in here. It's just grabbing x and y from fit
preds <- predict(cars.spl)
points(preds, color='blue', type='l')

# Predict on new points
r = diff(range(cars$speed))

cars.spl$fit$knot

unique(min(cars$speed) + r * cars.spl$fit$knot)
unique(cars$speed)
# Goal: reproduce preds with knots

# here's the function that I'm interested in
#debugging in: predict.smooth.spline.fit(fit, x, deriv, ...)


# Arguments
object <- cars.spl$fit
x <- c(10.5, 11.5)

# The interpolation functionality
xs <- (x - object$min)/object$range # put x's in [0, 1] (like paper)

# For every point, are we extrapolating to the left or right, or interpolating?
extrap.left <- xs < 0
extrap.right <- xs > 1
interp <- !(extrap <- extrap.left | extrap.right)

# Focus on the interpolations
n <- sum(interp)
y <- xs
if (any(interp))
    y[interp] <- .Fortran(C_bvalus, n = as.integer(n), knot = as.double(object$knot),
        coef = as.double(object$coef), nk = as.integer(object$nk),
        x = as.double(xs[interp]), s = double(n), order = as.integer(deriv))$s
#Extrapolations are easy
if (any(extrap)) {
    xrange <- c(object$min, object$min + object$range)
    if (deriv == 0) {
        end.object <- Recall(object, xrange)$y
        end.slopes <- Recall(object, xrange, 1)$y * object$range
        if (any(extrap.left))
            y[extrap.left] <- end.object[1L] + end.slopes[1L] *
              (xs[extrap.left] - 0)
        if (any(extrap.right))
            y[extrap.right] <- end.object[2L] + end.slopes[2L] *
              (xs[extrap.right] - 1)
    }
    else if (deriv == 1) {
        end.slopes <- Recall(object, xrange, 1)$y * object$range
        y[extrap.left] <- end.slopes[1L]
        y[extrap.right] <- end.slopes[2L]
    }
    else y[extrap] <- 0
}
if (deriv > 0)
    y <- y/(object$range^deriv)
list(x = x, y = y)
