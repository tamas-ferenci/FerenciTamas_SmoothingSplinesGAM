## A LOESS simítóról lesz szó


## ------------------------------------------------------------------------------------------------------------------------------------
library(ggplot2)
set.seed(1)


## ------------------------------------------------------------------------------------------------------------------------------------
n <- 101
x <- (1:n) + rnorm(n, 0, 0.1)
y <- sin(x/n*(2*pi))
yobs <- y + rnorm(n, 0, 0.2)
SimData <- data.frame(x, y, yobs)
p <- ggplot(SimData, aes(x = x, y = yobs)) + geom_point() +
  geom_line(aes(y = y), color = "orange", lwd = 1)
p


## ------------------------------------------------------------------------------------------------------------------------------------
p + geom_smooth(formula = y~x, method = "lm", se = FALSE)


## ------------------------------------------------------------------------------------------------------------------------------------
p + geom_vline(xintercept = 23.5, color = "red")


## ------------------------------------------------------------------------------------------------------------------------------------
span <- 0.75
n*span
ceiling(n*span)
sort(abs(x-23.5))
sort(abs(x-23.5))[ceiling(n*span)]


## ------------------------------------------------------------------------------------------------------------------------------------
tricube <- function(u, t) ifelse(u<t, (1-(u/t)^3)^3, 0)
curve(tricube(x, 2), to = 3)


## ------------------------------------------------------------------------------------------------------------------------------------
SimData$w <- tricube(abs(x-23.5), sort(abs(x-23.5))[ceiling(n*span)])
ggplot(SimData, aes(x = x, y = yobs, color = w>0)) + geom_point()


## ------------------------------------------------------------------------------------------------------------------------------------
ggplot(SimData, aes(x = x, y = yobs, color = w)) + geom_point()


## ------------------------------------------------------------------------------------------------------------------------------------
fit <- lm(yobs ~ x, weights = w, data = SimData)
p + geom_vline(xintercept = 23.5, color = "red") +
  geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2])


## ------------------------------------------------------------------------------------------------------------------------------------
p + geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2]) +
  geom_vline(xintercept = 23.5, color = "red") +
  geom_point(x = 23.5, y = predict(fit, data.frame(x = 23.5)), color="red")


## ------------------------------------------------------------------------------------------------------------------------------------
loessfun <- function(xin, x, yobs, span) {
  n <- length(x)
  w <- tricube(abs(x-xin), sort(abs(x-xin))[ceiling(n*span)])
  fit <- lm(yobs ~ x, weights = w)
  predict(fit, data.frame(x = xin))
}
p + geom_vline(xintercept = 23.5, color = "red") +
  geom_point(x = 23.5, y = loessfun(23.5, SimData$x, SimData$yobs, 0.75), color="red")


## ------------------------------------------------------------------------------------------------------------------------------------
p + geom_vline(xintercept = 48.3, color = "red") +
  geom_point(x = 48.3, y = loessfun(48.3, SimData$x, SimData$yobs, 0.75), color="red")
p + geom_vline(xintercept = 91.2, color = "red") +
  geom_point(x = 91.2, y = loessfun(91.2, SimData$x, SimData$yobs, 0.75), color="red")


## ------------------------------------------------------------------------------------------------------------------------------------
SmoothData <- expand.grid(grid = seq(0, 101, 0.1))
SmoothData$value <- apply(SmoothData, 1, function(x)
  loessfun(x["grid"], SimData$x, SimData$yobs, 0.75))
p + geom_line(data = SmoothData, aes(x = grid, y = value), color = "red")


## ------------------------------------------------------------------------------------------------------------------------------------
SmoothData <- expand.grid(grid = seq(0, 101, 0.1),
                          span = c(2/n+1e-10, 0.25, 0.5, 0.75, 1))
SmoothData$value <- apply(SmoothData, 1, function(x)
  loessfun(x["grid"], SimData$x, SimData$yobs, x["span"]))
SmoothData$span <- as.factor(SmoothData$span)

p + geom_line(data = SmoothData[SmoothData$span%in%c(0.25, 0.5, 0.75), ],
              aes(x = grid, y = value, color = span))


## ------------------------------------------------------------------------------------------------------------------------------------
p + geom_line(data = SmoothData[SmoothData$span==1, ], aes(x = grid, y = value),
              color = "red")


## ------------------------------------------------------------------------------------------------------------------------------------
p + geom_line(data = SmoothData[SmoothData$span==2/n+1e-10, ], aes(x = grid, y = value),
              color = "red")


## ------------------------------------------------------------------------------------------------------------------------------------
lm(y~x+x^2, data = SimData)


## ------------------------------------------------------------------------------------------------------------------------------------
lm(y~x+I(x^2), data = SimData)


## ------------------------------------------------------------------------------------------------------------------------------------
lm(y~poly(x,2), data = SimData)


## ------------------------------------------------------------------------------------------------------------------------------------
predict(lm(y~x+I(x^2)), data.frame(x = 43.9))
predict(lm(y~poly(x,2)), data.frame(x = 43.9))


## ------------------------------------------------------------------------------------------------------------------------------------
t(cbind(1, poly(x, 3)))%*%cbind(1, poly(x, 3))


## ------------------------------------------------------------------------------------------------------------------------------------
lm(y~poly(x,2, raw = TRUE), data = SimData)


## ------------------------------------------------------------------------------------------------------------------------------------
kappa(cbind(1, poly(x, 3)), exact = TRUE)
kappa(cbind(1, poly(x, 3, raw = TRUE)), exact = TRUE)


## ------------------------------------------------------------------------------------------------------------------------------------
loessfun <- function(xin, x, yobs, span, degree) {
  n <- length(x)
  w <- tricube(abs(x-xin), sort(abs(x-xin))[ceiling(n*span)])
  fit <- lm(yobs ~ poly(x, degree), weights = w)
  predict(fit, data.frame(x = xin))
}


## ------------------------------------------------------------------------------------------------------------------------------------
SmoothData <- rbind(expand.grid(grid = seq(0, 101, 0.1),
                                span = c(2/n+1e-10, 0.25, 0.5, 0.75, 1), degree = 1),
                    expand.grid(grid = seq(0, 101, 0.1),
                                span = c(3/n+1e-10, 0.25, 0.5, 0.75, 1), degree = 2))
SmoothData$value <- apply(SmoothData, 1, function(x)
  loessfun(x["grid"], SimData$x, SimData$yobs, x["span"], x["degree"]))
SmoothData$span <- as.factor(SmoothData$span)
p + geom_line(data = SmoothData[SmoothData$span%in%c(0.25, 0.5, 0.75), ],
              aes(x = grid, y = value, color = span)) + facet_grid(rows = vars(degree))


## ------------------------------------------------------------------------------------------------------------------------------------
SimData$train <- FALSE
SimData$train[sample(1:101, 80)] <- TRUE
ggplot(SimData, aes(x = x, y = yobs, color = train)) + geom_point() +
  geom_line(aes(y = y), color = "orange", lwd = 1)


## ------------------------------------------------------------------------------------------------------------------------------------
spans <- seq(0.03, 0.99, 0.01)
SSEfull <- sapply(spans, function(sp) sum((sapply(1:101, function(i)
  loessfun(SimData$x[i], SimData$x, SimData$yobs, sp, 1))-yobs)^2))
ggplot(data.frame(span = spans, SSE = SSEfull), aes(x = span, y = SSE)) + geom_line()


## ------------------------------------------------------------------------------------------------------------------------------------
SSEfull <- sapply(spans, function(sp) sum((sapply(which(!SimData$train), function(i)
  loessfun(SimData$x[i], SimData$x[SimData$train==TRUE],
           SimData$yobs[SimData$train==TRUE], sp, 1))-yobs[SimData$train==FALSE])^2))
ggplot(data.frame(span = spans, SSE = SSEfull), aes(x = span, y = SSE)) + geom_line()


## ------------------------------------------------------------------------------------------------------------------------------------
spans[which.min(SSEfull)]


## ------------------------------------------------------------------------------------------------------------------------------------
p + geom_line(data = SmoothData[SmoothData$span==spans[which.min(SSEfull)], ],
              aes(x = grid, y = value), color = "red")

