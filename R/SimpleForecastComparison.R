
#' @importFrom graphics par
SimpleForecastComparison <- function(theta0 = 2, theta1 = 1, n = 20, Zn = 50, sigma2a = 1, steps.ahead = 10) {
    # compare forecasts of four different simple ARIMA models
    old.par <- par(mfrow = c(2, 2))
    x <- 1:steps.ahead
    # trivial model
    the.predictions <- rep(theta0, steps.ahead)
    predse <- sqrt(sigma2a)
    the.lower.bound <- the.predictions - 1.96 * predse
    the.upper.bound <- the.predictions + 1.96 * predse
    do.plot(the.predictions, the.lower.bound, the.upper.bound, the.title = "trivial model")
    
    # random walk model
    the.predictions <- rep(Zn, steps.ahead)
    predse <- sqrt((1:length(x)) * sigma2a)
    the.lower.bound <- the.predictions - 1.96 * predse
    the.upper.bound <- the.predictions + 1.96 * predse
    do.plot(the.predictions, the.lower.bound, the.upper.bound, the.title = "random walk model")
    
    
    # random walk with drift model
    the.predictions <- Zn + (1:steps.ahead) * theta0
    predse <- sqrt((1:length(x)) * sigma2a)
    the.lower.bound <- the.predictions - 1.96 * predse
    the.upper.bound <- the.predictions + 1.96 * predse
    do.plot(the.predictions, the.lower.bound, the.upper.bound, the.title = "random walk with drift model")
    
    # simple linear regression model
    the.predictions <- theta0 + theta1 * (n + 1:steps.ahead)
    predse <- sqrt(sigma2a)
    the.lower.bound <- the.predictions - 1.96 * predse
    the.upper.bound <- the.predictions + 1.96 * predse
    do.plot(the.predictions, the.lower.bound, the.upper.bound, the.title = "simple linear regression model")
    
    
    par(old.par)
    return()
}
