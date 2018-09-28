
#' @importFrom graphics abline
#' @importFrom graphics par
#' @importFrom graphics plot
#' @importFrom stats qt
#' @importFrom stats rnorm
#' @importFrom graphics segments
#' @importFrom stats var
pi.sim <- function(mu = 100, sigma = 10, sample.size = 25, yrange = NULL, confidence.level = 0.95, number.simulations = 100, 
    scale = 2.5) {
    old.par <- par(mai = c(0.1, 0.5, 0.3, 0.1))
    on.exit({
        par(old.par)
        par(new = F)
    })
    tquantile <- qt(1 - (1 - confidence.level)/2, sample.size - 1)
    sigma.xbar <- sigma/sqrt(sample.size)
    sigma.pred <- sqrt((sigma.xbar)^2 + sigma^2)
    xrange <- c(0, 1)
    if (is.null(yrange)) 
        yrange <- c(mu - scale * tquantile * sigma.pred, mu + scale * tquantile * sigma.pred)
    plot(xrange, yrange, type = "n", xlab = "", ylab = "", xaxt = "n", main = paste(format(100 * confidence.level), 
        " Percent with n=", sample.size))
    abline(h = mu, lwd = 3)
    number.in <- 0
    number.out <- 0
    for (i in 1:number.simulations) {
        x <- rnorm(sample.size, mu, sigma)
        xnew <- rnorm(1, mu, sigma)
        mean.x <- mean(x)
        varx <- var(x)
        sd.pred <- sqrt(varx + varx/sample.size)
        lower <- mean.x - tquantile * sd.pred
        upper <- mean.x + tquantile * sd.pred
        xspot <- i/(number.simulations + 1)
        xlower <- (i - 0.35)/(number.simulations + 1)
        xupper <- (i + 0.35)/(number.simulations + 1)
        if (upper > xnew && lower < xnew) {
            number.in <- number.in + 1
            col <- 1
        } else {
            number.out <- number.out + 1
            col <- 6
        }
        col <- ifelse(upper > xnew & lower < xnew, 1, 6)
        segments(xspot, upper, xspot, lower, col = col)
        segments(xlower, upper, xupper, upper, col = col)
        segments(xlower, lower, xupper, lower, col = col)
        points(xspot, xnew, col = 4, pch = 1)
    }
    return(c(number.in, number.out))
}
