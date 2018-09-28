
#' @importFrom graphics abline
#' @importFrom graphics par
#' @importFrom graphics plot
#' @importFrom stats qt
#' @importFrom stats rnorm
#' @importFrom graphics segments
#' @importFrom stats var
ci.sim <- function(mu = 100, sigma = 10, sample.size = 25, yrange = NULL, confidence.level = 0.95, number.simulations = 100, 
    scale = 2.5) {
    old.par <- par(mai = c(0.1, 0.5, 0.3, 0.1))
    on.exit({
        par(old.par)
        par(new = F)
    })
    tquantile <- qt(1 - (1 - confidence.level)/2, sample.size - 1)
    sigma.xbar <- sigma/sqrt(sample.size)
    xrange <- c(0, 1)
    if (is.null(yrange)) 
        yrange <- c(mu - scale * tquantile * sigma.xbar, mu + scale * tquantile * sigma.xbar)
    plot(xrange, yrange, type = "n", xlab = "", ylab = "", xaxt = "n", main = paste(format(100 * confidence.level), 
        " Percent with n=", sample.size))
    abline(h = mu, lwd = 3)
    for (i in 1:number.simulations) {
        x <- rnorm(sample.size, mu, sigma)
        mean.x <- mean(x)
        sd.xbar <- sqrt(var(x)/sample.size)
        lower <- mean.x - tquantile * sd.xbar
        upper <- mean.x + tquantile * sd.xbar
        xspot <- i/(number.simulations + 1)
        xlower <- (i - 0.35)/(number.simulations + 1)
        xupper <- (i + 0.35)/(number.simulations + 1)
        col <- ifelse(upper > mu & lower < mu, 1, 6)
        segments(xspot, upper, xspot, lower, col = col)
        segments(xlower, upper, xupper, upper, col = col)
        segments(xlower, lower, xupper, lower, col = col)
    }
}
