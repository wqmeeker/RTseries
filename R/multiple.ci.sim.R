#' Confidence Interval Simulation
#' @description
#' Simulation to illustrate the effect that confidence level and sample size have on the results of confidence interval width and correctness.
#' @param sample.sizes A vector of sample sizes. Length 2 is recommended. Default is sample.sizes=c(30, 120)
#' @param confidence.levels A vector of confidence levels. Length 2 is recommended. Default is confidence.levels=c(0.8, 0.95)
#' @param mu Mean of the distribution from which samples are taken (default is 100)
#' @param sigma Standard deviation of the distribution from which samples are taken (default is 10).
#' @param number.simulations Number of confidence intervals to be simulated (default is 100).
#' @param scale Scale factor that determines the scale of the common y-axis of the plots. The default is 2. A larger number would allow more white space around edges. A smaller number might allow some of the interval endpoints to be outside of the plots.
#' @details
#' If the lengths of sample.sizes and confidence.levels are both 2, the output is in the form of a 2 x 2 factorial that shows the effect that sample size and confidence level have on the width and probability of correctness of a sequence of prediction intervals. Each time the function is executed, different simulated data sets led to different sets of confidence inervals to be plotted.
#' @examples
#' multiple.ci.sim()
#' multiple.ci.sim(sample.sizes = c(10, 40))
#' multiple.ci.sim(confidence.levels = c(0.95, 0.99))
#' multiple.ci.sim(sample.sizes = c(10, 40), confidence.levels = c(0.95, 0.99))
#'
#' @importFrom graphics mtext
#' @importFrom graphics par
#' @importFrom stats qt
#' @export
multiple.ci.sim <- function(sample.sizes = c(30, 120), confidence.levels = c(0.8, 0.95), mu = 100, sigma = 10, 
    number.simulations = 100, scale = 2.5) {
    old.par <- par(mfrow = c(length(sample.sizes), length(confidence.levels)), oma = c(0, 0, 2, 0))
    on.exit({
        par(old.par)
        par(new = F)
    })
    tquantile <- qt(1 - (1 - max(confidence.levels))/2, min(sample.sizes) - 1)
    sigma.xbar <- sigma/sqrt(min(sample.sizes))
    yrange <- c(mu - scale * tquantile * sigma.xbar, mu + scale * tquantile * sigma.xbar)
    for (j in 1:length(sample.sizes)) {
        for (i in 1:length(confidence.levels)) {
            ci.sim(mu = mu, sigma = sigma, sample.size = sample.sizes[j], confidence.level = confidence.levels[i], 
                yrange = yrange, number.simulations = number.simulations)
        }
    }
    mtext(side = 3, line = 0, cex = 1.5, outer = TRUE, "Simulation of Confidence Intervals")
}
