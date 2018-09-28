#' Prediction Intervals Simulation
#' @description
#' Simulation to illustrate the effect that confidence level and sample size have on the results of prediction interval width and correctness. Simulated samples are used to compute the prediction intervals and then one additional observation is generated and plotted for each inerval. If the additional observation does not fall in the prodiction interval, the interval is marked in red.
#' @inheritParams multiple.ci.sim
#' @param number.simulations Number of prediction intervals to be simulated (default is 100).
#' @examples
#' multiple.pi.sim()
#' multiple.pi.sim(sample.sizes = c(10, 40))
#' multiple.pi.sim(confidence.levels = c(0.95, 0.99))
#' multiple.pi.sim(sample.sizes = c(10, 40), confidence.levels = c(0.95, 0.99))
#'
#' @importFrom graphics mtext
#' @importFrom graphics par
#' @importFrom stats qt
#' @export
multiple.pi.sim <- function(sample.sizes = c(30, 120), confidence.levels = c(0.8, 0.95), mu = 100, sigma = 10, 
    number.simulations = 100, scale = 2) {
    old.par <- par(mfrow = c(length(sample.sizes), length(confidence.levels)), oma = c(0, 0, 2, 0))
    on.exit({
        par(old.par)
        par(new = F)
    })
    tquantile <- qt(1 - (1 - max(confidence.levels))/2, min(sample.sizes) - 1)
    sigma.xbar <- sigma/sqrt(min(sample.sizes))
    sigma.pred <- sqrt((sigma.xbar)^2 + sigma^2)
    yrange <- c(mu - scale * tquantile * sigma.pred, mu + scale * tquantile * sigma.pred)
    for (j in 1:length(sample.sizes)) {
        for (i in 1:length(confidence.levels)) {
            pi.sim(mu = mu, sigma = sigma, sample.size = sample.sizes[j], confidence.level = confidence.levels[i], 
                yrange = yrange, number.simulations = number.simulations)
        }
    }
    mtext(side = 3, line = 0, cex = 1.5, outer = TRUE, "Simulation of Prediction Intervals")
}
