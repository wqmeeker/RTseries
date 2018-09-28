
#' Probability Plot Simulation
#' @description
#' Simulates data and makes probability plots to allow an assessment of the expected deviation from probability plots in repeated sampling.
#' @param distribution Distribution from which samples are drawn.
#' @param parameter Value of the scale or shape parameter (depending on the distribution). Default is 1.
#' @param sample.size.vec Vector of sample sizes for the simulation. Defauls is c(10, 20, 40).
#' @param number.simulations Number of simulations for each sample size. Default is 5.
#' @param my.title Allows specification of an alternative title or '' for no title
#' @examples
#' multiple.probplot.sim()
#' multiple.probplot.sim(distribution='exponential')
#' multiple.probplot.sim(distribution='weibull', parameter=0.8)
#' multiple.probplot.sim(sample.size.vec=c(25, 50, 100))
#' 
#' @importFrom graphics mtext
#' @importFrom graphics par
#' @export
multiple.probplot.sim <- function(distribution = "normal", parameter = 1, sample.size.vec = c(10, 20, 
    40), number.simulations = 5, my.title = NULL) {
    old.par <- par(mfrow = c(length(sample.size.vec), number.simulations), oma = c(0, 0, 4, 0), err = -1)
    on.exit({
        par(old.par)
        par(new = F)
    })
    mid.plot <- floor(number.simulations/2 + 1)
    par(mfrow = c(length(sample.size.vec), number.simulations), oma = c(0, 0, 4, 0))
    for (j in 1:length(sample.size.vec)) for (i in 1:number.simulations) {
        if (i == mid.plot) {
            print.samp.siz <- T
        } else {
            print.samp.siz <- F
        }
        dist <- probplot.sim(distribution = distribution, parameter = parameter, sample.size = sample.size.vec[j], 
            print.samp.siz = print.samp.siz)
    }
    if (is.null(my.title)) 
        my.title <- paste("Normal Probability Plot with Simulated ", dist$sim, "Data")
    mtext(side = 3, line = 0, cex = 2, outer = TRUE, my.title)
}
