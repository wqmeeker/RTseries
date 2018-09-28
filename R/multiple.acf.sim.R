#' Time Series Model Simulation and ACF Plot
#' @description
#' Simulate time series realizations, with different realization sizes, from a particular time series model and display the sample ACF functions graphically.
#' @param model Optional list containing two components names ar and ma. These components should be vectors containing coefficianes of autoregressive and moving average parts of an ARMA model. If the list is not provided, a random model and parameters are generated.
#' @param sample.size.vec A vector of sample sizes. Length 3 is recommended. Default is sample.size.vec=c(30, 60, 120).
#' @param number.simulations Number of simulations. Default is number.simulations=4.
#' @examples
#' multiple.acf.sim()
#' multiple.acf.sim(sample.size.vec = c(50, 100, 200))
#'
#' @importFrom graphics mtext
#' @importFrom graphics par
#' @export
multiple.acf.sim <- function(model = NULL, sample.size.vec = c(30, 60, 120), number.simulations = 4) {
    if (is.null(model)) {
        model <- random.model()
        cat("Random model and parameters have been chosen \n")
    }
    old.par <- par()
    on.exit({
        old.par[c("cin", "cra", "csi", "cxy", "din")] <- NULL
        par(old.par)
        par(new = F)
    })
    par(mfrow = c(length(sample.size.vec), number.simulations), oma = c(0, 0, 6, 0))
    for (j in 1:length(sample.size.vec)) for (i in 1:number.simulations) {
        acf.sim(sample.size.vec[j], model = model)
    }
    mtext(side = 3, line = 2, cex = 2, outer = TRUE, "Simulated Sample ACF Functions")
    mtext(side = 3, line = 0, cex = 1, outer = TRUE, f.true.model.comp.string(model))
}
