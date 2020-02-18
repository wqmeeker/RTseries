#' True ACF and PACF Functions
#' @description
#' Given a specified ARMA model (and model parameters), this function will compute and plot the true ACF and PACF functions.
#' @param model Model Specified ARMA model.
#' @param nacf Number of ACF/APCF lags to be computed and plotted (default is 24).
#' @param print.table If true, a ttable of the functions is printed (default is FALSE).
#' @examples
#' show.true.acfpacf(model=list(ar=0.9))
#' show.true.acfpacf(model=list(ar=-0.9))
#' show.true.acfpacf(model=list(ar=c(0.9, -0.6)))
#' show.true.acfpacf(model=list(ar=c(-1.5, -0.6)))
#' show.true.acfpacf(model=list(ar=c(1.5, -0.6)))
#' show.true.acfpacf(model=list(ma=0.9))
#' show.true.acfpacf(model=list(ma=c(0.9, 0.7)))
#' show.true.acfpacf(model=list(ar=0.9, ma=.09))
#' show.true.acfpacf(model=list(ar=0.9, ma=-0.9))
#' show.true.acfpacf(model=list(ar=c(0.9, -0.5), ma=c(0.1, 0.1)))
#' 
#' @importFrom stats ARMAacf
#' @export
"show.true.acfpacf" <- function(model, nacf = 24, print.table = FALSE) {
    old.par <- par(mfrow = c(2, 1), oma = c(0, 0, 4, 0), err = -1)
    on.exit({
        par(old.par)
        par(new = F)
    })
    # model setup
    model.string <- f.true.model.comp.string(model)
    if (is.null(model$ar)) 
        model$ar <- 0
    if (is.null(model$ma)) 
        model$ma <- 0
    # get the true acf; adjust the sign of the ma coefficients
    tacfv <- ARMAacf(ar = model$ar, ma = -model$ma, lag.max = nacf, pacf = FALSE)
    tacfa <- array(tacfv, dim = c(length(tacfv), 1, 1))
    lags <- array(0:length(tacfv[-1]), dim = c(length(tacfv), 1, 1))
    z <- list(lag = lags, acf = tacfa, type = "True ACF")
    # plot the true acf
    my.acf.plot(z, print.table = print.table)
    mtext(side = 3, line = 0.65, cex = 1, model.string)
    # get the true pacf; adjust the sign of ma coefficients
    true.pacf <- ARMAacf(ar = model$ar, ma = -model$ma, lag.max = nacf, pacf = TRUE)
    z <- list(lag = lags, acf = array(c(0, true.pacf), dim = c(length(tacfv), 1, 1)), type = "True PACF")
    # plot the true pacf
    my.acf.plot(z, print.table = print.table)
    mtext(side = 3, line = 0.65, cex = 1, model.string)
    results <- list(true.acf=tacfv, true.pacf=true.pacf )
    invisible(results)
}
