#' Plot Time Series Data Object
#' @description
#' Plots the time series in the specified time series data object.
#' @param x A time series data object (created with function tsd).
#' @param ylab Label for the y axis. Default is response.units in data.tsd.
#' @param xlab Label for the time axis. Default is time.units in data.tsd.
#' @param main Plot title.  Default is data.title in data.tsd.
#' @param ... Allows sending down extra arguments such as graphs parameters.
#' @examples
#' plot(spot.tsd)
#' plot(boston.robberies.tsd, las=1)
#' plot(gasrx.tsd)
#' plot(gasrx.tsd)
#' 
#' @importFrom stats plot.ts
#' @export
plot.tsd <- function(x, ylab = NULL, xlab = NULL, main = NULL, ...) {
    tmp <- list(...)
    if (is.null(ylab)) 
        ylab <- attr(x, "response.units")
    if (is.null(xlab)) 
        xlab <- attr(x, "time.units")
    if (is.null(main)) 
        main <- attr(x, "data.title")
    plot.ts(x, main = main, xlab = xlab, ylab = ylab, ...)
    title(main = main, xlab = xlab, ylab = ylab)
}
