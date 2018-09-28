#' Scatter Plots Illustrating Autoorrelation
#' @description
#' Displays a sequence of scatter plots for an input time series, illustrating the autocorrelation between observations seperated by time lags between  (by default) 0 and 13, and the summary ACF function. 
#' @param in.object Time series or time series data objects (created by function code{\link{tsd}}).
#' @param prow Number of rows in the matrix of scatter plots to be displayed (default is 3). See mfrow in \code{\link{par}}.
#' @param pcol Number of columns in the matrix of scatter plots to be displayed (default is 5). See mfcol in \code{\link{par}}.
#' @seealso \code{\link{par}}
#' @examples
#' plot(spot.tsd)
#' show.acf(spot.tsd)
#' 
#' @importFrom stats acf
#' @importFrom stats cor
#' @importFrom graphics par
#' @importFrom graphics plot
#' @export
show.acf <- function(in.object, prow = 3, pcol = 5) {
    if (is.list(in.object)) {
        timeseries <- in.object$ts
    } else {
        timeseries <- in.object
    }
    timeseries <- timeseries[!is.na(timeseries)]
    old.par <- par()
    cannot.remove <- c("cin", "cra", "csi", "cxy", "din", "page")
    old.par[cannot.remove] <- NULL
    on.exit({
        par(old.par)
        par(new = F)
    })
    par(mfrow = c(prow, pcol), oma = c(0, 1, 7, 0))
    plot(timeseries, timeseries)
    print(c(1, cor(timeseries, timeseries)))
    for (i in 1:(prow * pcol - 2)) {
        drop.end <- (length(timeseries) - i + 1):length(timeseries)
        drop.begin <- (1:i)
        plot(timeseries[-drop.begin], timeseries[-drop.end], xlab = "series", ylab = paste("Series lag", 
            i), main = paste("Lag", i))
        print(cor(timeseries[-drop.begin], timeseries[-drop.end]))
    }
    xx <- acf(timeseries)
    if (class(in.object)[1] == "tsd") {
        mtext(attr(in.object, "data.title"), outer = TRUE, cex = 2)
    }
    invisible(xx)
}
