#' ARIMA Model Identification
#' @description
#' Graphical outputs to help tentatively identify a ARIMA model. The plots produced are: plot of the original or transformed data, a range-mean plot of the transformed data, and plots of the sample ACF and PACF.
#' @param data.tsd Time series data object (output from the tsd function).
#' @param d The number of regular differences carried out on the data. If a value is not specified then the default value is d=0.
#' @param seasonal Optional. The number of observations in a period for a seasonal timeseries. By default this value is automatically taken from the time-series data object (.tsd object).
#' @param gamma Optional. The Box-Cox transformation power parameter. The default value is gamma=1 (no transformation).
#' @param m Optional. The constant that is added to the response variable before the data are transformed. The default value is m=0.
#' @param D Optional. The number of seasonal differences carried out on the data. The default value is D=0.
#' @param lag.max Optional. The maximum number of lags at which to estimate the ACF and PACF. The default value is lag.max=38
#' @param print.table Optional. Default value is print.table = FALSE. If print.table = TRUE, R will output a table of the ACF and PACF estimates.
#' @return Invisible list containing the numerical values of the ACF and PACF estimates that were plotted.
#' @examples
#' iden(spot.tsd)
#'
#' Passengers.ts <- ts(Passengers, freq=12, start=1949)
#' Passengers.tsd <- tsd(Passengers.ts, data.title='International Airline Passengers',
#'                       time.units='Year',response.units='Thousands of Passengers')
#' iden(Passengers.tsd)
#' iden(Passengers.tsd, d=1, D=1)
#' iden(Passengers.tsd, d=1, D=1, gamma=0)
#'
#' @importFrom stats acf
#' @importFrom graphics close.screen
#' @importFrom graphics lines
#' @importFrom graphics mtext
#' @importFrom graphics par
#' @importFrom graphics plot
#' @importFrom graphics screen
#' @importFrom graphics split.screen
#' @importFrom stats tsp
#' @export
iden <- function(data.tsd, seasonal = tsp(data.tsd)[3], gamma = 1, m = 0, d = 0, D = 0, lag.max = 38, 
    print.table = FALSE) {
    object.name <- deparse(substitute(data.tsd))
    if (!is.element("tsd", class(data.tsd))) 
        stop("The input data object", object.name, "is not a RTSERIES time series data (tsd) object")
    time.units <- attr(data.tsd, "time.units")
    data.title <- attr(data.tsd, "data.title")
    response.units <- attr(data.tsd, "response.units")
    ylab <- response.units
    attr(data.tsd, "object.name") <- data.tsd
    nrdf <- d
    nsdf <- D
    old.par <- par()
    on.exit({
        old.par[c("cin", "cra", "csi", "cxy", "din", "page")] <- NULL
        par(old.par)
        par(new = FALSE)
    })
    cat(paste("Identification Output for", data.title, "\n"))
    y.trans <- box.cox.tran(data.tsd, gamma, m, ylab)
    trans.ylab <- attr(y.trans, "trans.string")
    # no differencing---plot range-mean plot
    period <- tsp(data.tsd)[3]
    if (period == 1) 
        period <- 10
    if (nrdf == "0" & nsdf == "0") {
        close.screen(all.screens = TRUE)
        fig1 <- matrix(c(0.001, 0.999, 0.601, 0.999, 0.001, 0.499, 0.001, 0.599, 0.501, 0.999, 0.301, 
            0.599, 0.501, 0.999, 0.001, 0.299), ncol = 4, byrow = TRUE)
        split.screen(fig1)
        screen(2)
        f.range.mean(y.trans, period)
        screen(1)
        par(mai = c(0.4, 0.5, 0.7, 0.1))
        plot(y.trans, xlab = "", ylab = "w", type = "n", main = "")
        lines(y.trans, lwd = 2, col = 2)
        diff.trans.ylab <- paste("w=", trans.ylab)
        cat(paste(diff.trans.ylab, "\n"))
        mtext(text = data.title, side = 3, line = 1.5, font = 2, cex = 1.5)
        mtext(text = diff.trans.ylab, side = 3, line = 0.5)
        mtext(text = "Time", side = 1, line = 1.5)
        screen(3)
        print.sd.series(y.trans)
        acf.out <- acf(y.trans, lag.max = lag.max, type = "correlation", plot = FALSE)
        par(mai = c(0.6, 0.5, 0.3, 0.1))
        my.acf.plot(acf.out, data.tsd, print.table = print.table, xlab = "")
        mtext("Lag", side = 1, line = 1.6)
        screen(4)
        pacf.out <- acf(y.trans, lag.max = lag.max, type = "partial", plot = FALSE)
        par(mai = c(0.6, 0.5, 0.3, 0.1))
        my.acf.plot(pacf.out, data.tsd, print.table = print.table, xlab = "")
        mtext("Lag", side = 1, line = 1.6)
        close.screen(all.screens = TRUE)
        if (!print.table) 
            cat("Use the agrument ',print.table = TRUE' to print tables of the ACF and PACF\n\n")
        invisible(list(acf.out, pacf.out))
    } else {
        # some differencing---plot range-mean plot
        yd <- y.trans
        if (nrdf > 0) {
            yd <- diff(yd, differences = nrdf)
            nrdf.lab <- paste("(1-B)^", format(nrdf))
        } else {
            nrdf.lab <- NULL
        }
        if (nsdf > 0) {
            yd <- diff(yd, lag = seasonal, differences = nsdf)
            nsdf.lab <- paste("(1-B^", format(seasonal), ")^", format(nsdf))
        } else {
            nsdf.lab <- NULL
        }
        diff.trans.ylab <- paste("w=", nrdf.lab, nsdf.lab, trans.ylab)
        cat(paste(diff.trans.ylab, "\n"))
        fig2 <- matrix(c(0, 1, 0.5, 1, 0, 0.5, 0, 0.5, 0.5, 1, 0, 0.5), ncol = 4, byrow = TRUE)
        close.screen(all.screens = TRUE)
        split.screen(fig2)
        screen(1)
        diff.main <- paste(data.title, "\n", diff.trans.ylab)
        # title(main=diff.main,cex=.8)
        plot(yd, ylab = "w", xlab = "time", type = "n")
        mtext(text = data.title, side = 3, line = 2, cex = 1)
        mtext(text = diff.trans.ylab, side = 3, line = 1)
        lines(yd, lwd = 2, col = 2)
        screen(2)
        print.sd.series(y.trans)
        acf.out <- acf(yd, lag.max = lag.max, type = "correlation", plot = FALSE)
        par(mai = c(1, 0.5, 0.4, 0.1))
        my.acf.plot(acf.out, data.tsd, print.table = print.table)
        screen(3)
        pacf.out <- acf(yd, lag.max = lag.max, type = "partial", plot = FALSE)
        par(mai = c(1, 0.5, 0.4, 0.1))
        my.acf.plot(pacf.out, data.tsd, print.table = print.table)
        close.screen(all.screens = TRUE)
        invisible(list(acf.out, pacf.out))
    }
}
