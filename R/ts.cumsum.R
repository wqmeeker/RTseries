#' Time Series Cumulative Sum
#' @description
#' Computes the cumulative sum (integration) of a time series with optional lagging. The function is the inverse of differencing (possibly seasonal).
#' @param w is the input time series.
#' @param lag allows integration at seasonal lags (lag=1 is the default).
#' @param start allows specification of starting value for the integration. If not specifies, w[1] is used.
#' @examples
#' ozoneRegdat <- read.csv(RTseriesExtDataPath('ozoneRegdat.csv'))
#' ozo.xreg <- cbind(ozoneRegdat[,2], ts.cumsum(ozoneRegdat[, 3], lag=12),
#'                 ts.cumsum(ozoneRegdat[, 4], lag=12))
#' esti(ozone.tsd, model=model.pdq(D=1, q=1, Q=1, period=12),
#'                 y.range=c(0.1, 9), xreg.in=ozo.xreg, gamma=0)
#' 
#' @export
ts.cumsum <- function(w, lag = 1, start) {
    if (!missing(start)) 
        w <- c(start, w) else w <- c(w)
    z <- rep(0, length(w))
    z[1:(lag + 1)] <- w[1:(lag + 1)]
    for (t in (lag + 1):length(z)) {
        z[t] <- z[t - lag] + w[t]
    }
    return(z)
}
