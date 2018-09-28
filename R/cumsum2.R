#' Integrate a time series
#' @description
#' Performs the operation of integrating a time series (inverse of a difference). Allows seasonal integtation.
#' @param w The input time series.
#' @param lag Seasonal lag if a seasonal integration is desired. The default is lag=1.
#' @param start Optional value that will be the base value. The degault is to use the first element of w.
#' @return A time series.
#' @examples
#' ozone.xmat <- read.csv(RTseriesExtDataPath("ozoneRegdat.csv"))
#' ozo.xreg <- cbind(ozone.xmat[,2], cumsum2(ozone.xmat[,3],12), cumsum2(ozone.xmat[,4],12))
#' @export
cumsum2 <-
function(w, lag = 1, start) {
    if (!missing(start)) 
        w <- c(start, w) else w <- c(w)
    z <- rep(0, length(w))
    z[1:(lag + 1)] <- w[1:(lag + 1)]
    for (t in (lag + 1):length(z)) {
        z[t] <- z[t - lag] + w[t]
    }
    return(z)
}
