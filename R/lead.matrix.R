#' Leading Indicator Matrix
#' @description
#' Uses a leading indicator time series to create a leading indicator matrix that can be used as input to the esti function.
#' @param xvec A leading indicator time series
#' @param lagvec A vector of integers indicating the lags with non-zzero coefficients in the dynamic regression model.
#' @param number.forecasts The number of forecasts that will be requested (the default is 24).
#' @return A time series matrix, suitable to use as input for a dynamic regression model using the esti function.
#' @examples
#' gasrx.lead.matrix <- lead.matrix(gasrx.tsd, lagvec=c(3, 4, 5, 6))
#' est.out <- esti(gasry.tsd, model=model.pdq(p=3), xreg.in=gasrx.lead.matrix)
#' 
#' @importFrom stats lag
#' @importFrom stats ts
#' @export
lead.matrix <- function(xvec, lagvec, number.forecasts = 24) {
    # create a leading indicator time series matrix
    grate <- ts(c(rep(NA, min(lagvec)), xvec, rep(0, number.forecasts - min(lagvec))))
    grate.xmat <- lag(grate, -lagvec[1])
    if (length(lagvec) == 1) 
        return(grate.xmat)
    for (i in 2:length(lagvec)) {
        grate.xmat <- cbind(grate.xmat, lag(grate, -lagvec[i]))
    }
    dimnames(grate.xmat) <- list(1:nrow(grate.xmat), paste("lag", lagvec, sep = ""))
    return(grate.xmat)
}
