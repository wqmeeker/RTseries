#' Prewhitening
#' @description
#' Performs a prewhitening operation to allow one to identify the dynamic relationship betweenan input time series (x) and an output time series (y). By default both the x and the y time series are filtered with the same model to compute residuals and then the cross correlation function between those 'residuals' is plotted, to identify the lags at which y sepends on x in the dynamic regression model.
#' @param y.tsd The output (y) time series.
#' @param x.tsd The input (x) time series.
#' @param x.model The assumed model for x.
#' @param x.coefficients The model coefficients for x.
#' @param y.model (optional) The assumed model for y (default is the x model).
#' @param y.coefficients  (optional) The model coefficients for y (default is the x model coefficcients).
#' @return The numerical values of the cross correlation are invisibly returned.
#' @examples
#' iden(gasrx.tsd)
#' gasrx.ar4.out <- esti(gasrx.tsd, model=model.pdq(p=4))
#' prewhiten(gasry.tsd, gasrx.tsd, x.model=model.pdq(p=4),
#'       x.coefficients=coef(gasrx.ar4.out))
#' 
#' @importFrom stats arima
#' @importFrom stats ccf
#' @export
"prewhiten" <- function(y.tsd, x.tsd, x.model, x.coefficients, y.model, y.coefficients) {
    # do prewhitening to identify the dynamic relationship between y (output) and x (input)
    y.missing <- FALSE
    if (missing(y.model)) {
        y.missing <- TRUE
        y.model <- x.model
        y.coefficients <- x.coefficients
    }
    # get model information
    x.model <- complete.model(x.model, x.tsd)
    y.model <- complete.model(y.model, y.tsd)
    x.model.string <- f.model.string(x.model)
    y.model.string <- f.model.string(y.model)
    # check the length of the coefficient vector
    numberParameters.x <- getNumberParameters(x.model)
    if (length(x.coefficients) != numberParameters.x) 
        stop("x model has", numberParameters.x, "parameters but there are only", length(x.coefficients), 
            "coefficients in x.coefficients")
    numberParameters.y <- getNumberParameters(y.model)
    if (length(y.coefficients) != numberParameters.y) 
        stop("y model has", numberParameters.y, "parameters but there are only", length(y.coefficients), 
            "coefficients in y.coefficients")
    # compute the model output information
    if (any(names(x.coefficients) == "intercept")) 
        x.coefficients["intercept"] <- 0
    if (any(names(x.coefficients) == "intercept")) 
        y.coefficients["intercept"] <- 0
    # 
    out.x <- arima(x.tsd - mean(x.tsd), order = x.model$local, seasonal = x.model$seasonal, fixed = x.coefficients)
    out.y <- arima(y.tsd - mean(y.tsd), order = y.model$local, seasonal = y.model$seasonal, fixed = y.coefficients)
    # compute the residuals
    resid.x <- residuals(out.x)
    resid.y <- residuals(out.y)
    # do the cross correlation function and add titles
    ccf.out <- ccf(resid.y, resid.x, main = "", ylab = "CCF", ylim = c(-1, 1))
    if (y.missing) {
        mtext(paste("Cross-correlation between", deparse(substitute(y.tsd)), "and", deparse(substitute(x.tsd)), 
            "with model", x.model.string), side = 3, line = 0.5)
    } else {
        mtext(paste("Cross-correlation between", deparse(substitute(y.tsd)), "and", deparse(substitute(x.tsd)), 
            "with models", y.model.string, "and", x.model.string), side = 3, line = 0.5)
    }
    invisible(ccf.out)
}

