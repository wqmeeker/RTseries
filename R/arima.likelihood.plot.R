
#' ARIMA Model Likelihood Plot
#' @description
#' Provides a plot of the likelihood or one-dimensional profile likelihood for a specified ARIMA model and times series data object.
#' @param data.tsd Time series data object.
#' @param model Time series model.
#' @param param1 List containing the integer parameter number of the parameter and sequence of values at which the likelihood will be evaluated.
#' @param relative If true, the relative likelihood is plotted instead of the log likelihood (default if FALSE)
#' @param verbose Prints model coefficient values for each evaluation point; useful to debug estimation problems when doing a profile plot (when the number of parameters is greater than 1).
#' @param my.title A title (or an empty character string) to replace the default title.
#' @return Invisibly returns the information used to make the contour plot.
#' @seealso \code{\link{arima.contour.plot}}.
#' @examples
#' #ar1 model
#' arima.likelihood.plot(device.inventory.tsd, model = model.pdq(p = 1),
#'            list(1, seq(-0.5, .9999, length =50)))
#' 
#' #ar1 model zoom in
#' arima.likelihood.plot(device.inventory.tsd,model = model.pdq(p = 1),
#'            list(1, seq(0.5, .9999, length =50)), my.title='')
#' 
#' @importFrom stats arima
#' @importFrom stats coef
#' @importFrom graphics mtext
#' @importFrom graphics plot
#' @export
"arima.likelihood.plot" <- function(data.tsd, model, param1 = list(1, seq(-0.99, 0.99, length = 10)), 
    relative = FALSE, verbose = FALSE, my.title) {
    # Profile (relative) likelihood for an ARIMA model
    model <- complete.model(model, data.tsd)
    model.string <- f.model.string(model)
    # get test mle
    arima.test.out <- arima(data.tsd, order = model$local, seasonal = model$seasonal)
    mle.coefficients <- coef(arima.test.out)
    mle.coefficients.names <- names(mle.coefficients)
    
    # centering the time series instead of estimating the constant term
    data.tsd <- data.tsd - mean(data.tsd)
    very.small <- -1e+35
    param.vec <- param1[[2]]
    parameter.position <- param1[[1]]
    prefix <- substring(mle.coefficients.names[parameter.position], 1, 2)
    mult.factor <- 1
    if (prefix == "ma" || prefix == "sm") 
        mult.factor <- -1
    z <- rep(very.small, length = length(param.vec))
    numberParameters <- model$local["p"] + model$local["q"] + model$seasonal$order["P"] + model$seasonal$order["Q"]
    numberParametersARMA <- numberParameters
    fixed.now <- rep(NA, length = numberParameters)
    if (model$local[2] == 0 && model$seasonal$order[2] == 0) {
        numberParameters <- numberParameters + 1
        fixed.now <- rep(NA, length = numberParameters)
        fixed.now[numberParameters] <- 0
    }
    for (i in 1:length(param.vec)) {
        fixed.now[parameter.position] <- mult.factor * param.vec[i]
        arima.out <- try(arima(data.tsd, order = model$local, seasonal = model$seasonal, fixed = fixed.now, 
            transform.pars = FALSE), silent = TRUE)
        if (length(arima.out) > 1 && verbose) {
            cat("length(arima.out)=", i, length(arima.out), "\n")
            print(coef(arima.out))
        }
        if (length(arima.out) == 14 && !is.nan(arima.out$loglik)) {
            z[i] <- arima.out$loglik
            arima.out.save <- arima.out
        }
    }
    zmin <- min(z[z > very.small])
    z[z <= very.small] <- zmin
    if (relative) {
        z <- exp(z)/max(exp(z))
        the.ylab <- "Relative Likelihood"
    } else {
        the.ylab <- "Log-Likelihood"
    }
    if (numberParametersARMA > 1) 
        the.ylab <- paste("Profile", the.ylab)
    if (missing(my.title)) 
        my.title <- paste(attr(data.tsd, "data.title"), model.string, "\n", the.ylab)
    the.xlab <- names(coef(arima.out))[param1[[1]]]
    plot(param.vec, z, xlab = "", ylab = the.ylab, type = "l", main = my.title, cex.lab = 1.2)
    mtext(fix.arma.lab.greek(the.xlab), side = 1, las = 1, line = 3, cex = 1.8)
    invisible(list(x = param.vec, z = z, xlab = the.xlab, ylab = the.ylab))
}

