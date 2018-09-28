
#' ARIMA Model Likelihood Contour Plot
#' @description
#' Provides a contour plot of the likelihood or two-dimensional profile likelihood for a specified ARIMA model and time series data object.
#' @param data.tsd Time series data object.
#' @param model Time series model.
#' @param param1 List containing the integer parameter number of the first parameter and sequence of values at which the likelihood will be evaluated.
#' @param param2 List containing the integer parameter number of the second parameter and sequence of values at which the likelihood will be evaluated.
#' @param relative If true, the relative likelihood is plotted instead of the log likelihood (default is FALSE)
#' @param verbose Prints model coefficient values for each evaluation point; useful to debug estimation problems when doing a profile plot (when the number of parameters is greater than 2).
#' @param my.title A title (or an empty character string) to replace the default title.
#' @param persp.plot If TRUE a perspeciive plot (instead of a contour plot) will be provided.
#' @param ... Allows sending down extra arguments to the contour or persp functions (such as graphs parameters).
#' @return Invisibly returns the information used to make the contour plot.
#' @seealso \code{\link{arima.likelihood.plot}}.
#' @examples
#' #ar1=2 model
#' arima.contour.plot(device.inventory.tsd, model = model.pdq(p = 2),
#'          list(1, seq(-1.99, 1.99, length = 50)),
#'          list(2, seq(-.99, .99, length = 50)))
#' #ar1=2 model--zoom in
#' \dontrun{arima.contour.plot(device.inventory.tsd, model = model.pdq(p = 2),
#'          list(1, seq(1.35, 1.394, length = 40)),
#'          list(2, seq(-0.408, -0.390, length = 40)), my.title='')}
#' 
#' @importFrom stats arima
#' @importFrom stats coef
#' @importFrom graphics contour
#' @importFrom graphics mtext
#' @importFrom graphics persp
#' @export
arima.contour.plot <- function(data.tsd, model, param1 = list(1, seq(-0.99, 0.99, length = 8)), param2 = list(2, 
    seq(-0.99, 0.99, length = 8)), relative = FALSE, verbose = FALSE, my.title, persp.plot = FALSE, ...) {
    # Profile (relative) likelihood contour for an ARIMA model note that the order of the parameters is
    # critical
    model <- complete.model(model, data.tsd)
    model.string <- f.model.string(model)
    # get test mle
    arima.test.out <- arima(data.tsd, order = model$local, seasonal = model$seasonal)
    mle.coefficients <- coef(arima.test.out)
    mle.coefficients.names <- names(mle.coefficients)
    # centering the time series instead of estimating the constant term
    data.tsd <- data.tsd - mean(data.tsd)
    very.small <- -1e+35
    param.vec1 <- param1[[2]]
    param.vec2 <- param2[[2]]
    parameter.position <- c(param1[[1]], param2[[1]])
    prefix1 <- substring(mle.coefficients.names[parameter.position[1]], 1, 2)
    prefix2 <- substring(mle.coefficients.names[parameter.position[2]], 1, 2)
    mult.factor1 <- 1
    if (prefix1 == "ma" || prefix1 == "sm") 
        mult.factor1 <- -1
    mult.factor2 <- 1
    if (prefix2 == "ma" || prefix2 == "sm") 
        mult.factor2 <- -1
    z <- matrix(very.small, nrow = length(param.vec1), ncol = length(param.vec2))
    numberParameters <- model$local["p"] + model$local["q"] + model$seasonal$order["P"] + model$seasonal$order["Q"]
    numberParametersARMA <- numberParameters
    fixed.now <- rep(NA, length = numberParameters)
    # if there is a constant term, fix it at 0
    if (model$local[2] == 0 && model$seasonal$order[2] == 0) {
        numberParameters <- numberParameters + 1
        fixed.now <- rep(NA, length = numberParameters)
        fixed.now[numberParameters] <- 0
    }
    for (i in 1:length(param.vec1)) {
        if (i%%10 == 0) 
            cat(paste(100 * (i/length(param.vec1)), "% done\n", sep = ""))
        for (j in 1:length(param.vec2)) {
            fixed.now[parameter.position] <- c(mult.factor1 * param.vec1[i], mult.factor2 * param.vec2[j])
            arima.out <- try(arima(data.tsd, order = model$local, seasonal = model$seasonal, fixed = fixed.now, 
                transform.pars = FALSE), silent = TRUE)
            # browser()
            if (length(arima.out) > 1 && verbose) {
                cat("length(arima.out)=", i, j, length(arima.out), "\n")
                print(coef(arima.out))
            }
            if (length(arima.out) > 1 && !is.nan(arima.out$loglik)) {
                z[i, j] <- arima.out$loglik
                arima.out.save <- arima.out
            }
            # if(i==50&&j==3)browser()
        }
    }
    # browser()
    zmin <- min(z[z > very.small])
    z[z <= very.small] <- zmin
    z.rel <- exp(z)/max(exp(z))
    if (relative) {
        z <- exp(z)/max(exp(z))
        zlab <- "Relative Likelihood"
    } else {
        zlab <- "Log-Likelihood"
    }
    if (numberParametersARMA > 2) 
        zlab <- paste("Profile", zlab)
    if (missing(my.title)) 
        my.title <- paste(attr(data.tsd, "data.title"), model.string, "\n", zlab)
    the.xlab <- names(coef(arima.out.save))[param1[[1]]]
    the.ylab <- names(coef(arima.out.save))[param2[[1]]]
    if (persp.plot) {
        persp(param.vec1, param.vec2, z, xlab = fix.arma.lab(the.xlab), ylab = fix.arma.lab(the.ylab), 
            zlab = zlab, main = my.title, col = 13, ltheta = 120, shade = 0.3, ticktype = "detailed", 
            axes = TRUE, ...)
        # title(xlab=bquote(phi))
    } else {
        contour(param.vec1, param.vec2, z, xlab = "", ylab = "", labcex = 1.3, main = my.title, cex.lab = 1.5, 
            ...)
        mtext(fix.arma.lab.greek(the.ylab), side = 2, las = 1, line = 2, cex = 1.8)
        mtext(fix.arma.lab.greek(the.xlab), side = 1, las = 1, line = 3, cex = 1.8)
    }
    invisible(list(x = param.vec1, y = param.vec2, z = z, xlab = fix.arma.lab(the.xlab), ylab = fix.arma.lab(the.ylab)))
}
