#' Time Series Diagnostic Plots for AR Models
#' @description
#' Plots the time series and predicted values together, a time series plot of the residuals, a plot of the ACF function of the residuals, and a normal Q-Q plot of the residusls.
#' @param ts.fit Results object from an lm fit of an autoregressive model.
#' @param data.tsd Time series data object corresponding to the AR model that was fit.
#' @param all.on.one If TRUE, all of the plot will be included in one frame. Fefault is TRUE.
#' @examples
#' spotlag.frame <- as.data.frame(cbind(Spot=c(spot.tsd,NA,NA,NA,NA),
#'                                      Spot1=c(NA,spot.tsd,NA,NA,NA),
#'                                      Spot2=c(NA,NA,spot.tsd,NA,NA),
#'                                      Spot3=c(NA,NA,NA,spot.tsd,NA),
#'                                      Spot4=c(NA,NA,NA,NA,spot.tsd)))
#' spot.ar1.fit <- lm(Spot ~ Spot1, data=spotlag.frame)
#' ts.diag.summary(spot.ar1.fit, data.tsd=spot.tsd)
#' spot.ar1.fit <- lm(Spot ~ Spot1, data=spotlag.frame)
#' ts.diag.summary(spot.ar1.fit, data.tsd=spot.tsd)
#' spot.ar2.fit <- lm(Spot ~ Spot1 + Spot2, data=spotlag.frame)
#' ts.diag.summary(spot.ar2.fit, data.tsd=spot.tsd)
#' 
#' @importFrom graphics abline
#' @importFrom stats acf
#' @importFrom stats coef
#' @importFrom graphics lines
#' @importFrom graphics par
#' @importFrom graphics plot
#' @importFrom stats predict
#' @importFrom stats qqnorm
#' @importFrom stats ts
#' @importFrom stats tsp
#' @export
ts.diag.summary <- function(ts.fit, data.tsd, all.on.one = TRUE) {
    # diagnostic summary for fitting AR models
    if (all.on.one) 
        par(mfrow = c(2, 2))
    the.time.units <- time.units(data.tsd)
    the.data.title <- data.title(data.tsd)
    the.order <- length(coef(ts.fit)) - 1
    model.string <- paste(paste("AR(", the.order, ")", sep = ""), " Model", sep = "")
    the.tsp <- tsp(data.tsd)
    plot(data.tsd, main = paste(the.data.title, "\nand", model.string, "1-Step Ahead Predictions"))
    ts.predict <- ts(c(rep(NA, the.order), predict(ts.fit)), start = the.tsp[1], frequency = the.tsp[3])
    lines(ts.predict, lty = 2)
    plot(ts(residuals(ts.fit), start = the.tsp[1], frequency = the.tsp[3]), main = paste(the.data.title, 
        "\n", model.string, "Residuals"), xlab = the.time.units, ylab = "Residuals")
    abline(h = 0)
    acf(residuals(ts.fit), main = paste(the.data.title, "\n", model.string, "Residual ACF Function"))
    qqnorm(residuals(ts.fit), main = paste(the.data.title, "\n", model.string, "Normal Q-Q Plot"))
    if (all.on.one) 
        par(mfrow = c(1, 1))
    invisible()
}
