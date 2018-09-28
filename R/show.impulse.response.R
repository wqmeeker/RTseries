#' Compute and plot the impulse response function
#' @description
#' Computes and plots the coefficients of the impulse response function an ARMA model or a transfer function model (using the Box-Jenkins convention for the sign of the MA coefficients). 
#' @param denominator Vector of denominator or autoregressive (AR) coefficients (default is none).
#' @param numerator Vector of numerator or moving average (MA) coefficients (default is none).
#' @param delay in the transfer function model. Default is 0.
#' @param lag.max number of coefficients to compute. Default is 20.
#' @param y.axis logical variable indicating whether the y axis is displayed. Default is FALSE.
#' @param type should be either transfer or arma. Default is transfer.
#' @return A vector of lag.max coefficients is returned invisibly.
#' @seealso \code{\link{psi.weights}}
#' @examples
#' show.impulse.response(numerator=c(1.0), delay=2)
#' show.impulse.response(numerator=c(1.0, -1.3), delay=2)
#' show.impulse.response(numerator=c(1.0, -1.3, -1.65), delay=2)
#' show.impulse.response(numerator=c(0.5, -1.05, -1.52, -0.762, -0.381, -0.191,
#'                -0.0953, -0.0477, -0.0238, -0.01191), delay=2)
#' show.impulse.response(numerator=c(0.5, -0.8, -1.0),
#'                denominator=0.5, delay=2) 
#' show.impulse.response(numerator=c(.1,-.5), den = c(1.0, -0.55))
#' 
#' @importFrom stats ARMAtoMA
#' @export
show.impulse.response <- function(denominator = numeric(), numerator = numeric(), delay = 0, lag.max = 20, 
    y.axis = FALSE, type = "transfer") {
    switch(type, transfer = {
        if (length(numerator) == 0) stop("A numerator coefficient must be specified in transfer-function mode")
        ir.weights <- Trans2IRF(numerator = numerator, denominator = denominator, delay = delay, lag.max = lag.max + 
            delay)
        
    }, arma = {
        ir.weights <- ARMAtoMA(ar = denominator, ma = -numerator, lag.max = lag.max)
    }, {
        stop("type must be transfer or arma")
    })
    y.range <- range(ir.weights)
    if (y.axis) 
        plot(x = c(0, lag.max), y = y.range, bty = "n", ylab = "", xlab = "lag", type = "n", las = 1, 
            cex.lab = 1.4) else plot(x = c(0, lag.max), y = y.range, bty = "n", ylab = "", xlab = "lag", yaxt = "n", type = "n", 
        cex.lab = 2, cex.axis = 2)
    abline(h = 0, lwd = 2)
    axis(side = 1, at = 1:lag.max, labels = FALSE, tck = -0.015, mgp = c(5, 2.1, 0))
    
    for (i in 1:lag.max) {
        if (ir.weights[i] > 0) 
            DrawLine(c(i, 0), c(i, ir.weights[i])) else DrawLine(c(i, ir.weights[i]), c(i, 0))
    }
    invisible(ir.weights)
}
