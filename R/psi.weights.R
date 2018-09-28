#' Compute psi weights for an ARMA model
#' @description
#' Computes the coefficients of the (potentially) infinite moving average (MA) model representation of an ARMA model (using the Box-Jenkins convention for the sign of the MA coefficients). These coefficients can also be viewed as the impulse response function of the ARMA model.
#' @param ar Vector of autoregressive (AR) coefficients (default is none).
#' @param ma Vector of moving average (MA) coefficients (default is none).
#' @param lag.max number of psi weights to compute
#' @return A vector of lag.max coefficients.
#' @examples
#' psi.weights(ar=0.5)
#' psi.weights(ar=0.5, ma=-0.5)
#' psi.weights(ar=0.9, ma=-0.9)
#' psi.weights(ar=0.99)
#' psi.weights(ar=0.5, ma=0.5)
#' 
#' @importFrom stats ARMAtoMA
#' @export
psi.weights <- function(ar = numeric(), ma = numeric(), lag.max = 10) {
    ARMAtoMA(ar = ar, ma = -ma, lag.max = lag.max)
}
