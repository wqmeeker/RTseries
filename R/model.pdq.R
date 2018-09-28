#' Specify an ARIMA Model
#' @description
#' Provides a simple method to specify an ARIMA model that can be used with the \code{\link{esti}} function. Defaults for the following six arguments is 0.
#' @param p The order of the nonseasonal AR polynomial in the model.
#' @param d The number of nonseasonal differences.
#' @param q The order of the nonseasonal MA polynomial in the model.
#' @param P The order of the seasonal AR polynomial in the model.
#' @param D The number of seasonal differences.
#' @param Q TThe order of the nonseasonal MA polynomial in the model.
#' @param period Optional parameter to allow specification of the seasonal period. If not specified, the seasonal period will be inferred from the time series data object that is specified to the \code{\link{esti}} function.
#' @examples
#' # An AR(3) model
#' ar3.model <- model.pdq(p=3)
#' ar3.model
#'
#' # The 'airline model'
#' airline.model <- model.pdq(period=12,d=1,D=1,q=1,Q=1)
#' airline.model
#'
#' # The trivial model
#' trivial.model <- model.pdq()
#' trivial.model
#'
#' @export
model.pdq <- function(p = 0, d = 0, q = 0, P = 0, D = 0, Q = 0, period = NULL) {
    order <- c(p = p, d = d, q = q)
    if (is.null(period)) 
        seasonal <- c(P = P, D = D, Q = Q) else seasonal <- list(order = c(P = P, D = D, Q = Q), period = period)
    return(list(local = order, seasonal = seasonal))
}
