#' Computes the pacf using the Durbin method
#' @description
#' For a given acf function, computes the pacf
#' @param the.acf A vector giving the acf function.
#' @param lag.max integer saying how many pacf values to compute. The default is 10.
#' @examples
#' Durbin(c(0.4, rep(0,100)))
#' Durbin(0.9^(1:100))
#'
#'
#' @export
"Durbin" <- function(the.acf, lag.max=10) {
    the.pacf <- drop(.Call(stats:::C_pacf1, c(1, the.acf), lag.max=lag.max))
    return(the.pacf)
}
