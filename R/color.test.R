#' Color Test
#' @description
#' Provides a pie chart showing different color options, by number.
#' @param n is the number of colors (8 is the default because the colors recycle after col=9 in the standard R setup).
#' @examples
#' color.test()
#' shaded.tsplot(spot.tsd, col=4)
#' 
#' @importFrom graphics pie
#' @export
color.test <- function(n = 8) {
    pie(rep(1, length = n), col = 1:n, labels = as.character(1:n))
}
