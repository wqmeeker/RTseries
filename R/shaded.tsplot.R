#' Shaded Time Series Plot
#' @description
#' Provides a shaded time series plot.
#' @param x Time series object or a time series data object.
#' @param col The shaded color, default is 1 (black). Other options are 2 for red, 3 for green, 4 for blue, 5 for light blue, 6 for purple, 7 for yellow, and  8 for grey.
#' @param top If top = FALSE, the shaded part will be below the time series. If top = TRUE, the shaded part will be above the time series.
#' @seealso \code{\link{color.test}}.
#' @examples
#' plot(spot.tsd)
#' shaded.tsplot(spot.tsd)
#' shaded.tsplot(spot.tsd, col=4)
#' shaded.tsplot(spot.tsd, top=TRUE, col=3)
#'
#' @importFrom graphics plot
#' @importFrom graphics polygon
#' @importFrom stats time
#' @importFrom stats tsp
#' @importFrom graphics par
#' @export
shaded.tsplot <- function(x, col = 1, top = FALSE) {
    if (top) 
        setdir <- 4 else setdir <- 3
    plot(x, type = "n", bty = "l")
    polygon(c(time(x), tsp(x)[2:1]), c(x, par("usr")[c(setdir, setdir)]), col = col)
}
