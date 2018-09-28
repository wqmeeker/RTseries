
#' Annotate Plots.
#' @description
#' Used to annotate the plots. Add annotation on points chosen on
# the plot. Place the cursor and left click. Repeat for each point desired. Click the middel button to
# escape. Then enter the desired text for each of the chosen points.
#' @param cex provides control over the text font size (default=1)
#' @return Draft \code{\link{text}} commands that might be added to a function.
#' @seealso documentation for \code{\link{locator}}.
#' @examples
#' plot(att.stock.tsd)
#' add.text()
#' 
#' @importFrom graphics locator
#' @importFrom graphics text
#' @export
add.text <- function(cex = 1) {
    x <- 1
    y <- 1
    xy <- locator()
    x <- xy$x
    y <- xy$y
    output <- rep("", length(x))
    if (!is.null(x)) {
        for (i in 1:length(x)) {
            cat(paste("Input text for", x[i], y[i], ":"))
            stuff <- readline()
            text(x[i], y[i], stuff, cex = cex)
            output[i] <- paste("text(", x[i], ",", y[i], ",", stuff, ")")
        }
    }
    return(output)
}
