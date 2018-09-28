

#' @importFrom graphics lines
#' @importFrom graphics plot
do.plot <- function(the.predictions, the.lower.bound, the.upper.bound, the.title) {
    y.range <- range(the.predictions, the.lower.bound, the.upper.bound)
    x <- 1:length(the.predictions)
    plot(range(x), y.range, xlab = "Steps Ahead", ylab = "Prediction", type = "n", main = the.title)
    lines(x, the.predictions, type = "b")
    lines(x, the.lower.bound, lty = 2)
    lines(x, the.upper.bound, lty = 2)
}
