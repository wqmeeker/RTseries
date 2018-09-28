
#' @importFrom graphics plot
#' @importFrom stats stl
try.stl <- function(the.ts, window.size) {
    for (i in window.size) {
        fit <- stl(the.ts, s.window = i)
        plot(fit, main = paste("s.window=", i))
    }
}
