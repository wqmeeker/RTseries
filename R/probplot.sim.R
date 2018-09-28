
#' @importFrom graphics axis
#' @importFrom graphics mtext
#' @importFrom graphics plot
#' @importFrom graphics points
#' @importFrom stats qnorm
probplot.sim <- function(distribution = "normal", parameter = NULL, sample.size = 10, axis.probs = c(0.01, 
    0.05, 0.1, 0.3, 0.5, 0.7, 0.9, 0.95, 0.99), print.samp.siz = F) {
    result <- nsf.sim(distribution, parameter, sample.size)
    xdata <- sort(result$sample)
    ppoint <- (1:length(xdata) - 0.5)/length(xdata)
    plot(xdata, qnorm(ppoint), yaxt = "n", type = "n", ylab = "", xlab = "data", cex = 0.7)
    points(xdata, qnorm(ppoint), cex = 1.2)
    axis(side = 2, at = qnorm(as.numeric(axis.probs)), labels = axis.probs, adj = 1, tck = -0.02, mgp = c(5, 
        1.1, 0), cex = 0.7)
    if (print.samp.siz) 
        mtext(side = 3, line = 1, text = paste("Sample Size=", sample.size), cex = 1)
    invisible()
    return(list(sim = result$cdist, parameter = result$parameter, parameter.name = result$parameter.name))
}
