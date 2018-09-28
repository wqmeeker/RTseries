#' AR or MA Polynomial Roots
#' @description
#' Compute and display an AR or an MA polynomial and its roots, relative to the 'unit circle'.
#' @param coeficients A vector of coefficients of polynomial. 
#' @param nplot The number of points used to display the plot (default is 400).
#' @param my.title If my.title = NULL, it will display coefficient on middle top of graph; otherwise, it will not display coeeficient on graph.
#' @param digits The number of decimal places to be used in numeric output (default is 4).
#' @return The roots of the polynomial are returned as a vector, but invisibly.
#' @examples
#' arma.roots(c(1.6,0.4))
#' arma.roots(c(.5,-.1))
#' arma.roots(c(.5,-.9,.1,.5))
#' # trivial (first order or linear polynomial) example
#' arma.roots(.5)
#'
#' @importFrom graphics abline
#' @importFrom graphics mtext
#' @importFrom graphics par
#' @importFrom graphics plot
#' @importFrom graphics symbols
#' @importFrom graphics title
#' @export
arma.roots <- function(coeficients, nplot = 400, my.title = NULL, digits = 4) {
    old.par <- par(pty = "s", mfrow = c(1, 2))
    on.exit({
        par(old.par)
        par(new = F)
    })
    root <- polyroot(c(1, -coeficients))
    real.root <- Re(root)
    im.root <- Im(root)
    xrange <- range(real.root)
    xrange <- c(xrange[1] - 1.2 * abs(xrange[1]), xrange[2] + 1.2 * abs(xrange[2]))
    xplot <- seq(xrange[1], xrange[2], length = nplot)
    fpoly <- 1
    for (i in 1:length(coeficients)) {
        fpoly <- fpoly - xplot^i * coeficients[i]
    }
    plot(xplot, fpoly, type = "l", xlab = "B", ylab = "Function")
    title(main = "Polynomial Function versus B")
    abline(h = 0)
    distance <- sqrt(real.root^2 + im.root^2)
    root.mat <- cbind(round(real.root, digits = digits), round(im.root, digits = digits), round(distance, 
        digits = digits))
    dimnames(root.mat) <- list(1:nrow(root.mat), c("re", "im", "dist"))
    print(root.mat)
    size.limit <- max(abs(real.root), 1.5, abs(im.root))
    plot(root, xlim = c(-size.limit, size.limit), ylim = c(-size.limit, size.limit), cex = 1.5, xlab = "", 
        ylab = "")
    symbols(0, 0, circles = 1, add = T, inches = F, col = 6)
    abline(h = 0)
    abline(v = 0)
    title("Roots and the Unit Circle", xlab = "Real Part", ylab = "Imaginary Part")
    if (is.null(my.title)) 
        mtext(paste("Coefficients=", paste(coeficients, collapse = ", ")), outer = T, cex = 1.5, line = -4)
}
