plot.line <-
function (point1, point2, xrange = NULL, col = 1, lty = 1, lwd = 1) 
{
    if (point1[1] - point2[1] != 0) {
        slope <- (point1[2] - point2[2])/(point1[1] - point2[1])
        intercept <- point1[2] - slope * point1[1]
        if (is.null(xrange)) 
            xrange <- c(point1[1], point2[1])
        yrange <- intercept + slope * xrange
        lines(xrange, yrange, col = col, lty = lty, lwd = lwd)
    }
    else {
        lines(c(point1[1], point2[1]), c(point1[2], point2[2]), 
            col = col, lty = lty, lwd = lwd)
    }
}
