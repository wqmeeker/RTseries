"plot.arima.contour" <- function(contour.structure, relative = F, perspective = F, ...) {
    if (relative) {
        max.like <- max(contour.structure$z)
        contour.structure$z <- exp(contour.structure$z - max.like)
    }
    if (perspective) {
        persp(contour.structure, xlab = contour.structure$xlab, ylab = contour.structure$ylab, ...)
    } else {
        contour(contour.structure, xlab = contour.structure$xlab, ylab = contour.structure$ylab, ...)
    }
    invisible()
}

