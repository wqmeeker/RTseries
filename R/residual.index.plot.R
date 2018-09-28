
#' @importFrom stats cooks.distance
#' @importFrom graphics par
#' @importFrom graphics plot
#' @importFrom stats residuals
#' @importFrom graphics title
#' @importFrom stats ts
residual.index.plot <- function(fit.object) {
    old.par <- par()
    cannot.remove <- c("cin", "cra", "csi", "cxy", "din")
    old.par[cannot.remove] <- NULL
    on.exit(par(old.par))
    par(mfrow = c(2, 1), oma = c(0, 1, 2, 0))
    the.residuals <- ts(residuals(fit.object))
    plot(the.residuals, xlab = "Observation Number", ylab = "Residual")
    title(main = "Residuals versus Observation Number")
    the.cookd <- ts(cooks.distance(fit.object))
    plot(the.cookd, xlab = "Observation Number", ylab = "Cooks D")
    title(main = "Cook's Distance versus Observation Number")
}
