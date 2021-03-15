
#' @importFrom graphics abline
#' @importFrom graphics lines
#' @importFrom graphics plot
#' @importFrom graphics title
#' @importFrom stats tsp
"my.acf.plot" <- function(z, data.ts = NULL, conf.int = T, number.parameter = -1, sample.size = NULL, print.table = FALSE, 
    xlab = "Lag", seasonal.lags=FALSE) {
    true <- FALSE
    if (!is.list(z)) 
        stop("z must be a list")
    cname <- names(z)
    if (!is.null(data.ts) && !is.null(tsp(data.ts)[3]) && !seasonal.lags) {
        seasonal.factor <- tsp(data.ts)[3]
    } else {
        seasonal.factor <- 1
    }
    if (!any(cname == "acf") && !any(cname == "lag")) 
        stop("Missing components acf or lag")
    switch(z$type, partial = {
        T.x <- c(0, z$lag[, 1, 1]) * seasonal.factor
        T.y <- c(0, z$acf[, 1, 1])
        c1 <- rep(2/sqrt(z$n.used), length(T.x))
        T.main <- "PACF"
        T.ylab <- "Partial ACF"
        partial <- TRUE
    }, ccf = {
        T.x <- c(z$lag[, 1, 1]) * seasonal.factor
        T.y <- c(z$acf[, 1, 1])
        c1 <- rep(2/sqrt(z$n.used), length(T.x))
        T.main <- "Cross Correlation"
        T.ylab <- "CCF"
        partial <- T
    }, rcorrelation = , correlation = {
        partial <- FALSE
        T.x <- z$lag[, 1, 1] * seasonal.factor
        #T.x <- z$lag[, 1, 1]
        T.y <- z$acf[, 1, 1]
        T.y[1] <- 0
        c1 <- 2 * bartlett.se(T.y, z$n.used)
        if (number.parameter > -1) {
            cat("Ljung-Box Statistics \n")
            ljung.box.out <- ljung.box(T.y, z$n.used, number.parameter)
            print(ljung.box.out)
        }
        if (!is.null(sample.size)) {
            T.main <- paste("ACF ", "   Sample Size= ", sample.size)
        } else {
            T.main <- "ACF"
            if (z$type == "rcorrelation") T.main <- "Residual ACF"
        }
        T.ylab <- "ACF"
    }, `True ACF` = , `True PACF` = {
        true <- T
        T.x <- z$lag[, 1, 1] * seasonal.factor
        T.y <- z$acf[, 1, 1]
        T.y[1] <- 0
        c1 <- rep(0, length(T.y))
        T.main <- z$type
        T.ylab <- z$type
    })
    plot(T.x, T.y, type = "n", xlab = xlab, ylab = T.ylab, main = "", ylim = c(-1, 1), lwd = 2)
    title(main = T.main, cex.main = 0.9)
    lines(T.x, T.y, type = "h", lwd = 3, col = 2)
    abline(h = 0)
    if (!true) 
        lines(T.x[-1], c1[-1], col = 4, lty = 2, lwd = 2)
    # write to file
    if (!true) 
        lines(T.x[-1], wqm.Uminus(c1[-1]), col = 4, lty = 2, lwd = 2)
    if (any(c1[-1]/2 > 0)) {
        acf.matrix <- cbind(T.x[-1], T.y[-1], c1[-1]/2, T.y[-1]/(c1[-1]/2))
        dimnames(acf.matrix) <- list(rep("", length(T.x[-1])), c("Lag", T.ylab, "se", "t-ratio"))
    } else {
        acf.matrix <- cbind(T.x[-1], T.y[-1])
        dimnames(acf.matrix) <- list(rep("", length(T.x[-1])), c("Lag", T.ylab))
    }
    if (print.table) {
        cat(paste(T.main, "\n"))
        print(acf.matrix)
    }
    invisible()
}
