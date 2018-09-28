print.arima <- function(arima.out, interactive = F) {
    model.string <- f.model.string(arima.out$wqm.model)
    f.model.vector.out <- f.model.vector(arima.out)
    cat("ARIMA estimation results:\n")
    cat(paste("Series:", arima.out$series.name, "\n"))
    cat(paste(model.string, "\n"))
    cat(paste("-2(Log Likelihood):", format(wqm.Uminus(2 * arima.out$loglik)), "\n"))
    cat(paste("AICc:", format(arima.out$aic), "\n"))
    cat(paste("S:", format(sqrt(arima.out$sigma2)), "\n"))
    cat("\nParameter Estimation Results \n")
    print(f.model.vector.out$esti.matrix)
    if (!is.null(arima.out$reg.coef)) {
        const.term <- arima.out$reg.coef
        cat(paste("\nConstant term:", format(const.term)))
        const.se <- sqrt(arima.out$sigma2/arima.out$n.used)
        cat(paste("\nStandard error:", format(const.se)))
        cat(paste("\nt-ratio:", format(const.term/const.se), "\n"))
    }
    if (!length(arima.out$var.coef) == 0) {
        eigen.vcv <- eigen(arima.out$var.coef)
        if (any(eigen.vcv$values <= 0)) {
            warning("Nonpositive eigenvalues in covariance matrix")
            arima.out$var.coef <- NULL
        }
        if (!length(arima.out$var.coef) == 0) {
            cat("\nVariance-Covariance matrix\n")
            print(arima.out$var.coef)
            cat("\nCorrelation matrix\n")
            print(ccor(arima.out$var.coef))
        }
    }
    invisible(f.model.vector.out)
}
