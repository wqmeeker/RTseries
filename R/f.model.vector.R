f.model.vector <- function(arima.mle.out) {
    # modified 19 march 2016 to allow fixing parameters
    param <- arima.mle.out$coef
    label <- names(arima.mle.out$coef)
    # multiply MA estimates by minus 1 to get to the standard parameterization
    if (length(label) > 0) {
        for (i in 1:length(param)) {
            prefix <- substring(label[i], 1, 2)
            if (prefix == "ma" || prefix == "sm") 
                param[i] = -param[i]
        }
    }
    # opt <- rep(TRUE, length(param))
    se <- rep(0, length(param))
    # se[!opt] <- 0
    t.ratio <- rep(NA, length(param))
    # browser() if (!is.null(arima.mle.out$var.coef)) { se[opt] <- sqrt(diag(arima.mle.out$var.coef)) }
    # t.ratio <- param/se
    fixed <- arima.mle.out$fixed
    kount.opt <- 0
    if (length(label) > 0) {
        for (i in 1:length(param)) {
            if (is.na(fixed[i])) {
                kount.opt <- kount.opt + 1
                se[i] <- sqrt(diag(arima.mle.out$var.coef)[kount.opt])
                t.ratio[i] <- param[i]/se[i]
            }
        }
    }
    esti.matrix <- cbind(param, se, t.ratio, param - 1.96 * se, param + 1.96 * se)
    dimnames(esti.matrix) <- list(label, c("MLE", "se", "t.ratio", "95% lower", "95% upper"))
    number.parameter <- sum(is.na(fixed))
    return(list(esti.matrix = esti.matrix, number.parameter = number.parameter))
}
