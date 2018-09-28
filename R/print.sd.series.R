print.sd.series <- function(y.trans) {
    cat(paste("Standard deviation of the working series=", format(sqrt(var((y.trans))))), "\n")
}
