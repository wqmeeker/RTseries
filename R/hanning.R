"hanning" <- function(y) {
    se <- length(y)
    return(c(NA, 0.25 * y[-c(1, 2)] + 0.5 * y[-c(1, se)] + 0.25 * y[-c(se - 1, se)], NA))
}

