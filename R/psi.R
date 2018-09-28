psi <- function(model, number.psi = 38, debug = F) {
    ar <- model$ar
    ma <- model$ma
    my.psi <- rep(1, length = number.psi + 1)
    for (j in 1:number.psi) {
        my.psi[j + 1] <- ifelse(!is.null(ar), sum(ar[1:min(j, length(ar))]), 0) * my.psi[j] - ifelse(j <= 
            length(ma), ma[j], 0)
        if (debug) 
            browser()
    }
    return(my.psi)
}
