is.stationary <- function(coefficients) {
    # check roots of a model-defining polynomial to see if they are outside of the unit circle
    for (i in length(coefficients):1) {
        # check for leading 0 in the polynomial and remove
        if (coefficients[i] == 0) {
            coefficients[i] <- NA
            break
        }
    }
    coefficients <- coefficients[!is.na(coefficients)]
    if (length(coefficients) == 0) 
        return(TRUE)
    # find the roots and check their magnitude
    root <- polyroot(c(1, -coefficients))
    real.root <- Re(root)
    im.root <- Im(root)
    distance <- sqrt(real.root^2 + im.root^2)
    if (any(distance <= 1.00001)) 
        return(FALSE) else return(TRUE)
}
