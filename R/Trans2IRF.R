Trans2IRF <- function(denominator = numeric(), numerator = numeric(), delay = 0, lag.max = 20) {
    # get the impulse-response weights for a given transfer function model
    s <- length(numerator)
    r <- length(denominator)
    # initialize with zeros
    nu <- rep(0, delay + lag.max + 1)
    # j runs from 0
    for (j in 0:(lag.max + delay)) {
        # get the initial value from the numerator term
        if (j == delay) 
            nu[j + 1] <- numerator[1]
        if (j > delay && j < delay + s) 
            nu[j + 1] <- -numerator[j - delay + 1]
        if (j >= delay + s) 
            nu[j + 1] <- 0
        # factor in the effect for the denominator
        if (r > 0) {
            for (i in 1:r) {
                if (j - i >= 1) 
                  nu.previous <- nu[j - i + 1] else nu.previous <- 0
                nuprev <- nu[j + 1]
                nu[j + 1] <- nu[j + 1] + denominator[i] * nu.previous
            }
        }
    }
    nu
}
