fix.arma.lab <- function(the.lab) {
    
    the.prefix <- substring(the.lab, 1, 2)
    the.rest <- substring(the.lab, 3, nchar(the.lab))
    switch(the.prefix, ar = {
        the.lab <- paste("phi", the.rest, sep = "_")
    }, ma = {
        the.lab <- paste("theta", the.rest, sep = "_")
    }, sa = {
        the.rest <- substring(the.lab, 4, nchar(the.lab))
        the.lab <- paste("Phi", the.rest, sep = "_")
    }, sm = {
        the.rest <- substring(the.lab, 4, nchar(the.lab))
        the.lab <- paste("Theta", the.rest, sep = "_")
    }, {
        warning(paste(the.lab, "does not have a recognized parameter prefix\n"))
    })
    
    the.lab
}
