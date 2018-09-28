fix.arma.lab.greek <- function(the.lab) {
    
    the.prefix <- substring(the.lab, 1, 2)
    the.rest <- substring(the.lab, 3, nchar(the.lab))
    
    switch(the.prefix, ar = {
        the.lab <- bquote(phi[.(the.rest)])
    }, ma = {
        the.lab <- bquote(theta[.(the.rest)])
    }, sa = {
        the.rest <- substring(the.lab, 4, nchar(the.lab))
        the.lab <- bquote(Phi[.(the.rest)])
    }, sm = {
        the.rest <- substring(the.lab, 4, nchar(the.lab))
        the.lab <- bquote(Theta[.(the.rest)])
    }, {
        warning(paste(the.lab, "does not have a recognized parameter prefix\n"))
    })
    
    the.lab
}
