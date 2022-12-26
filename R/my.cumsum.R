#' @export
my.cumsum <-
structure(function (w, lag = 1, start) 
{
    if (!missing(start)) 
        w <- c(start, w)
    else w <- c(w)
    z <- rep(0, length(w))
    z[1:(lag + 1)] <- w[1:(lag + 1)]
    for (t in (lag + 1):length(z)) {
        z[t] <- z[t - lag] + w[t]
    }
    return(z)
}, source = c("function(w, lag = 1.,start)", "{", "if(!missing(start))", 
"w <- c(start,w)", "else", "\tw <- c(w)", "\tz <- rep(0., length(w))", 
"\tz[1.:(lag + 1.)] <- w[1.:(lag + 1.)]", "\tfor(t in (lag + 1.):length(z)) {", 
"\t\tz[t] <- z[t - lag] + w[t]", "\t}", "\treturn(z)", "}"))
