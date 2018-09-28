strip.na <- structure(function(x, xcheck = as.factor(x)) {
    if (all(is.na(xcheck))) 
        return(NULL)
    bad.stuff <- is.na(xcheck) | xcheck == -Inf | xcheck == "NaN"
    return(x[!bad.stuff])
}, source = c("function(x, xcheck = as.factor(x))", "{", "        #as.factor used because otherwise, the is.na does not handle char NA", 
    "        if(all(is.na(xcheck))) return(NULL)", "        bad.stuff <- is.na(xcheck) | xcheck ==  - Inf | xcheck == \"NaN\"", 
    "        return(x[!bad.stuff])", "}"))
