is.diff <- function(model) {
    if (model$local["d"] > 0) 
        return(TRUE)
    seasonal.part <- model$seasonal
    if (is.list(seasonal.part)) {
        if (seasonal.part$order["D"] > 0) 
            return(TRUE)
    } else {
        if (seasonal.part["D"] > 0) 
            return(TRUE)
    }
    return(FALSE)
}
